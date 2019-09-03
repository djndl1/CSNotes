# Signals

Signals are software interrupts. Signals provide a way of handling asynchronous events.

Read `man 7 signal`.

## Concepts

Every signal has a name beginning with `SIG`. Signal names are defined by positive integer constants. Terminal, hardware exceptions, `kill` function, `kill` command or software conditions can generate signals.

We can tell the kernel to set the _disposition_ of the signal.

- ignore the signal: `SIGKILL` and `SIGSTOP` cannot be ignored

- catch the signal: call a function when the signal occurs

- let the default action apply: every signal has a default action.

## `signal()`

The semantics of `signal` differ among implementations, we must use `sigaction` instead.

When a program is executed, the status of all signal is either default or ignore. Normally, all signals are set to their default action unless the processthat calls `exec` is ignoring the signal. When a process calls `fork`, the child inherits the parent's signal dispositions.

## Unreliable Signals

TODO

## Reentrant Functions

The SUS specifies the functions that are guaranteed to be safe to call from within a signal handler. These functions are reentrant and are called _async-signal safe_. They block any signals during operation if delivery of a signal might causes inconsistencies.

As a general rule, when calling the reentrant async-signal safe functions from a signal handler, we should save and restore `errno`.

## Reliable Signal 

A signal is _generated_ for a process (or sent to a process) when the event that causes the signal occurs. It is _delivered_ to a process when the action for a signal is taken. Between signal generation and delivery, the signal is said to be _pending_. A process has the option of _blocking_ the delivery of a signal. If a blocked signal's action is not ignore, it's pending until the process either unblocks the signal or changes the action to ignore the signal. `sigpending` determines which signals are blocked and pending. If the system delivers the signal more than once, the signals are said to be queued.

Each process has _signal mask_ that defines the set of signals currently blocked from delivery to that process.

## Sending a Signal

`kill()` sends a signal to a process or a group of processes. `raise` allows a process to send a signal to itself. `alarm` sets a timer and generates a `SIGALRM` after the timeout. The default disposition is to terminate the program. Most processes that use an alarm clock catch this signal. `pause` suspends the calling process until a signal is caught.

```c

#include <setjmp.h>
#include <signal.h>
#include <unistd.h>

static jmp_buf env_alrm;

static void sig_alrm(int signo)
{
        longjmp(env_alrm, 1);
}

unsigned int sleep2(unsigned int seconds)
{
        if (signal(SIGALRM, sig_alrm) == SIG_ERR)
                return seconds;
        if (setjmp(env_alrm) == 0) {
                alarm(seconds);
                pause();
        }

        return alarm(0);
}
// this implementation of sleep will interrupt other signal handlers
```

A common use for `alarm` is to put an upper time limit on operations that can block.

```c 
#include <stdio.h>
#include <setjmp.h>
#include <signal.h>
#include <unistd.h>

#define MAXLINE 4096

static jmp_buf env_alrm;

static void sig_alrm()
{
        longjmp(env_alrm, 1);
}

int main(int argc, char *argv[])
{
        int     n;
        char    line[MAXLINE];

        if (signal(SIGALRM, sig_alrm) == SIG_ERR)
                fprintf(stderr, "signal(SIGALRM) error");
        if (setjmp(env_alrm) != 0) {
                fprintf(stderr, "read timeout");
                return 1;
        }

        alarm(10);
        if ((n = read(STDIN_FILENO, line, MAXLINE)) < 0) {
                fprintf(stdout, "read error");
                return 2;
        }
        alarm(0);

        write(STDOUT_FILENO, line, n);
        return 0;
}
```

## Signal Sets

Signals can be grouped into a set `sigset_t` so that the kernel can mask a group of them.

```c
       int sigemptyset(sigset_t *set);

       int sigfillset(sigset_t *set);

       int sigaddset(sigset_t *set, int signum);

       int sigdelset(sigset_t *set, int signum);

       int sigismember(const sigset_t *set, int signum);
```

A process can examine its signal mask, change its signal mask, or perform both operations in one step by calling `sigprocmask()`. `sigpending` returns the set of signals that are blocked from delivery and currently pending for the calling process. We can use signal mask to protect critical regions of code that we don't want interrupted by a signal.

## `sigaction`

`sigaction` supersedes `signal` and what's more:

```c
void (*signal(int sig, void (*func)(int)))(int)
{
	struct sigaction sa_old, sa = { .sa_handler = func, .sa_flags = SA_RESTART };
	if (__sigaction(sig, &sa, &sa_old) < 0)
		return SIG_ERR;
	return sa_old.sa_handler;
}
```

## `sigsetjmp` and `siglongjmp`

When a signal is caught, the signal-catching function is entered, with the current signal automatically being added to the signal mask of the process. To allow to choose non-local goto save and restore signal mask, POSIX defines `sigsetjmp` and `siglongjmp`. They should always be used when branching from a signal handler.

```c
       int sigsetjmp(sigjmp_buf env, int savesigs);
       void siglongjmp(sigjmp_buf env, int val);
```

## `sigsuspend`

After we unblock a few signals, the pending signals  may immediately be delivered. `sigsuspend()` restores  the signal mask and put the process to sleep in a single atomic operation. The process is suspended until a signal is caught or until a signal occurs that terminates the process. If a signal is caught and if the signal handler returns, then `sigsuspend` returns, and the signal mask of the process is set to its value before the call to `sigsuspend`.

TODO 

## `abort`

`abort()` sends the `SGIABRT` to the caller. The intent of letting the process catch the `SIGABRT` is to allow it to perform any cleanup that it wants to do before the process terminates.  If the process doesn’t terminate itself from this signal handler, POSIX.1 states that, when the signal handler returns, abort terminates the process.

Catching the signal is intended to provide the application developer with a portable means  to abort processing, free from possible interference from any implementation-supplied functions.

```c
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void my_abort(void)
{
        sigset_t            mask;
        struct sigaction    action;

        // restore SIGABRT disposition
        sigaction(SIGABRT, NULL, &action);
        if (action.sa_handler == SIG_IGN) {
                action.sa_handler = SIG_DFL;
                sigaction(SIGABRT, &action, NULL);
        }
        if (action.sa_handler == SIG_DFL)
                fflush(NULL);           // flush all open stdio streams

        // ensure that SIGABRT is not ignored
        sigfillset(&mask);
        sigdelset(&mask, SIGABRT);
        sigprocmask(SIG_SETMASK, &mask, NULL);
        kill(getpid(), SIGABRT); // may not be default action

        fflush(NULL);           // the signal handler may generate some output
        action.sa_handler = SIG_DFL;
        sigaction(SIGABRT, &action, NULL);
        sigprocmask(SIG_SETMASK, &mask, NULL); // just in case
        kill(getpid(), SIGABRT);               // one more time
        exit(1);
}
```

TODO

# Pipes

Pipes are the oldest form of UNIX System IPC and are provided by all UNIX systems. Half-duplex pipes are the most commonly used form of IPC. Every time a shell pipeline is used, the shell create a separate process for each command and links the standard output of one process to the standard input of the next using a pipe.

Pipes are on many systems half-duplex. Pipes can be used only between processes that have a common ancestor.

```c
int pipe(int pipefd[2]); // CREATES a pipe, which has two ends, represented by two file descriptors.
```

A pipe in a single process is next to useless. Normally, the process that calls pipe then calls fork, creating an IPC channel from the parent to the child, or vice versa. For a pipe from the parent to the child, the parent closes the read end of the pipe and the child closes the write end. For a pipe from the child to the parent, the parent closes `fd[1]`, and the child closes `fd[0]`.

```c

int
main(void)
{
	int		n;
	int		fd[2];
	pid_t	pid;
	char	line[MAXLINE];

	if (pipe(fd) < 0)
		err_sys("pipe error");
	if ((pid = fork()) < 0) {
		err_sys("fork error");
	} else if (pid > 0) {		/* parent */
		close(fd[0]);
		write(fd[1], "hello world\n", 12);
	} else {					/* child */
		close(fd[1]);
		n = read(fd[0], line, MAXLINE);
		write(STDOUT_FILENO, line, n);
	}
	exit(0);
}
```

To make a pipeline, we create a pipe, fork a child, duplicate the file descriptor of the read end of the child onto the the standard input

```c
#define	DEF_PAGER	"/bin/more"		/* default pager program */

int
main(int argc, char *argv[])
{
	int		n;
	int		fd[2];
	pid_t	pid;
	char	*pager, *argv0;
	char	line[MAXLINE];
	FILE	*fp;

	if (argc != 2)
		err_quit("usage: a.out <pathname>");

	if ((fp = fopen(argv[1], "r")) == NULL)
		err_sys("can't open %s", argv[1]);
	if (pipe(fd) < 0)
		err_sys("pipe error");

	if ((pid = fork()) < 0) {
		err_sys("fork error");
	} else if (pid > 0) {								/* parent */
		close(fd[0]);		/* close read end */

		/* parent copies argv[1] to pipe */
		while (fgets(line, MAXLINE, fp) != NULL) {
			n = strlen(line);
			if (write(fd[1], line, n) != n)
				err_sys("write error to pipe");
		}
		if (ferror(fp))
			err_sys("fgets error");

		close(fd[1]);	/* close write end of pipe for reader */

		if (waitpid(pid, NULL, 0) < 0)
			err_sys("waitpid error");
		exit(0);
	} else {										/* child */
		close(fd[1]);	/* close write end */
		if (fd[0] != STDIN_FILENO) {
			if (dup2(fd[0], STDIN_FILENO) != STDIN_FILENO)
				err_sys("dup2 error to stdin");
			close(fd[0]);	/* don't need this after dup2 */
		}

		/* get arguments for execl() */
		if ((pager = getenv("PAGER")) == NULL)
			pager = DEF_PAGER;
		if ((argv0 = strrchr(pager, '/')) != NULL)
			argv0++;		/* step past rightmost slash */
		else
			argv0 = pager;	/* no slash in pager */

		if (execlp(pager, argv0, (char *)0) < 0)
			err_sys("execl error for %s", pager);
	}
	exit(0);
}
```

## `popen`, `pclose`

Since a common operation is to create a pipe to another process to either read its output or send it input, the standard I/O library has historically provided the `popen` and `pclose` functions. These two functions handle all the dirty work: creating a pipe, forking a child, closing the unused ends of the pipe, executing a shell to run the command, and waiting for the command to terminate.

```c
#include <stdlib.h>
#include <sys/wait.h>
#include <stdio.h>

#define PAGER "${PAGER:-most}"
#define MAXLINE 4096


int main(int argc, char *argv[])
{
        char    line[MAXLINE];
        FILE    *fpin, *fpout;

        if (argc != 2) {
                fprintf(stderr, "usage: a.out <pathname>\n");
                return 1;
        }

        if ((fpin = fopen(argv[1], "r")) == NULL) {
                fprintf(stderr ,"cannot open %s\n", argv[1]);
                exit(2);
        }
        if ((fpout = popen(PAGER, "w")) == NULL) {
               perror("popen error\n");
                exit(3);
        }

        while (fgets(line, MAXLINE, fpin) != NULL) {
                if (fputs(line, fpout) == EOF) {
                        perror("fputs error to pipe\n");
                        exit(4);
                }
        }
        if (pclose(fpout) == -1)
                exit(5);
        return 0;
}
```

`popen` and `pclose` implementation

```c
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

#include <stdlib.h>
#include <stdio.h>

static pid_t *childpid = NULL;
static int maxfd;

#include <limits.h>

#ifdef	OPEN_MAX
static long	openmax = OPEN_MAX;
#else
static long	openmax = 0;
#endif

/*
 * If OPEN_MAX is indeterminate, this might be inadequate.
 */
#define	OPEN_MAX_GUESS	256

static long open_max(void)
{
	if (openmax == 0) {		/* first time through */
		errno = 0;
		if ((openmax = sysconf(_SC_OPEN_MAX)) < 0) {
			if (errno == 0)
				openmax = OPEN_MAX_GUESS;	/* it's indeterminate */
			else {
                                fprintf(stderr, "Cannot obtain OPEN_MAX");
                                exit(1);
                        }
		}
	}
	return(openmax);
}


FILE *popen(const char* cmdstring, const char* type)
{
        int     i;
        int     pfd[2];
        pid_t   pid;
        FILE    *fp;

        // only allow "r" or "w"
        if ((type[0] != 'r' && type[0] != 'w') || type[1] != '\0') {
                errno = EINVAL;
                return NULL;
        }

        if (childpid == NULL) {
                maxfd = open_max();
                if ((childpid = calloc(maxfd, sizeof(pid_t))) == NULL)
                        return NULL;
        }

        if (pipe(pfd) < 0)
                return NULL;
        if (pfd[0] >= maxfd || pdf[1] >= maxfd) {
                close(pfd[0]);
                close(pfd[1]);
                errno = EMFILE;
                return NULL;
        }

        if ((pid = fork()) < 0)
                return NULL;
        else if (pid == 0) { // child process
                if (*type == 'r') { // child writes
                        close(pfd[0]);
                        if (pfd[1] != STDOUT_FILENO) {
                                dup2(pfd[1], STDOUT_FILENO);
                                close(pfd[1]);
                        }
                } else {
                        close(pfd[1]);
                        if (pfd[0] != STDIN_FILENO) {
                                dup2(pfd[0], STDIN_FILENO);
                                close(pfd[0]);
                        }
                }
                /* close all descriptors in childpid[] */
                for (i = 0; i < maxfd; i++)
                        if (childpid[i] > 0)
                                close(i);

                execl("/bin/sh", "sh", "-c", cmdstring, (char *)0);
                _exit(127);
        }

        /* parent continues */
        if (*type == 'r') {
                close(pfd[1]);
                if ((fp = fdopen(pfd[0], type)) == NULL)
                        return NULL;
        } else {
                close(pfd[0]);
                if ((fp = fdopen(pfd[1], type)) == NULL)
                        return NULL;
        }
        childpid[fileno(fp)] = pid;
        return  fp;
}
```


# Coprocesses

TODO


# FIFO (named pipes)

Unnamed pipes can be used only between related processes when a common ancestor has created the pipe. With FIFOs, unrelated processes can exchange data. 

Creating a FIFO is similar to creating a file. `mkfifo` and `mkfifoat` create a FIFO. Once a FIFO is created,  `open`, `close`, `read`, `write`, `unlink` all work with FIFOs. As with a pipe, if we write to a FIFO that no process has open for reading, the signal `SIGPIPE` is generated. When the last writer for a FIFO closes the FIFO, an end of file is generated for the reader of the FIFO.

```bash
mkfifo fifo1
prog3 < fifo1 & prog1 < infile | tee fifo1 | prog2
```

A use for FIFOs is to send data between a client and a server. If we have a server that is contacted by numerous clients, each client can write its request to a well-known FIFO that the server creates.  The constant `PIPE_BUF` specifies the maximum amount of data that can be written atomically to a FIFO. Since there are multiple writers for the FIFO, the requests sent by the clients to the server need to be less than `PIPE_BUF` bytes in size. This prevents any interleaving of the client writes.  The
server then creates a unique FIFO for each client, using a pathname based on the client’s process ID to send back responses.

# XSI IPC: message queue, semaphores and shared memory

Each IPC structure (message queue, semaphore, or shared memory segment) in the kernel is referred to by a non-negative integer identifier. The identifier is an internal name for an IPC object. An IPC object is associated with a key that acts as an external name. Whenever an IPC structure is being created, a key must be specified.

TODO

## Message Queues

A message queue is a linked list of messages stored within the kernel and identified by a message queue identifier. 

A new queue is created or an existing queue opened by `msgget`. New messages are added to the end of a queue by `msgsnd`. The `msgctl` (`ioctl`-like functions for XSI IPC) function performs various operations on a queue. Messages are retrieved from a queue by `msgrcv`.

Use: If we need a bidirectional flow of data between a client and a server, we can use either message queues or full-duplex pipes.

TODO

## Semaphores

A semaphore is a counter used to provide access to a shared data object for multiple processes.

To obtain a shared resource, a process:

1. Test the semaphore that controls the resource.

2. Use the resource if the value of the semaphore is positive. The semaphore is decremented by 1.

3. If the value of the semaphore is 0, the process goes to sleep unitl the semaphore value is greater than 0.

To implement semaphores correctly, the test of a semaphore’s value and the decrementing of this value must be an atomic operation.

A common form of semaphore is called a binary semaphore. It controls a single resource, and its value is initialized to 1. In general, however, a semaphore can be initialized to any positive value, with the value indicating how many units of the shared resource are available for sharing.


TODO

## Shared Memory

# POSIX Semaphore

The POSIX semaphore mechanism is one of three IPC mechanisms that originated with the real-time extensions to POSIX.1. POSIX semaphores are available in two flavors: named and unnamed. Unamed semaphores exist in memory only and require that processes have access to the memory to be able to use the semaphores. This means they can be used only by threads in the same process or threads in different processes that have mapped the same memory extent into their address spaces. Named semaphores, in contrast, are accessed by name and can be used by threads in any processes that know their names.

`sem_open` creates a new named semaphore. `sem_close` release any resources associated with the semaphore. If our process exits without having first called sem_close, the kernel will close any open semaphores automatically. To destroy a named semaphore, we can use the `sem_unlink` function.The sem_unlink function removes the name of the semaphore. If there are no open references to the semaphore, then it is destroyed. Otherwise, destruction is deferred until the last open reference is closed. To decrement the value of a semaphore, we can use the `sem_wait`, `sem_trywait`, `sem_timedwait` function. To increment the value of a semaphore, we call the `sem_post` function.

When we want to use POSIX semaphores within a single process, it is easier to use
unnamed semaphores. `sem_init` creates an unnamed semaphore. When we are done using the unnamed semaphore, we can discard it by calling the `sem_destroy` function. `sem_getval` retrieves the value of a semaphore. Be aware that the value of the semaphore can change by the time that we try to use the value just read. Unless we use additional synchronization mechanisms to avoid this race, the sem_getvalue function is useful only for debugging.

One of the motivations for introducing the POSIX semaphore interfaces was that they
can be made to perform significantly better than the existing XSI semaphore interfaces.

```c
#include <fcntl.h>
#include <semaphore.h>
#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>

struct slock {
        sem_t *semp;
        char name[_PC_NAME_MAX];
};

typedef struct slock slock_t;

void s_free(slock_t *sp)
{
        sem_close(sp->semp);
        free(sp);
}

int s_lock(slock_t *sp)
{
        return sem_wait(sp->semp);
}

int s_trylock(slock_t *sp)
{
        return sem_trywait(sp->semp);
}

int s_unlock(slock_t *sp)
{
        return sem_post(sp->semp);
}

slock_t *s_alloc()
{
        struct slock *sp;
        static int cnt;
        if ((sp = malloc(sizeof(struct slock))) == NULL)
                return NULL;
        do {
                snprintf(sp->name, sizeof(sp->name), "/%ld.%d", (long)getpid(),
                         cnt++);
                sp->semp = sem_open(sp->name, O_CREAT|O_EXCL, S_IRWXU, 1);
        } while ((sp->semp == SEM_FAILED) && (errno == EEXIST));
        if (sp->semp == SEM_FAILED) {
                free(sp);
                return(NULL);
        }
        sem_unlink(sp->name); // this does not destroy the semaphore immdiately
        return(sp);
}

```
