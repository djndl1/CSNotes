The socket network IPC interface can be used by processes to communicate with other processes, regardless of where they are running — on the same machine or on different machines. The socket interface can be used to communicate using many different network protocols

# Socket Descriptors

A socket is an abstraction of a communication endpoint. Applications use socket descriptors to access sockets. Socket descriptors are implemented as file descriptorss in Unixes.

The `socket` function creates a socket. Calling `socket` is similar to calling `open`. Communication on a socket is bidirectional. `shutdown()` disable I/O on a socket. It should be used over `close` because it deactivates a socket independently of the number of active file descriptors referencing it.

# Addressing

`man 7 ip`


The machine’s network address helps us identify the computer on the network we wish to contact, and the service, represented by a port number, helps us identify the particular process on the computer. 

The TCP/IP protocol suite uses big-endian byte order. Applications sometimes need to translate them between the processor ’s byte order and the network byte order. 

Four functions are provided to convert between the processor byte order and the network byte order for TCP/IP applications:

```c
 uint32_t htonl(uint32_t hostlong);

 uint16_t htons(uint16_t hostshort);

 uint32_t ntohl(uint32_t netlong);

 uint16_t ntohs(uint16_t netshort);
```

An address identifies a socket endpoint in a particular communication domain. The address format is specific to the particular domain.

```c
           struct sockaddr_in {
               sa_family_t    sin_family; /* address family: AF_INET */
               in_port_t      sin_port;   /* port in network byte order */
               struct in_addr sin_addr;   /* internet address */
           };

           /* Internet address. */
           struct in_addr {
               uint32_t       s_addr;     /* address in network byte order */
           };
```

```c
           struct sockaddr_in6 {
               sa_family_t     sin6_family;   /* AF_INET6 */
               in_port_t       sin6_port;     /* port number */
               uint32_t        sin6_flowinfo; /* IPv6 flow information */
               struct in6_addr sin6_addr;     /* IPv6 address */
               uint32_t        sin6_scope_id; /* Scope ID (new in 2.4) */
           };

           struct in6_addr {
               unsigned char   s6_addr[16];   /* IPv6 address */
           };
```

`inet_addr` and `inet_ntoa` functions convert between the binary address format and a string in dotted-decimal functions. `inet_ntop` and `inet_pton` support similar functionality and work with both IPv4 and IPv6 addresses.

```c
const char *inet_ntop(int af, const void *src,
                             char *dst, socklen_t size);
int inet_pton(int af, const char *src, void *dst);
```

# Network Configuration and Data Files

Historically, BSD networking software has provided interfaces to access the various network configuration information.

The hosts known by a given computer system are found by calling `gethostent()`. It returns the next entry in the file (`/etc/hosts`).

We can get network names and numbers with a similar set of interfaces (`/etc/networks`).

```c
       struct netent *getnetent(void);
       struct netent *getnetbyname(const char *name);
       struct netent *getnetbyaddr(uint32_t net, int type);
       void setnetent(int stayopen);
       void endnetent(void);
```

We can map between protocol names and numbers:

```c
       struct protoent *getprotoent(void);
       struct protoent *getprotobyname(const char *name);
       struct protoent *getprotobynumber(int proto);
       void setprotoent(int stayopen);
       void endprotoent(void);
```

Services are represented by the port number portion of the address. Each service is offered on a unique, well-known port number. We can map a service name to a port.

```c
       struct servent *getservent(void);
       struct servent *getservbyname(const char *name, const char *proto);
       struct servent *getservbyport(int port, const char *proto);
       void setservent(int stayopen);
       void endservent(void);
```

`getaddrinfo` allows us to map a host name and a service name to an address.

```c
       int getaddrinfo(const char *node, const char *service,
                       const struct addrinfo *hints,
                       struct addrinfo **res);

       void freeaddrinfo(struct addrinfo *res);
```

TODO

# Associating Addresses with Sockets

For a server, we need to associate a well-known address with the server's socket on which client requests will arrive.

`bind` function associates an address with a socket. For the Internet domain, if we specify the special IP address `INADDR_ANY` (defined in the socket endpoint will be bound to all the system’s network interfaces. `getsockname` discover the address bound to a socket. If the socket is connected to a peer, the peer's address  can be obtained by calling `getpeername`.




# Connection Establishment 

When dealing with a connection-oriented network service, we need to create a connection between the sockets before we can exchange data. `connect()` creates a connection.

```c
#include <unistd.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <sys/types.h>

#define MAXSLEEP 128

// exponential backoff
int connect_try(int domain, int type, int protocol,
                const struct sockaddr *addr, socklen_t alen)
{
    int numsec, fd;

    for (numsec = 1; numsec < MAXSLEEP; numsec <<= 1) {
        if ((fd = socket(domain, type, protocol)) < 0)
            return -1;
        if (connect(fd, addr, alen) == 0)
            return fd;
        close(sockfd);
        if (numsec <= MAXSLEEP)
            sleep(numsec);
    }
    return -1;
}
```

The `connect` function can also be used with a connectionless network service (`SOCK_DGRAM`).

A server announces that it is willing to accept connect requests by calling the `listen` function. It can specify a maximum number of acceptable connections. `accept` retrieves a connect request and  convert it int a connection. 

```c
int initserver(int type, const struct sockaddr *addr, socklen_t alen,
    int qlen)
{
        int fd;
        int err = 0;

        if ((fd = socket(addr->sa_family, type, 0)) < 0)
                return -1;
        if (bind(fd, addr, alen) < 0)
                goto errout;
        if (type == SOCK_STREAM || type == SOCK_SEQPACKET) {
                if (listen(fd, qlen) < 0)
                        goto errout;
        }
        return fd;

errout:
        err = errno;
        close(fd);
        errno = err;
        return -1;
}
```

# Data Transfer

```c
        // already connected
       ssize_t send(int sockfd, const void *buf, size_t len, int flags);
        // can be used with connectionless sockets, otherwise, the dest_addr is ignored
       ssize_t sendto(int sockfd, const void *buf, size_t len, int flags,
                      const struct sockaddr *dest_addr, socklen_t addrlen);
       // 
       ssize_t sendmsg(int sockfd, const struct msghdr *msg, int flags);
       
       struct msghdr {
               void         *msg_name;       /* Optional address */
               socklen_t     msg_namelen;    /* Size of address */
               struct iovec *msg_iov;        /* Scatter/gather array */
               size_t        msg_iovlen;     /* # elements in msg_iov */
               void         *msg_control;    /* Ancillary data, see below */
               size_t        msg_controllen; /* Ancillary data buffer len */
               int           msg_flags;      /* Flags (unused) */
       };
```

```c
       ssize_t recv(int sockfd, void *buf, size_t len, int flags);

        // can obtain the source address from which the data was sent
       ssize_t recvfrom(int sockfd, void *buf, size_t len, int flags,
                        struct sockaddr *src_addr, socklen_t *addrlen);

       ssize_t recvmsg(int sockfd, struct msghdr *msg, int flags);
```


```c
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <errno.h>
#include <netdb.h>

#define BUFLEN  128

extern int connect_retry(int, int, int,
                         const struct sockaddr *, socklen_t);

void print_uptime(int sockfd)
{
        int     n;
        char    buf[BUFLEN];

        // bytestream, keep receiving and writing until no more bytes available
        while ((n = recv(sockfd, buf, BUFLEN, 0)) > 0) {
                write(STDOUT_FILENO, buf, BUFLEN);
        }
        if (n < 0)
                fprintf(stdout, "recv error");
}

int main(int argc, char *argv[])
{
        struct addrinfo *ailistl, *aip;
        struct addrinfo hint;
        int             sockfd, err;

        if (argc != 2) {
                fprintf(stdout, "usage: ruptime hostname\n");
                exit(1);
        }
        memset(&hint, 0, sizeof(struct addrinfo));
        hint.ai_socktype = SOCK_STREAM;
        hint.ai_canonname = NULL;
        hint.ai_addr = NULL;
        hint.ai_next = NULL;
        if ((err = getaddrinfo(argv[1], "ruptime", &hint, &ailistl)) != 0) {
                fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(err));
                exit(1);
        }
        for (aip = ailistl; aip != NULL; aip = aip->ai_next) {
                if ((sockfd = connect_retry(aip->ai_family, SOCK_STREAM, 0,
                                            aip->ai_addr, aip->ai_addrlen)) < 0)
                        err = errno;
                else {
                        print_uptime(sockfd);
                        exit(0);
                }
        }
        fprintf(stderr, "cannot connect to %s\n", argv[1]);
        exit(2);
}
```

```c
#include <unistd.h>
#include <string.h>
#include <netdb.h>
#include <errno.h>
#include <syslog.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/stat.h>

#define BUFLEN 128
#define QLEN 10

#ifndef HOST_NAME_MAX
#define HOST_NAME_MAX   256
#endif

extern int initserver(int, const struct sockaddr *, socklen_t, int);

int
set_cloexec(int fd)
{
	int		val;

	if ((val = fcntl(fd, F_GETFD, 0)) < 0)
		return(-1);
        
	val |= FD_CLOEXEC;		/* enable close-on-exec */

	return(fcntl(fd, F_SETFD, val));
}

void
daemonize(const char *cmd)
{
	int i, fd0, fd1, fd2;
	pid_t pid;
	struct rlimit rl;
	struct sigaction sa;

	/*
	 * Clear file creation mask.
	 */
	umask(0);

	/*
	 * Get maximum number of file descriptors.
	 */
	if (getrlimit(RLIMIT_NOFILE, &rl) < 0) {
                fprintf(stderr, "%s: cannot get file limit\n", cmd);
                exit(1);
        }

	/*
	 * Become a session leader to lose controlling TTY.
	 */
	if ((pid = fork()) < 0) {
                fprintf(stderr, "%s: cannot fork\n", cmd);
                exit(2);
        }
	else if (pid != 0) /* parent */
		exit(0);
	setsid();

	/*
	 * Ensure future opens won't allocate controlling TTYs.
	 */
	sa.sa_handler = SIG_IGN;
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = 0;
	if (sigaction(SIGHUP, &sa, NULL) < 0) {
                fprintf(stderr, "%s: cannot ignore SIGHUP\n", cmd);
                exit(3);
        }
	if ((pid = fork()) < 0) {
                fprintf(stderr, "%s: cannot fork\n", cmd);
                exit(2);
        }
	else if (pid != 0) /* parent */
		exit(0);

	/*
	 * Change the current working directory to the root so
	 * we won't prevent file systems from being unmounted.
	 */
	if (chdir("/") < 0) {
                fprintf(stderr, "%s: cannot change directory\n", cmd);
                exit(3);
        }

	/*
	 * Close all open file descriptors.
	 */
	if (rl.rlim_max == RLIM_INFINITY)
		rl.rlim_max = 1024;
	for (i = 0; i < rl.rlim_max; i++)
		close(i);

	/*
	 * Attach file descriptors 0, 1, and 2 to /dev/null.
	 */
	fd0 = open("/dev/null", O_RDWR);
	fd1 = dup(0);
	fd2 = dup(0);

	/*
	 * Initialize the log file.
	 */
	openlog(cmd, LOG_CONS, LOG_DAEMON);
	if (fd0 != 0 || fd1 != 1 || fd2 != 2) {
		syslog(LOG_ERR, "unexpected file descriptors %d %d %d",
		  fd0, fd1, fd2);
		exit(1);
	}
}


void serve(int sockfd)
{
        int     clfd;
        FILE    *fp;
        char    buf[BUFLEN];

        set_cloexec(sockfd);
        for (;;) {
                if ((clfd = accept(sockfd, NULL, NULL)) < 0) {
                        syslog(LOG_ERR, "ruptimed: accpet error: %s", strerror(errno));
                        exit(1);
                }
                set_cloexec(clfd);
                if ((fp = popen("/usr/bin/uptime", "r")) == NULL) {
                        sprintf(buf, "error: %s\n", strerror(errno));
                        send(clfd, buf, strlen(buf), 0);
                } else {
                        while (fgets(buf, BUFLEN, fp) != NULL)
                                send(clfd, buf, strlen(buf), 0);
                        pclose(fp);
                }
                close(clfd);
        }
}

int main(int argc, char *argv[])
{
        struct addrinfo *ailist, *aip;
        struct addrinfo hint;
        int             sockfd, err, n;
        char            *host;

        if (argc != 1) {
                printf("usage: ruptimed");
                exit(1);
        }
        if ((n = sysconf(_SC_HOST_NAME_MAX)) < 0)
                n = HOST_NAME_MAX; /* best guess */
        if ((host = malloc(n)) == NULL) {
                fprintf(stderr, "malloc error\n");
                exit(2);
        }

        if (gethostname(host, n) < 0) {
                fprintf(stderr, "gethostname error\n");
                exit(3);
        }
        daemonize("ruptimed");
        memset(&hint, 0, sizeof(hint));
        hint.ai_flags = AI_CANONNAME;
        hint.ai_socktype = SOCK_STREAM;
        hint.ai_canonname = NULL;
        hint.ai_addr = NULL;
        hint.ai_next = NULL;
        if ((err = getaddrinfo(host, "ruptime", &hint, &ailist)) != 0) {
          syslog(LOG_ERR, "ruptimed: getaddrinfo error: %s", gai_strerror(err));
          exit(1);
}
        for (aip = ailist; aip != NULL; aip = aip->ai_next) {
                if ((sockfd = initserver(SOCK_STREAM, aip->ai_addr,
                                         aip->ai_addrlen, QLEN)) >= 0) {
                        serve(sockfd);
                        exit(0);
                }
        }
        exit(1);
}
```
