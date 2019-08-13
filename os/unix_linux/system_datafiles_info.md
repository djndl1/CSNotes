# Password File `/etc/passwd`

The UNIX System's password file, the user database, contains the following fields defined in `<pwd.h>`

```c
// POSIX defines five of them
char    *pw_name   // User's login name.
uid_t    pw_uid    // Numerical user ID.
gid_t    pw_gid    // Numerical group ID.
char    *pw_dir    // Initial working directory.
char    *pw_shell  // Program to use as shell.
```

```c
// linux defines as the following
struct passwd {
    char   *pw_name;       /* username */
    char   *pw_passwd;     /* user password */
    uid_t   pw_uid;        /* user ID */
    gid_t   pw_gid;        /* group ID */
    char   *pw_gecos;      /* user information */
    char   *pw_dir;        /* home directory */
    char   *pw_shell;      /* shell program */
};
```

Three additional fields are 

```c
# implemented by FreeBSD and MacOS
char    *pw_class  # user access class
time_t   pw_change # next time to change password
tiem_t   pw_expire # acount expiration time
```

The logging shell field can be set to `/dev/null`/`/bin/false`/`/bin/true` so that no one can log in as this user. Some systems provide the `nologin` command, which prints a customizable error message and exits with a nonzero exit status. The `nobody` user can be used to allow people to log in to a system but with a use ID and group ID that provide no priviledges. The only files that `nobody` can access are those that are readable or writable by the world.

The `finger` command (need manual installation on Debian) supports additional information in the comment field, each of which separated by a comma: the user's name, office location, office phone number and home phone number. The `vipw` command allow administators to edit the password file.

`getpwnam`, `getpwuid` allow us to look up an entry given a user's login name or a numerical user ID. `getpwent`, `setpwent`, `getpwent` go through the entire password file.

## Shadow Passwords

The encrypted password is copy of the user's password that has been put through a one-way encryption algorithm. Given an encrypted password, we can't apply an algorithm that inverts it and returns the plaintext password. A common experiment is for someone to obtain a copy of the password file and try guessing the passwords. To make it more difficult to obtain the encrypted passwords, systems now store the encrypted password in the _shadow password file_.

```c
struct spwd {
    char *sp_namp;     /* Login name */
    char *sp_pwdp;     /* Encrypted password */
    long  sp_lstchg;   /* Date of last change (measured in days
                        since 1970-01-01 00:00:00 +0000 (UTC)) */
    long  sp_min;      /* Min # of days between changes */
    long  sp_max;      /* Max # of days between changes */
    long  sp_warn;     /* # of days before password expires
                            to warn user to change it */
    long  sp_inact;    /* # of days after password expires
    until account is disabled */
    long  sp_expire;   /* Date when account expires
                        (measured in days since
                        1970-01-01 00:00:00 +0000 (UTC)) */
    unsigned long sp_flag;  /* Reserved */
};
```

The shadow password file should not be readable by the world.

A separate set of functions like `getspnam` are available to access the shadow password file.

### Group File `/etc/group`

The group database contains the fields 

```c
struct group {
    char   *gr_name;        /* group name */
    char   *gr_passwd;      /* group password */
    gid_t   gr_gid;         /* group ID */
    char  **gr_mem;         /* NULL-terminated array of pointers
                                to names of group members */
};
```

There is also a set of functions available to access the structure and these fields: `getgrgid`, `getgrnam`, `getgrent`, `setgrent`, `endgrent`.

### Supplementary Group IDs

4.2BSD introduces the concept of supplementary group IDs. It is a feature required by POSIX.1. The advantage of using supplementary group IDs is that changing groups is no longer necessary.

`getgroups`, `setgroups` and `initgroups` are provided to fetch and set the supplementary group IDs.
