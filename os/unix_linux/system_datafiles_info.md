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

