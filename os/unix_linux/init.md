# The init system

e.g. systemd, Runit, OpenRC, launched, 

## SysV-style

Besides usual initialization of system (as the name suggests), init also handles rebooting, shutdown and booting into recovery mode (single-user mode). Upon execution, `inittab` is scanned and some corresponding action is taken.
A running System V is in one of the _runlevels_ (predetermined number of states). A per-runlevel set of scripts are run, which typically mount filesystems, start or stop the X window System, shutdown the machine.

### Runlevels

A mode of operation, which defines the state of machine. The exact setup of these configurations varies between operating systems.

The LSB definition:

- 0: off

- 1: single user mode: does not configure netif or start daemons; for administrative tasks

- 2: multi-user mode: no netif

- 3: multi-user mode with networking: 

- 4: user-defined: 

- 5: runlevel 3 + display manager

- 6: reboot

Most Linux distros default to runlevel 5.

## OpenRC

### Service Management

```shell
rc-service service_name [stop | start | status]

rc-update show -v
rc-update add service_name run_level
rc-update delete service_name

rc-status [service_name]
```

Additional service configuration can reside in `/etc/conf.d`

### Runlevels

Configured in `/etc/runlevels` with symlink to enabled `init.d` files; It is possible to add a custom runlevel just by stacking on another
