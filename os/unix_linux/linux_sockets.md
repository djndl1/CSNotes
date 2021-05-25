# Overview

A method of IPC that allow data to be exchanged between applications either on the same host or on different hosts connected by network.

A socket has a _domain_, which determines

1. the method of identifying a socket

2. the range of communication

Common domains include

1. the UNIX (`AF_UNIX`) domain: communication between applications on the same host

2. the IPv4 (`AF_INET`) domain

3. the IPv6 (`AF_INET6`) domain

Every socket implementation provides at least

1. _stream_ `SOCK_STREAM` (connection-oriented, in peers): reliable (the transmitted data will arrive and in the exact order, otherwise a failure is notified), bidirectional, byte-stream (no concept of message boundaries).

2. _Datagram_ `SOCK_DGRAM`: does not need to be connected to another socket in order to be used.

## Creating a Socket

`socket()`: mainly communication domain and type (_stream_, _datagram_ etc.)

## Binding a Socket to an Address

`bind()`: since the address structure depends on the socket domain, the length of structure must be specified for this generic interface.

### Address Structures

Generic structure as a template for all of the domain-specific address structure

```c
struct sockaddr {
    sa_family_t sa_family;
    char        sa_data[14];
}
```

1. UNIX domain: pathnames

2. Internet domain: IP address + a port number

## Stream Sockets (Telephone Call)

The server (_passive socket_) `listen`s  The client (_active socket_) proactively `connect`s and then the server `accept`s the connection. `read`, `write`, `send`, `recv` are performed before `close()`.

`accept()` creates a new socket for communication while the listening socket remains unaffected.

Connections are uniquely identified by the OS by the following 5-tuple: (local-IP, local-port, remote-IP, remote-port, protocol)

[https://stackoverflow.com/questions/29331938/what-things-are-exactly-happening-when-server-socket-accept-client-sockets](What things are exactly happing when server socket accept client sockets)

[https://stackoverflow.com/questions/11129212/tcp-can-two-different-sockets-share-a-port](TCP can Two different sockets share a port)

For the client, to reattempt the connection, the portable method is to close the socket and create a new socket.

## Datagram Sockets (Mailbox)

No `listen` and `accept`, just `recvfrom` and `sendto`.

`connect` can be used with datagram socket so that the kernel records a particular address as the socket's peer, just to avoid specifying the peer's address every time data are sent.
