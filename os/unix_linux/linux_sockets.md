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

# Unix Sockets


A socket address takes the form of a pathname. The socket is merely an entry in the file system. The underlying device is not affected. The pathname should better be _absolute_, located in a secure directory, removed after using.

The address structure should be zeroed by `memset` to ensure any nonstandard fields are also properly initialized and also the pathname is null-terminated.

## Permissions 

Connecting and writing to a socket requires write permission. By default, a socket is created by `bind()` with all permissions granted to owner, group and other.

## `socketpair()` Only for `AF_UNIX`

`SOCK_STREAM`: bidirectional pipe (stream pipe). Sockets created by `socketpair` are not bound to any address, avoiding a whole class of security vulnerabilities.

## Linux: Abstract Socket Namespace

A namespace for socket other than the file system. An abstract binding starts with a null byte `\0`.

# TCP/IP

Router: a _multihomed host_ whose function is to connect one subnetwork to another.

Networking Protocol: a set of rules defining how information is to be transmitted across a network, generally organized as a series of layers, e.g. TCP/IP. The lower layer make sno attempt to interpret information sent from the upper layer.

Data Link layer: transmits the _frame_ (data payload, dst_addr, frame size, MTU (the upper limit on the size of a frame)) across the physical link and khandles acknowledgement from the receiver.

Network Layer (IP): breaking data into proper fragments for frames (an IP datagram (up to 2^16 bytes) might be too big for a frame). Routing data across the internet and providing services to the transport layer. `SOCK_RAW` is for socket to directly manipulate the network layer. An IP datagram also has an upper size. IP is connectionless (no virtual circuit) and unreliable (best effort, no arrival guarantee, not duplication-free, not ordered, no error recovery).

## IP Addresses

network-id, host-id (may be subnetted)

loopback on `127.0.0.0/8`, `INADDR_LOOPBACK`

wildcard address `0.0.0.0` (in most implementations) `INADDR_ANY`

The rationale for subnetting is that an organization doesn't attach all of its hosts to a single network. The subnet mask services the same role as described as the extended network ID.

### IPv6

A series of 16-bit hexadecimal numbers separated by colons

- loopback: `::1`

- wildcard `0::0` or `::`

IPv4 address are represented as 80zeros-FFFF-IPv6_addr, written as `::FFFF:204.152.189.116`


## Transport Layer

Port numbers: a way to differentiate the applications on a host. The same well-known port number is usually assigned to a service under both TCP and UDP.

If an application doesn'nt select (`bind()`) a particular port, then TCP and UDP assign a unique _ephemeral port_ number to the socket, typically from 49152 to 65535.

### UDP

Two features added to IP:

1. Port number

2. data checksum to allow detection of errors in the transmitted data.

A UDP-based application generally doesn’t know the MTU of the path between
the source and destination hosts. UDP-based applications that aim to avoid IP frag-
mentation typically adopt a conservative approach, which is to ensure that the
transmitted IP datagram is less than the IPv4 minimum reassembly buffer size of
576 bytes.

### TCP

Reliable, connection-oriented, bidirectional, byte-stream communica-
tion channel between two endpoints

1. Communication starts with establishing a connection.

2. data packaged in segments, each segment is transmitted in a single IP datagram.

3. (arrival guarantee) The receiver sends back a positive acknowledgement to the sender after successfully receiving a segment. With each segment sent, the sender starts a timer for possible retransmissions in case of a timeout.

4. Sequencing (no duplication, in order): each byte transmitted is assigned a logical sequence number (with the randomly starting one called ISN, _initial sequence number_), indicating the position of that byte in the data stream for the connection (Different on the both ends). The sequence number allows TCP segments to be assembled in the correct order at the destination and then passed as a byte stream to the application layer (the underlying segments may still be out of order). The acknowledgement message tells the receiver what segment has been received using the sequence number. Duplicates have the same sequence number so they can be detected and discarded.

5. Flow control: prevents a fast sender from overwhelming a slow receiver. Use buffering and sling windows mechanism.

6. Congestion control: slow-start and congestion-avoidance algorithms, preventing a fast sender from overwhelming the network.
