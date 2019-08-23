The socket network IPC interface can be used by processes to communicate with other processes, regardless of where they are running — on the same machine or on different machines. The socket interface can be used to communicate using many different network protocols

# Socket Descriptors

A socket is an abstraction of a communication endpoint. Applications use socket descriptors to access sockets. Socket descriptors are implemented as file descriptorss in Unixes.

The `socket` function creates a socket. Calling `socket` is similar to calling open. Communication on a socket is bidirectional. `shutdown()` disable I/O on a socket. It should be used over `close` because it deactivates a socket independently of the number of active file descriptors referencing it.

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

TODO

# Associating Addresses with Sockets

For a server, we need to associate a well-known address with the server's socket on which client requests will arrive.

`bind` function associates an address with a socket. For the Internet domain, if we specify the special IP address `INADDR_ANY` (defined in the socket endpoint will be bound to all the system’s network interfaces. `getsockname` discover the address bound to a socket. If the socket is connected to a peer, the peer's address  can be obtained by calling `getpeername`.
