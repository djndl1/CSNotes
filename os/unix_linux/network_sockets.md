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
