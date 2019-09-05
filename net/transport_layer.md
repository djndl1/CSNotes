Together with the network layer, the transport layer is the heart of the protocol hierarchy. The network layer provides end-to-end packet delivery using datagrams or virtual circuits. The transport layer builds on the network layer to provide data transport from a process on a source machine to a process on a destination machine with a desired level of reliability that is independent of the physical networks currently in use. It provides the abstractions that applications need to use the network.

 It can be difficult to provide a connectionless transport service on top of a connection-oriented network service, since it is inefficient to set up a connection to send a single packet and then tear it down immediately afterwards. 
 
 Users have no real control over  the network layer.  Putting on top of the network layer another layer is the only possbility that improves the quality of the service. The existence of the transport layer makes it possible for the transport service to be more reliable than the underlying network.  Hiding the network service behind a set of transport service primitives ensures that changing the network merely requires replacing one set of library procedures with another one that does the same thing with a different underlying service. Application programmers can write code according to a standard set of primitives and have these programs work on a wide variety of networks.

The bottom four layers can be seen as the _transport service provider_, whereas the upper layer(s) are the _transport service user_. 

# Transport Service Primitives

A connection-oriented example:

- `LISTEN`: block until some process tries to connection

- `CONNECT`: Actively attempt to establish a connection

- `SEND`: send information

- `RECEIVE`: block until a data packet arrives

- `DISCONNECT`: request a release of the connection

Segment is the term for packet/frame in the transport layer.

Every data packet sent will be acknowledged. The packets bearing control segments are also acknowledged.

Real world example, Berkeley Sockets:

- `SOCKET`: create a communication endpoint

- `BIND`: associate a local address with a socket

- `LISTEN`: announce willingness to accept connections; allocate space to queue incoming calls; nonblocking

- `ACCEPT`: block waiting; passively establish an incoming connection

- `CONNECT`: actively attempt to establish a connection

- `SEND`: send some data over the connection

- `RECEIVE`: receive some data from the connection

- `CLOSE`: release the connection

```c
// file server code
#include <sys/types.h>
#include <sys/fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SERVER_PORT 12345
#define BUF_SIZE 4096
#define QUEUE_SIZE 10

int main(int argc, char *argv[])
{
        int s, b, l, fd, sa, bytes, on = 1;
        char buf[BUF_SIZE];
        struct sockaddr_in channel;

        /* build address structure to bind to socket */
        memset(&channel, 0, sizeof(channel));
        channel.sin_family = AF_INET;
        struct in_addr server_addr;
        if (inet_pton(AF_INET, argv[1], &server_addr) != 1) {
                perror("Failed to convert address\n");
                exit(1);
        }
        channel.sin_addr.s_addr = server_addr.s_addr;
        channel.sin_port = htons(SERVER_PORT);

        char rv_addr[5];
        inet_ntop(AF_INET, &channel.sin_addr, rv_addr, INET_ADDRSTRLEN);
        printf("Server Address: %s, %x:%d\n", rv_addr,
               ntohl(channel.sin_addr.s_addr), ntohs(channel.sin_port));
        // passive open, Waiting for connection
        s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        if (s < 0) {
                perror("failed to create the socket\n");
                exit(2);
        }
        setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char*)&on, sizeof(on));
        if (bind(s, (struct sockaddr *)&channel, sizeof(channel)) < 0) {
                perror("bind error\n");
                exit(3);
        }
        if (listen(s, QUEUE_SIZE) < 0) {
                perror("listen failed\n");
                exit(4);
        }

        while (1) {
                sa = accept(s, 0, 0);
                if (sa < 0) {
                        perror("accept failed");
                        exit(5);
                }
                read(sa, buf, BUF_SIZE);
                printf("%d\n", buf[strlen(buf)-1]);
                //buf[strlen(buf)-1] = '\0';
                printf("opening file %s, %d\n", buf, strlen(buf));
                fd = open(buf, O_RDONLY);
                if (fd < 0) {
                        perror("open failed");
                        exit(6);
                }

                while (1) {
                        bytes = read(fd, buf, BUF_SIZE);
                        if (bytes <= 0) break;
                        write(sa, buf, bytes);
                }
                close(fd);
                close(sa);
        }
        return 0;
}

```

```c
//client code

#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUF_SIZE 4096
#define SERVER_PORT 12345

int main(int argc, char *argv[])
{
        int c, s, bytes;
        char buf[BUF_SIZE];
        struct hostent *server;
        struct sockaddr_in channel;

        if (argc != 3) {
                printf("Usage: client server_IP filename\n");
                exit(1);
        }
        s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        if (s < 0) {
                perror("Failed to create a socket!\n");
                exit(2);
        }
        memset(&channel, 0, sizeof(channel));
        channel.sin_family = AF_INET;
        inet_pton(AF_INET, argv[1], &channel.sin_addr);
        channel.sin_port = htons(SERVER_PORT);

        if (connect(s, &channel, sizeof(channel)) < 0) {
                perror("Failed to connect!\n");
                exit(2);
        }
        write(s, argv[2], strlen(argv[2])+1);
        

        while (1) {
                bytes = read(s, buf, BUF_SIZE);
                if (bytes <= 0) break;
                write(1, buf, bytes);
        }

        return 0;
}

```

# Elements of Transport Protocols

At the transport layer, the physical channel is the entire network, explicit addressing of destinations is required. Datagrams may loiter inside the network for a while before arriving at the destination out of order. Buffering and flow control are needed.

## Addressing

TSAP (Transport Service Access Point) means a specific endpoint in the transport layer. NSAP (Network Service Access Point) are endpoints in the network layer (e.g. IP addresses).

Stable ports may be a way to determine where to connect. Another way is to query a known port, behind which runs the portmapper that returns the port of the queried service.
