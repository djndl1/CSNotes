The socket network IPC interface can be used by processes to communicate with other processes, regardless of where they are running — on the same machine or on different machines. The socket interface can be used to communicate using many different network protocols

# Socket Descriptors

A socket is an abstraction of a communication endpoint. Applications use socket descriptors to access sockets. Socket descriptors are implemented as file descriptorss in Unixes.

The `socket` function creates a socket. Calling `socket` is similar to calling open. Communication on a socket is bidirectional. `shutdown()` disable I/O on a socket. It should be used over `close` because it deactivates a socket independently of the number of active file descriptors referencing it.

# Addressing

The machine’s network address helps us identify the computer on the network we wish to contact, and the service, represented by a port number, helps us identify the particular process on the computer. 

The TCP/IP protocol suite uses big-endian byte order. Applications sometimes need to translate them between the processor ’s byte order and the network byte order. 

Four functions are provided to convert between the processor byte order and the network byte order for TCP/IP applications:

```c
 uint32_t htonl(uint32_t hostlong);

 uint16_t htons(uint16_t hostshort);

 uint32_t ntohl(uint32_t netlong);

 uint16_t ntohs(uint16_t netshort);
```
