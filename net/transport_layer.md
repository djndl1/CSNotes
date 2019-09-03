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
