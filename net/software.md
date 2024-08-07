# Network Software

To reduce design complexity, most networks are organized as a stack of _layers_ or _levels_. The purpose of each layer is to offer certain services to the higher layers while shielding those layers from the details of how the offered services are actually implemented. When layer n on one machine carries on a conversation with layer n on another machine (_peers_), the rules and conventions used in this conversation are collectively known as the _layer n protocol_. Between each pair of adjacent layers is an _interface_, defining which primitive operations and services the lower layer makes available to the upper one.

A set of layers and protocols is a _network architecture_. A list of the protocols used by a certain system, one protocol per layer, is called a _protocol stack_.

One mechanism for finding errors in received information uses codes for error detection. Finding a working path through a network is called routing. Identifying the senders and receivers is called _addressing_/_naming_. Different networks being connected together is called internetworking. Flow control is about controlling sending/receiving rate between layers. Quality of service is the mechanism that reconcile competing demands from users.

Layers can offer two different types of service to the layers above them: 

- connection-oriented: to use a connection-oriented network service, the service user first establishes a connection, uses the connection, and then releases the connection. In most case, the sending order is preserved so that the bits arrive in the order they were sent. The sender, receiver, and subnet conduct a negotiation about the parameter to be used when establishing a connection. A connection with associated resources is called a _circuit_.

- connectionless: each message carries the full destination address and each is routed through the intermediate nodes inside the system independent of all the subsequent messages. These messages have different names in different contexts. A packet (at the network layer) is forwarded using store-and-forward switching or cut-through switching. it is possible that the first one sent can be delayed so that the second one arrives first.

Each of the both can be further characterized by its reliability. Reliable connection-oriented service has two minor variations: message sequences and byte streams. Unreliable (meaning not acknowledged) connectionless service is often called datagram service, which also does not return an acknowledgement to the sender. To add reliability to connectionless service, acknowledged datagram is one way, request-reply (usually used in client-server model).

A service is formally specified by a set of _primitives_ (operations) available to user processes to access the service. These primitives tell the service to perform some action or report on an action taken by a peer entity.

# Reference Models

## The OSI (Open System Interconnection) Reference Model

Three concepts, _services_ (sematics of a layer), _interfaces_ (how to access a layer) and _protocols_ (between peer entites, inside a layer) are central to the OSI model. These ideas fit nicely with OOP.



```
+-----------------+
|   Application   | ---> protocols commonly used by users
+-----------------+
|  Presentation   | ---> symtax and semantics of the information transmitted
+-----------------+
|    Session      | ---> establishes sessions (dialog control, token management, synchronization)
+-----------------+
|   Transport     | ---> split data from above into smaller units, ensure all data correctly arrive
+-----------------+
|    Network      | ---> controls the operation of the subnet, how packets are routed
+-----------------+
|  Data  Link     | ---> transforms a raw transmission facility into a line free of undetected transmission errors
+-----------------+
|     Physical    | ---> trasmitts raw bits over a communication channel
+-----------------+
```

- A layer should be created where a different abstract is needed

- Each layer should perform a well-defined function

- The function of each layer should be chosen with an eye toward defining internationally standardized protocols

- The layer boundaries should be chosen to minimize the information flow across the interfaces

- The number of layers should be large enough that distinct functions need not be thrown together in the same layer out of necessity and small enough that the architecture does not become unwieldy.

## The TCP/IP Reference Model

```
                  +----------------------------------------------------------+
                  |                                                          |
Application       |    HTTP     SMTP     RTP      DNS                        |
                  |                                                          |
                  +----------------------------------------------------------+
                  |                                                          | 
Transport         |               TCP            UDP                         |
                  |                                                          |
                  +----------------------------------------------------------+
                  |                                                          | allow hosts to inject packets into
Internet          |               IP           ICMP                          | any network and have them travel 
                  |                                                          | independently to the destination
                  +----------------------------------------------------------+
                  |                                                          | an interface between hosts and 
 Link             |    DSL      SONET     802.11        Ethernet             |
                  |                                                          | transmission links
                  +----------------------------------------------------------+
```

## A Practical Model

```
+---------------+
|               |
|  Application  | interacting with applications
|               |
+---------------+
|               |
|  Transport    | increase reliability, provide delivery abstractions
|               |
+---------------+
|               |
|    Network    | how to combine multiple links into networks
|               |
+---------------+
|               |
|     Link      | how to send to finite-length messages between directly connected computers with reliability
|               |
+---------------+
|               |
|   Physical    | how to transmit bits across different kinds of media
|               |
+---------------+
```

# Cases Analysis

## ARPANET

- distributed, digital packet-switching

- subnets and hosts; subnet-host protocol, subnet-subnet protocol.

- TCP/IP to handle heterogeneous networks

- BSD sockets

## NSFNET

- Connected through fuzzballs

- Used TCP/IP from the beginning

- Connected to ARPANET

## Internet

Users access the Internet through DSL, dialup, FTTH or mobile network to ISPs' POPs (point of presence). ISPs have their backbones, long distance transmission lines that interconnect routers at POPs in the different cities that the ISPs serve. ISPs connect their networks to exchange traffic at IXPs (Internet eXchnage Points).

ISPs who publicly compete with one another for customers often privately cooperate to do peering. Tier 1 ISPs don't pay transit. Companies locate their data centers at ISP POPs so that they have short fast connections.

## 3G Mobile Phone Networks

The scarity of spectrum leads to the cellular network design, under which within a cell, users are assigned channels that do not interfere with each other.

TODO

## 802.11

- Unlicensed bands such as ISM

- Infrastructure/Ad hoc

TODO

## RFID and Sensor Networks

TODO

# Standard 

TODO
