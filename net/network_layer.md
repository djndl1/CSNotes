The network layer is the lowest layer that deals with end-to-end transmission (data link layer is concerned about only from one end of the wire to the other). The network layer must know about the topology of the network and choose appropriate paths through it.

# Main Design Issues

- Store-and-Forward Packet Switching: a packet is stored in a route until it has fully arrived and the link has finished its processing by verifying the checksum. then it is forwarded to the next router along the path until it reaches the destination host.

- services provided upward: uniform addressing; topology-unrelated; If connectionless service is offered, packets (often called _datagrams_)are injected into the network individually and routed independently of each other. No advance setup is needed. Internet Protocol, the basis for the entire Internet, is the dominant example of a connectionless network service. If connection-oriented service is used, a path from the source router all the way to the destination router must be established before any data packets can be sent, forming a _virtual circuit_. The idea behind virtual circuits is to avoid having to choose a new route for every packet sent. A route from the source machine to the destination machine is chosen as part of the connection setup and stored in tables inside the routers, using lable switching. MPLS, used within ISP networks in the Internet, with IP packets wrapperd in an MPLS header is an example of a connenction-oriented network service.


In datagram network, every packet contains the full source and destination address. Routers do not hold state information about connections. Each packet is routed independently. Datagrams allow the routers to balance the traffic throughout the network. In virtual-circuit network, each packets contains a short VC number. Each VC requires route table space per connection. Route is chosen when VC is st up; all packets follow it. All VCs that passed through the failed router are terminated.

# Routing Algorithm

The algorithms that choose the routes and the data structures that they use are major area of network layer design. The routing algorithm is the part of the network layer software responsible for deciding which output line an incoming packet should be transmitted on. In VC networks, a routing is called a _session routing_.

 - forwarding: A router handles each packet as it arrives, looking up the outgoing line to use for it in the routing tables
 
 - routing: filling in and updating the routing tables. 
 
 Robustness requires a network to run continuously for years without failing and to deal with constantly changing topology. Stability requires fast convergence and equilibrium. Fairness and efficiency may be approach in differnt ways: minimizing the mean packet delay, or maximizing total network throughout.
 
 Routing algorithms can be grouped into two major classes: _nonadaptive_ and adaptive (static and dynamic).
 
 optimality principle: If router J is on the optimal path from router I to router K, then the optimal path from J to K also falls along the same route.
 
 TODO

# Congestion Control

Too many packets present in a part of the network causes packet delay and loss that degrades performance. The network and transport layers share the responsibility for handling congestion.  it is the network layer that directly experiences it and must ultimately determine what to do with the excess packets. the most effective way to control congestion is to reduce the load that the transport layer is placing on the network. This requires the network and transport layers to work together.

Unless the network is well designed, it may experience a _congestion collapse_, in which performance plummets as the offered load increases beyond the capacity. packets can be sufficiently delayed inside the network that they are no longer useful when they leave the network. A different failure mode occurs when senders retransmit packets that are greatly delayed, thinking that they have been lost, which wasting the capacity again.

If routers have an infinite amount of memory, congestion gets worse, not better. This is because by the time packets get to the front of the queue, they have already timed out (repeatedly) and duplicates have been sent.

Flow control relates the traffic between a particular sender and a particular receiver. Congestion control is a global 

- provisioning: the most basic way to avoid congestion is to build a network that is well matched to the traffic that it carries. 

- traffic-aware routing: splitting traffic across multiple paths

- admission control: decrease the load

- traffic throttling: request the sources to throttle their traffic

- load shedding: just choose the packets to discard

TODO 


# Quality of Service

Provide qualify of service that is matched to application needs. An easy solution to provide  good quality of service is to build a network with enough capacity for whatever traffic will be thrown at it (overprovisioning).

Four issues must be addressed to ensure quality of service:

- What applications need from the network 

- How to regulate the traffic that enters the network 

- How to reserve resources at routers to guarantee performance

- Whether the network can safely accept more traffic.

TODO 


# Internetworking

Numerous protocols are in widespread use across different networks in every layer. Getting packets from one network to another is not always so easy. The Internet is the prime example of interconnection. 

Networks may differ in modulation techniques or frame formats, which are internal to the physical and data  link layers. It's the interfaces between between the network layer and the data link layer that matter. When packets sent by a source on one network must transit one or more foreign networks before reaching the destination, many problems can occur at the interfaces between networks. e.g. how multicast is supported, how packets are splitted, connectionless-to-connection transit

With a router, the packet is extracted from the frame and the network address in the packet is used for deciding where to send it.  With a switch (or bridge), the entire frame is transported on the basis of its MAC address. Switches do not have to understand the network layer protocol being used to switch packets. Routers do. when bridges were introduced, it was intended that they would join different types of networks, or at least different types of LANs. They were to do this by translating frames from one LAN into frames from another LAN. Today, bridges are predominantly used to connect the same kind of network at the link layer, and routers connect different networks at the network layer.

Internetworking has been very successful at building large networks, but it only works when there is a common network layer. There have been many network protocols over time. A router that can handle multiple network protocols is called a _multiprotocol router_. It must either translate the protocols, or leave connection for a higher protocol layer.

- tunneling: the source and the destination hosts are on the same type of network but there is a different network in between. It transmits one computer network protocol that is encapsulated inside another network protocol, e.g. IPv6 over IPv4. The path through the other network can be seen as a big tunnel extending from one multiprotocol router to the other (tunneling a car from France to England). Tunneling is widely used to connect isolated hosts and networks using other networks. The network that results is called an _overlay_.
 
Within each network, an intradomain or interior gateway protocol is used for routing. Across the networks that make up the internet, an interdomain or exterior gateway protocol (Border Gateway Protocol).

Each network or link imposes some maximum size on its packets. The network designer are not free to choose any old maximum packet size they wish. One solution is to make sure the problem not occur in the first place (which is impractical). The alternative solution to the problem is to allow routers to break up packets into fragments, sending each fragment ass a separate network layer packet. One way to recombine the fragments back into the original is transparent fragmentation, that is, the network that fragments the packet is responsible for recombining them when forwarding to another network. The other fragmentation strategy is to refrain from recombining fragments at any intermediate routers and reassembly is performed only at the destination host. The design used by IP is to give every fragment a packet number, carried on all packets, an absolute byte offset within the packet, and a flag indicating whether it is the end of the packet. Modern Internet uses _path MTU discovery_. Each IP packet is sent with its header bits to indicate that no fragmentation is allowed to be performed. If a router receives a packet that is too large, it generates an error packet, returns it to the source and drops the packet. The source then refragment the packet according to the error packet. TCP and IP typically implemented together to be able to pass this sort of information. This adds some startup delay.

# The Network Layer in the Internet

Ten principles:

1. make sure it works; do not finalize the design or standard until multiple prototypes have successfully communicated with each other.

2. Keep it simple

3. Make clear choices: choose only one way to do a thing

4. Exploit modularity

5. Expect heterogeneity 

6. avoid static options and parameters

7. a good design need not to be perfect

8. Be strict when sending and tolerant when receiving

9. Think scalability

10. consider performance and cost


the biggest backbones to which everyone else connects to reach the rest of the Internet, are called _Tier 1 networks_. Attached to the backbones are ISPs (Internet Service Providers) that provide Internet access to homes and businesses, data centers and colocation facilities full of server machines, and regional (mid-level) networks. The glue that holds the whole Internet together is the network layer protocol, IP (Internet Protocol). It provides a _best-effort_ way to transport packets from source  to destination.

## The IP Version 4 Protocol 

An IPv4 datagram consists of a header part and a body or payload part. The header has a 20-byte fixed part and a variable-length optional part.

```
<------------------------------------- 32  bits ----------------------------------------->

+------------|--------------|------------------------------|------------------------------+
|  Version   |     IHL      |  Differentiated services     |       Total length           |
+------------|--------------|-------------------------------------------------------------+
|                    identification                        |  |DF|MF|   Fragment offset   |
+------------------------------|----------------------------------------------------------+
|     Time to live             |        Protocol           |      Header Checksum         |
+------------------------------|---------------------------|------------------------------+
|                                  Source address                                         |
+-----------------------------------------------------------------------------------------+
|                                Destination address                                      |
+-----------------------------------------------------------------------------------------+
|                                    Options (0 or more words)                            |
|                                                                                         |
+-----------------------------------------------------------------------------------------+
```

- IHL: how long the header is, 20-60 bytes;

- differentiated services: distinguish between different type of service; the top 6 bits mark the packet with its service class, the bottom 2 bits are used to carry explicit congestion notification information;

- total length: length everything in the datagram;

- identification: allow the destination host to determine which packet a newly arrived fragment belongs to;

- DF: don't fragment;

- MF: yet more fragments to come;

- fragment offset: tells where in the current packet this fragment belongs;

- TtL (time to live): a counter used to limit packet lifetimes, hop count. It prevents packets from wandering around forever.

- protocol: which transport process to give the packet to, e.g. TCP, UDP et al.;

- checksum: the header rates its own checksum for protection;

- options: allow subsequent versions of the protocol to include information not present in the original design, to permit experimenters to try out new ideas. It has fallout out of favor, partly supported and rarely used.

## IP Address

An IP address does not actually refer a host, but a network interface. An IP address has a prefix, denoting network, often represented by a number of length `128.208.0.0/24`, which forms a _subnet mask_ `255.255.255.0`.  Routers can forward packets based on only the network portion of the address, as long as each of the networks has a unique address block.

Allowing the block of addresses to be split into several parts for internal use as multiple networks is called _subnetting_, which creates _subnets_. A router simply ANDs the destination address and all the subnet masks and decides to which network it should forward. The subnet divisions can be changed later if necessary, by updating all subnet masks at routers

Routers in organizations at the edge of a network, such as a university, need to have an entry for each of their subnets, telling the router which line to use to get to that network. For routes to destinations outside of the organization, they can use the simple default rule of sending the packets on the line toward the ISP that connects the organization to the rest of the Internet. Routers in ISPs and backbones in the middle of the Internet. must know which way to go to get to every network and no simple default will work. These core routers are said to be in the default-free zone of the Internet. Routing algorithms require each router to exchange information about the addresses it can reach with other routers.

To form a hierarchy, we can also combine multiple small prefixes into a single larger prefix, called route aggregation, forming a _supernet_. With aggregation, IP addresses are contained in prefixes of varying sizes. It is up to each router to have the corresponding prefix information. This design works with subnetting and is called _CIDR_ (Classless Inter-Domain Routing). Aggregation is heavily used throughout the Internet and can reduce the size of router tables to around 200,000 prefixes.

Prefixes are allowed to overlap. The rule is that packets are sent in the direction of the most specific route, or the longest matching prefix that has the fewest IP addresses. Note this does not mean  IPs are reused. It's just that some of IPs that seem to be under a subnet are not used in that subnet, instead, they are IPs under another subnet.

Conceptually, CIDR works as follows. When a packet comes in, the routing table is scanned to determine if the destination lies within the prefix. It is possible that multiple entries with different prefix lengths will match, in which case the entry with the longest prefix is used. 

### Classful and Special Addressing

TODO

## NAT (Network Address Translation)

To solve the scarcity of IPv4 addresses, NAT is widely used. The basic idea behind NAT is for the ISP to assign each home or business a single IP address (or at most, a small number of them) for Internet traffic. Within the customer network, every computer gets a unique IP address, which is used for routing intramural traffic. before a packet exits the customer network and goes to the ISP, an address translation from the unique internal IP ad- dress to the shared public IP address takes place. This translation makes use of three ranges of IP addresses that have been declared as private. 

- 10.0.0.0 - 10.255.255.255/8

- 172.16.0.0 - 172.31.255.255/12

- 192.168.0.0 - 192.168.255.255/16

The NAT designers observed that most IP packets carry either TCP or UDP payloads. Ports are effectively an extra 16 bits of addressing that identify which process gets which incoming packet.  Whenever
an outgoing packet enters the NAT box, the 10.x.y.z source address is replaced by the customer’s true IP address. In addition, the TCP Source port field is replaced by an index into the NAT box’s 65,536-entry translation table. This table entry contains the original IP address and the original source port. Finally, both the IP and TCP header checksums are recomputed and inserted into the packet. When a packet arrives at the NAT box from the ISP, the Source port in the TCP header is extracted and used as an index into the NAT box’s mapping table. From the entry located, the internal IP address and original TCP Source port are extracted and inserted into the packet. 

1. NAT violates the architectural model of IP, which states that every IP address uniquely identifies a single machine worldwide.

2. NAT breaks the end-to-end connectivity model of the Internet, which says that any host can send a packet to any other host at any time. A remote user cannot make connections to a game server on the home network,  Since the mapping in the NAT box is set up by outgoing packets.

3. Third, NAT changes the Internet from a connectionless network to a peculiar kind of connection-oriented network. 

4. NAT violates the most fundamental rule of protocol layering

5. processes on the Internet are not required to use TCP or UDP.

6. some applications use multiple TCP/IP connections or UDP ports in prescribed ways.

7. since the TCP Source port field is 16 bits, at most 65,536 machines can be mapped onto an IP address. 

Despite the issues, NAT is widely used in practice, especially for home and small business networks, as the only expedient technique to deal with the IP address shortage.


## IP Version 6

It uses 128-bit addresses; a shortage of these addresses is not likely any time in the foreseeable future. However, IPv6 has proved very difficult to deploy. It is a different network layer protocol that does not really interwork with IPv4, despite many similarities. 

IPv6 is not compatible with IPv4, but it is compatible with the other auxiliary Internet protocols, including TCP, UDP, ICMP, IGMP, OSPF, BGP, and DNS, with small modifications being required to deal with longer addresses.

IPv6 has longer address, simplifies the header, has better support for options.  Authentication and privacy are key features of the new IP. These were later retrofitted to IPv4.  More attention has been paid to quality of service.

```
+-----------------------|-----------------+
|Version|Diff.Services  |  Flow label     |
+-----------------------|-----------------+
| Payload length    |NextHeader| Hop Limit|
+-----------------------------------------+
|                                         |
|            Source Address               |
|             (16 bytes)                  |
|                                         |
+-----------------------------------------+
|                                         |
|            Destination Address          |
|              (16 bytes)                 |
|                                         |
+-----------------------------------------+
```

- Differentiated services: used to distinguish the class of service for packets with different real-time delivery requirements.

- Flow label: mark groups of packets that have the same requirements and should be treated in the same way by the network. Each flow for quality of service purposes is designated by the source address, destination address and flow number

- Next header: which extension header; which transport protocol handler (UDP, TCP or something else) to pass the packet to;

- Hop limit: keep packets from living forever;

- Source/Destination address: 16 byte addresses: leading zeros within a group can be omitted, one or more groups of 16 zero bits can be replaced by a pair of colons.

```
8000:0000:0000:0000:0123:4567:89AB:CDEF
8000::123:4567:89AB:CDEF
```

 IPv4 addresses can be written as a pair of colons and an old dotted decimal number: 
 
 ```
 ::192.31.20.46
 ```

All IPv6-conformant hosts dynamically determine the packet size to use  using the path MTU discovery. There is no need for another checksum field.

IPv6 provides extension headers for extra information in an efficient way:

- hop-by-hop: miscellaneous information that all routers along the path must examine. e.g. support of datagrams exceeding 64KB.
 
TODO


## Internet Control Protocols

The Internet has several companion cotnrol protocols that are used in the network layer, including ICMP, ARP, and DHCP.

### ICMP The Internet Control Message Protocol

When some unexpected occurs during packet processing at router, the event is reported by the ICMP. It is also used to test the Internet.

Each ICMP message type is carried encapsulated in an IP packet.

- `DELINEATION UNREACHABLE`: a router cannot locate the destination

- `TIME EXCEEDED`: TtL counter reaches zero. `traceroute` uses this.

- `PARAMETER PROBLEM`: an illegal value has been detected in a header field

- `SOURCE QUENCH`: conjestion control, rarely used; 

- `REDIRECT`: incorrectly routing.

- `ECHO`/`ECHO REPLY`: sent by the hosts see if a given destination is reachable and currently alive. Used by `ping`

- `TIMESTAMP REQUEST`/`TIMESTAMP REPLY`: similar to `ECHO`, with arrival time and departure time included

- `ROUTER ADVERTISEMENT`/`ROUTER SOLICITATION`: let hosts find nearby routers.

### ARP The Address Resolution Protocol

Data link layer NICs do not understand Internet addresses. To get the mapping from IP addresses to Ethernet addresses, the host outputs a broadcase to ask who owns a specific IP address. The protocol used for asking this question and getting the reply is _ARP_ (Address Resolution Protocol). Once a machine has run ARP, it caches the result in case it needs to contact the same machine shortly. The process of querying can also be the process of sending its mapping. Every machine broadcasts its mapping when it is configured. Proxy ARP is used in special cases in which a host wants to appear on a network even though it acatually resides on another one.

### DHCP The Dynamic Host Configuration Protocol

With DHCP, every network must have a DHCP server that is responsible for configuration. A host without an IP address broadcasts a DHCP DISCOVERY packet. The router may relay the packet if the DHCP server is not attached to the network. The server then allocates a free IP address and sends to the requesting host in a DHCP OFFER packet. IP address assignment may be for a fixed period time, a technique called _leasing_. The host must ask for a DHCP renewal before the lease expires. 

DHCP is used by ISPs to set the parameters of devices over the Internet access link, so that customers do not need to phone their ISPs to get this information (the network mask, the IP address of the default gatewawy, the IP address of DNS and time servers). 


### MPLS: MultiProtocol Label Switching

TODO

### OSPF: An Interior Gateway Routing Protocol

TODO

### BGP: The Exterior Gateway Routing Protocol

TODO


### Internet Multicasting

IP supports one-to-many communication or mutlicasting, using class D IP addresses. Twenty-eight bits are available for identifying groups. A best-effort attempt is made to deliver it to all the members of the group addressed but no guarantees are given.

The range of `224.0.0.0/24` is reserved for multicast on the local network. The packets are multicasst by simply broadcasting them on the LAN with a multicast address. Routers do not forward the packet off the LAN.

TODO

### Mobile IP

TODO
