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



