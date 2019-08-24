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
