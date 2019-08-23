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
