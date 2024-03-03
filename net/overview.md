# Intro

Computer network means *a collection of autonomous computers interconnected by a single technology*. A distributed system is a collection of independent computers appears to its users as a single coherent system.

## Use

Modern information distribution depends on highly targeted behavioral advertising. Communication is typically based client-server model or peer-to-peer communication. Instant messaging, social networks have restructured person-to-person communication. 
E-commerce, online entertainment have been popular.

Ubiquitous computing was proposed to entails computing in everyday life, resulting in IoT. Various consumer electronic devices are networked, even through power-line networks.

## Types

- Broadband Access Networks

- Wireless Access Networks: cellular networks, WIFI, NFC, sensor networks.

- Content Provider Networks: CDN (a large collection of servers that are geographically distributed in such a way that content is placed as close as possible to the users that are requesting it).

- Transit Network, Backbone networks: among ISPs and large content providers. The recent decade has seen more direct interconnection instead of relying on the transit network.

- Enterprise Networks: companies, factories, VPNs, VoIP

## Techonology

- Personal Area Networks (PAN): bluetooth

- Local Area Networks (LAN): WLAN, mesh network, IEEE 802.11, Ethernet, VLAN

- Home networks: a LAN, but with various IoT devices, demanding reliability, security and low cost.

- Metropolitan Area Networks (MAN): cable television networks, IEEE 802.16 WiMAX, LTE, 5G

- Wide Area Network: subnet (switches and their communication lines)

- Virtual Private Networks, Software-Defined WAN

- Internetworks: a collection of interconnected networks. This does not conform to the definition above of a computer network. e.g. the global Internet
  - *gateway*: the device that makes a connection between two or more networks and provides the necessary translation in terms of hardware and software, e.g. router.
  
## Examples

### Internet

DoD required a robust Command & Control network. An idea based on digital packet switching was proposed. Transmitted data is divided into packets and sent independently towards the destination. Each packet was received in its entirety before being forwarded (store-and-forward). ARPANET was built on this idea and expanded to various institutions, but its protocols ware not suitable for running over different networks, leading to the invention of TCP/IP protocols. the US National Science Foundation built a WAN based on TCP/IP, the NSFNET. Later, NSFNET was commercialized and developed into a larger internetwork, operated by different providers. Similar networks in other countries were also incorporated later.

#### Architecture

Ordinary users access the Internet by fiber (FTTX), wireless or cable (DOCSIS, DSL). Custom packets enter the ISP network at the ISP's POP (Point of Presence). Routers and transmission lines connect POPs together (the backbone of the ISP). ISPs exchange their traffic at IXPs (Internet eXchange Points), a large site where routers of different ISPs are connected to each other in a PLAN. Some small ISPs would pay other ISPs for internet connectivity to reach distant hosts (transit). Data centers are built near or at ISP POPs for fast connection. In the recent decade, content providers have been starting to build direct connections between each other or server farms inside an ISP's network.

### Mobile Network

### Early Generations

- 1G (AMPS, 1982): analog voice signals

- 2G (GSM, 1991): digital signals; voice calls and text messaging

- 3G 2001: both digital voice and broadband digital data services

Radio coverage area is divided into cells to reuse a certain range of ratio spectrum. 

#### 4G Architecture

- *Code Division Multiple Access* (CDMA)

- *eNodeB*: the celluar base station

- *Radio Access Network*: the cellular base station with its controller

- *Core Network*: fully packet-switching network (Evolved Packet Core) that carries the traffic for the ratio access network. Older mobile phone networks used a circuit-switched core in the style of the traditional phone network to carry void calls.

Customers access the network through E-UTRAN (the radio communication protocol) to eNodeB, entering the Ratio Access Network. The RAN is connected to the core network, which connects to external packet networks such as the Internet through S-GW (Serving Network Gateway) and the P-GW (Packet Data Network Gateway).

A device may change its connecting base station through *handover*, soft (connecting the new one before the old one drops) or hard (connecting to the new one after the old one drops)

Each mobile phone network has a *Home Subscriber Server* (HSS) that keeps records of the location of each subscriber.

Security is enforced using SIM (Subscriber Identity Module) cards as basis for authentication and encryption.

### 802.11 WiFi

- AP mode and Ad-Hoc mode

- Path diversity to overcome multipath fading

- OFDM (Orthonogal Frequency Division Multiplexing)

- Carrier Sense Multiple Access (CSMA): wireless transmission does not occur in the same medium coverage, unlike classic Ethernet, a party may not be able to detect that another is already using the target channel due to different signal coverage.

- Security: WEP (Wired Equivalent Privacy) -> WPA (WiFi Protected Access) -> WPA2 -> 802.1X

## Social Issues

- Online speed: Illegal online content is not going unpunished; Platforms are responsible for their content; DMCA

- network neutrality: ISPs should provided equal quality of service to as given type of application traffic, regardless of who is sending that content.
  - no blocking, no throttling, no paid prioritization and transparency about reasonable network management practices that might be seen as violating any of the first three rules.
  
- Security: DDoS and botnet; email spamming; phishing

- Privacy: profiling and tracking; browser fingerprinting: use the unique configuration of the browser to determine the identity; location privacy. Anonymous accusations may not be valid under certain justice systems.

- Disinformation

## Network Protocols

### Goals & Requirements

- *Reliability*: the ability to recover from errors, faults or failures.
  - a network has to operate correctly even though it is comprised of a collection of components that are themselves unreliable
    (fluke eletrical noise, random wireless signals, hardware flaws, software bugs etc).
  - error detection, error correction by adding redundant information, used at low layers.
  - path routing

- *Resource Allocation*: sharing access to a common, limited resource: bandwidth, latency etc.
  - *scalable* designs continue to work well when the network gets large.
  - statistical multiplexing (sharing bandwidth based on the statistics of demand)
  - *flow control*:  how to keep a fast sender from swamping a slow receiver with data. Too much traffic causes congestion.

- *Evolvability*: allowing for incremental deployment of protocol improvements over time.
  - protocol layering to support change

- *Security*: defending the network against various types of attacks. Confidentiality, authenticity and integrity.

### Protocol Layering

> The purpose of each layer is to offer certain services to the higher layers while shielding those layers from the details of how the offered services are actually implemented.

- *peers*: The entities comprising the corresponding layers on different machines

> The peer processes in layer 4, for example, conceptually think of their communication as being ‘‘horizontal,’’ using the layer 4 protocol. Each one is likely to have procedures called something like SendToOtherSide and GetFromOtherSide, even though these procedures actually communicate with lower layers across the 3/4 interface, and not with the other side. The peer process abstraction is crucial to all network design. Using it, the unmanageable task of designing the complete network can be broken into several smaller, manageable design problems, namely, the design of the individual layers. As a consequence, all real networks use layering.

- *protocol*: is an agreement between the communicating parties on how communication is to proceed. A set of rules governing the format and meaning of the packets, or messages that are exchanged by the peer entities within a layer.

- *network architecture*: a set of layers and protocols

- *protocol stack*: a list of the protocols used by a certain system

- *Service*: a set of primitives (operations) available to the upper layer. It relates to an interface between two layers.

### Connetion-Oriented or Connectionless Service

- Connection-Oriented: modeled after the telephone system, the order is preserved, a negotiation is conducted before the actual data transfer.
  - a connection may send messages or byte stream.

- Connectionless: modeled after the postal system.  Each message carries the full destination address, and each one is routed through the intermediate nodes inside the system independent of all the subsequent messages.  Each message is sent either by store-and-forward switching or cut-through switching.
  - unreliable datagram service: no acknowledgement
  - acknowledged datagram service: e.g. text messaging
  
Some reliable services are built upon lower unreliable protocols.

# Reference Models

Two prevailing layered protocol models are the TCP/IP reference model and the OSI reference model.

## The OSI (Open System Interconnection) Reference Model

Three concepts, _services_ (sematics of a layer), _interfaces_ (how to access a layer) and _protocols_ (between peer entites, inside a layer) are central to the OSI model. These ideas fit nicely with OOP.


```
┌─────────────┐                                            ┌────────────┐      
│ Application │◄──────────────────────────────────────────►│ Application│  APDU
└─────▲───────┘                                            └─────▲──────┘      
      │                                                          │             
┌─────▼───────┐                                            ┌─────▼──────┐      
│ Presentation│◄──────────────────────────────────────────►│Presentation│  PPUD
└─────▲───────┘                                            └─────▲──────┘      
      │                                                          │             
┌─────▼───────┐                                            ┌─────▼──────┐      
│ Session     │◄──────────────────────────────────────────►│   Session  │  SPDU
└─────▲───────┘                                            └─────▲──────┘      
      │                                                          │             
┌─────▼───────┐                                            ┌─────┴──────┐      
│ Transport   │◄──────────────────────────────────────────►│  Transport │  TPDU
└─────▲───────┘                                            └─────▲──────┘      
      │                                                          │             
      │                                                          │             
      │                Communication subnet boundary             │             
      │         ┌───────────────────────────────────────┐        │             
      │         │                                       │        │             
      │         │           Internet subnet protocol    │        │             
┌─────▼───────┐ │    ┌───────────┐     ┌────────────┐   │  ┌─────▼──────┐      
│ Network     │◄├───►│ Network   │◄───►│  Network   │◄──┼─►│  Network   │Packet
└─────▲───────┘ │    └───────────┘     └────────────┘   │  └─────▲──────┘      
      │         │                                       │        │             
┌─────▼───────┐ │    ┌───────────┐     ┌────────────┐   │  ┌─────▼──────┐      
│ Data Link   │◄├───►│ Data Link │◄───►│ Data Link  │◄──┼─►│ Data Link  │Frame 
└─────▲───────┘ │    └───────────┘     └────────────┘   │  └─────▲──────┘      
      │         │                                       │        │             
┌─────▼───────┐ │    ┌───────────┐     ┌────────────┐   │  ┌─────▼──────┐      
│  Physical   │◄├───►│  Physical │◄───►│  Physical  │◄──┼─►│  Physical  │Bit   
└─────────────┘ │    └───────────┘     └────────────┘   │  └────────────┘      
    Host A      │       Router             Router       │      Host B          
                │                                       │                      
                └───────────────────────────────────────┘                      
```

- A layer should be created where a different abstract is needed

- Each layer should perform a well-defined function

- The function of each layer should be chosen towards defining internationally standardized protocols

- The layer boundaries should be chosen to minimize the information flow across the interfaces

- The number of layers should be large enough that distinct functions need not be thrown together in the same layer out of necessity and small enough that the architecture does not become unwieldy.

The OSI model makes clear distinction between *services*, *interfaces* and *protocols*.

The OSI model came out at a bad time when TCP/IP has been proposed and used on Unix with and was beginning to gain traction. Due to some design flaws in the model, initial implementations were of poor quality. The OSI model has never gained widespread use.

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

- *Link*: not really a lower but rather an interface between hosts and transmission links.

- *Internet*: an official packet format and protocol called *Internet Protocol* (IP) and a companion protocol *Internet Control Message Protocol* (ICMP). Packet routing and congestion management are two major issues.

- *Transport*:
  - *TCP*: reliable, connection-oriented, byte-stream with flow control
  - *UDP*: unreliable, connectionless
  
- *Application*: include any session and presentation functions that they any application requires.

The TCP/IP model does not make a clear distinction of services, protocols and interfaces. Difficult to generalize to other protocol stacks. No distinction between the link layer and the physical layer. Some early protocols (telnet) were not well designed and implemented due to their ad-hoc nature.

## A Practical Model

```
+---------------+
|               |
|  Application  | interacting with applications
|               |
+---------------+
|               |
|  Transport    | stengthen the deliveery guarantees of the Network layer: increase reliability, provide delivery abstractions
|               |
+---------------+
|               |
|    Network    | how to combine multiple links into networks and internetworks
|               |
+---------------+
|               |
|     Link      | how to send to finite-length messages between directly connected computers with reliability (within a link)
|               |
+---------------+
|               |
|   Physical    | how to transmit bits across different kinds of media
|               |
+---------------+
```

>  the strength of the OSI reference model is the model itself (minus the presentation and session layers), which has proven to be exceptionally useful for discussing computer networks. In contrast, the strength of the TCP/IP reference model is the protocols, which have been widely used for many years.
