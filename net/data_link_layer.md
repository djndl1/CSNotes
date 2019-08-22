 The data link layer uses the services of the physical layer to send and receive bits over communication channels. It has three functions:

- Providing a well-defined service interface to the network layer: transmit the bits handled by the network layer to the destination machine. It provides unacknowledged connectionless service, acknowledged connectionless service and acknowledged connection-oriented service.

- Dealing with transmission errors

- Regulating the flow of data so that the slow receivers are not swamped by fast senders.

The key role of this layer is to achieve reliable, efficient communication of whole units of information called _frames_, which has a header, a payload for holding the packet, and a frame trailer. Frame management forms the heart of what the data link layer does.

# Main issues

## Framing

The physical layer cannot guarantee that all data bits arrive at the destination without failure. It is up to the data link layer to detect and correct errors. The usual approach to is compute a _checksum_.

There are four methods to break up a packet:

- byte count: unreliable, hard to find the boundary between frames if the count has been corrupted. Rarely used now.

- Flag bytes with bytes stuffing: having each frame start and end with special bytes, often the same byte called a _flag byte_. It marks frame boundaries with a FLAG byte and escape FLAG bytes and ESCAPE bytes inside the data with an ESCAPE byte.

- Flag bits with bit stuffing: delimiting the bit stream. Each frame begins and ends with a special bit pattern. The same bit pattern in the payload is stuffed with a bit, instead of an escape byte. USB uses this technique.

- Physical layer coding violation: uses coding violations to delimit frames.

Many data link protocols use a combination of the above methods.

## Error Control

The usual way to ensure reliable delivery is to provide the sender with some feedback about what is happening at the other end of the line. A timer is used so that the sender may not wait for a feedback forever. Sequence numbers are assigned to outgoing frames to prevent duplicates.

## Flow Control

Two approaches are commonly used:

- Feedback-based flow control

- rate-based flow control: only seen as part of the transport layer.

# Error Correcting and Error Detecting

These two techniques occupies a different ecological niche. There are burst errors and channel erasure (a bit is lost).

Error detecting codes are commonly used in link, network, and transport layers.

## Error-Correcting Codes

TODO

## Error-Detecting Codes

TODO

# Elementary Data Link Protocols

In a common implementation, the physical layer process and some of the data link layer process run on dedicate hardware called an NIC (Network Interface Card). The rest of the link layer process (taking the form of a device driver) and the network layer run on the main CPU as part of the OS. It may be possible that all three layers are implementated on a piece of hardware called a _network accelerator_.

When the data link layer accepts a packet, it encapsulates the packet in a frame by adding a data link header and trailer to it. A frame consists of an embedded packet, some control information (in the header) and a checksum (in the trailer).

```c
// file `protocol.h`

#define MAX PKT 1024

typedef enum {false, true} boolean;
typedef unsigned int seq nr;    // sequence or ack numbers
typedef struct {unsigned char data[MAX PKT];} packet; // packet definition
typedef enum {data, ack, nak} frame kind; // frame_kind defintion

typedef struct {
// header
frame kind kind;   // frame kind
seq_nr seq;        // sequence number
seq_nr ack;        // acknowledgement number

packet info;       // payload
} frame;

/* Wait for an event to happen; return its type in event, such as `cksum_eror`, `frame_arrival` */
void wait_for_event(event type *event);

/* Fetch a packet from the network layer for transmission on the channel. */
void from_network_layer(packet *p);

/* Deliver information from an inbound frame to the network layer. */
void to_network_layer(packet *p);

/* Go get an inbound frame from the physical layer and copy it to r. */
void from_physical_layer(frame *r);

/* Pass the frame to the physical layer for transmission. */
void to_physical_layer(frame *s);

/* Start the clock running and enable the timeout event. */
void start_timer(seq nr k);

/* Stop the clock and disable the timeout event. */
void stop_timer(seq nr k);

/* Start an auxiliary timer and enable the ack timeout event. */
void start_ack_timer(void);

/* Stop the auxiliary timer and disable the ack timeout event. */
void stop_ack_timer(void);

/* Allow the network layer to cause a network layer ready event. */
void enable_network_layer(void);

/* Forbid the network layer from causing a network layer ready event. * /
void disable_network_layer(void);

/* Macro inc is expanded in-line: increment k circularly. */
#define inc(k) if (k < MAX SEQ) k = k + 1; else k = 0

```

## A Utopian Simplex Protocol

```c
/* Protocol Utopia provides for data transmission in one direction only,
 from sender to receiver. The communication channel is assumed to be error 
 free and the receiver is assumec to be able to process all the input infinitely quickly
 infinite buffer space is available */
 
typedef enum { frame_arrival } event_type;

#include "protocol.h"


void sender1(void)
{
        frame s;
        packet buffer;

        while (true) {
                from_network_layer(&buffer);
                s.info = buffer;
                to_physical_layer(&s);
        }
}

void receiver1(void)
{
        frame r;
        event_type event;

        while (true) {
                wait_for_event(&event);
                from_physical_layer(&r);
                to_network_layer(&info);
        }
}
```

## A Simplex Stop-and-Wait Protocol for an Error-Free Channel

A general solution to flow control between senders and receivers is to have the receiver provide a feedback to the sender.  After having passed a packet to its network layer, the receiver sends a little dummy frame back to the sender which, in effect, gives the sender permission to transmit the next frame. A half-duplex physical channel would suffice here.

```
/* Protocol 2 Stop-and-Wait also provides for a one-directional flow of data from sender to receiver.
 The communication channel is once again assumed to be error free. However, this time the receiver
 has only a finite buffer capacity and a finite processing speed, so the protocol must explicitly
 prevent the sender from flooding the receiver with data faster than it can be handled */

typedef enum { frame_arrival } event_type;
#include "protocol.h"

void sender2(void)
{
        frame s;
        packet buffer;
        event_type event;

        while (true) {
                from_network_layer(&buffer);
                s.info = buffer;
                to_physical_layer(&s);
                wait_for_event(&event); // do not proceed until given the go ahead
        }
}

void receiver2(void)
{
        frame r, s;
        event_type event;
        while (true) {
                wait_for_event(&event);
                from_physical_layer(&r);
                to_network_layer(&r.info);
                to_physical_layer(&s); // send a dummy frame to awaken sender
        }
}
```

## A Simplex Stop-and-Wait Protocol for a Noisy Channel

Add a timer to sender for retransmitting a lost frame. A 1-bit sequence number is added to the frame to prevent duplicates.
