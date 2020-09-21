using GLib;

struct NtpPacket
{
    uint8 li_vn_mode;
    uint8 stratum;
    uint8 poll;
    uint8 precision;

    uint32 rootDelay;
    uint32 rootDispersion;

    uint32 refId;
    uint32 refTm_s;
    uint32 refTm_f;

    uint32 origTm_s;
    uint32 origTm_f;

    uint32 rxTm_s;
    uint32 rxTm_f;
    uint32 txTm_s;
    uint32 txTm_f;
}

const string hostname = "ntp.pool.org";
const uint16 portno = 123;

int main(string[] args)
{
    var packet = NtpPacket();
    packet.li_vn_mode = 0x1b;
    assert(sizeof(NtpPacket) == 48);

    // resolve the server name
    unowned Posix.HostEnt server = Posix.gethostbyname(hostname); // a pointer to internal storage
    if (server == null) {
        error(@"Cannot resolve host $hostname: $(Posix.errno)");
    }
    print(@"Found $(server.h_addr_list.length) IP address(es) for $hostname");

    // construct the socket address
    var address = Posix.SockAddrIn();
    address.sin_family = Posix.AF_INET;
    address.sin_port = Posix.htons(portno);
    Posix.memcpy(&address.sin_addr, server.h_addr_list[0], server.h_length);

    var stringAddress = Posix.inet_ntoa(address.sin_addr);
    print(@"Using $hostname IP address $stringAddress\n");

    // create a socket
    var sockfd = Posix.socket(Posix.AF_INET, Posix.SOCK_DGRAM, Posix.IPProto.UDP);
    if (sockfd < 0) {
        error(@"Cannot create socket: $(Posix.errno)\n");
    }

    // connect to the server
    var ok = Posix.connect(sockfd, address, sizeof(Posix.SockAddrIn));
    if (ok < 0) {
        error(@"Cannot connect to $(Posix.errno)\n");
    }

    // send the request packet
    var written = Posix.write(sockfd, &packet, sizeof(NtpPacket));
    if (written < 0)
        error(@"cannot send UDP packet: $(Posix.errno)\n");

    // wait for an answer
    var received = Posix.read(sockfd, &packet, sizeof(NtpPacket));
    if (received < 0)
        error(@"cannot read from socket: $(Posix.errno)\n");

    packet.txTm_f = Posix.ntohl(packet.txTm_f);
    packet.txTm_s = Posix.ntohl(packet.txTm_s);
    const uint64 NTP_TIMESTAMP_DELTA = 2208988800ull;
    time_t txTm = (time_t) (packet.txTm_s - NTP_TIMESTAMP_DELTA);

    var str = Posix.ctime(ref txTm);
    print(@"Curent UTC time is $str");

    return 0;
}
