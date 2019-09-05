#include <sys/types.h>
#include <sys/fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SERVER_PORT 12345
#define BUF_SIZE 4096
#define QUEUE_SIZE 10

int main(int argc, char *argv[])
{
        int s, b, l, fd, sa, bytes, on = 1;
        char buf[BUF_SIZE];
        struct sockaddr_in channel;

        if (argc < 2) {
                printf("Usage: server server_IP\n");
                exit(0);
        }

        /* build address structure to bind to socket */
        memset(&channel, 0, sizeof(channel));
        channel.sin_family = AF_INET;
        struct in_addr server_addr;
        if (inet_pton(AF_INET, argv[1], &server_addr) != 1) {
                perror("Failed to convert address\n");
                exit(1);
        }
        channel.sin_addr.s_addr = server_addr.s_addr;
        channel.sin_port = htons(SERVER_PORT);

        char rv_addr[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &channel.sin_addr, &rv_addr[0], sizeof(channel));
        printf("Server Address: %s, %x:%d\n", rv_addr,
               ntohl(channel.sin_addr.s_addr), ntohs(channel.sin_port));
        // passive open, Waiting for connection
        s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        if (s < 0) {
                perror("failed to create the socket\n");
                exit(2);
        }
        setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char*)&on, sizeof(on));
        int be = bind(s, (struct sockaddr *)&channel, sizeof(channel));
        if (be < 0) {
                perror("bind error\n");
                exit(3);
        }
        if (listen(s, QUEUE_SIZE) < 0) {
                perror("listen failed\n");
                exit(4);
        }

        while (1) {
                sa = accept(s, 0, 0);
                if (sa < 0) {
                        perror("accept failed");
                        exit(5);
                }
                read(sa, buf, BUF_SIZE);
                printf("%d\n", buf[strlen(buf)-1]);
                //buf[strlen(buf)-1] = '\0';
                printf("opening file %s, %d\n", buf, strlen(buf));
                fd = open(buf, O_RDONLY);
                if (fd < 0) {
                        perror("open failed");
                        exit(6);
                }

                while (1) {
                        bytes = read(fd, buf, BUF_SIZE);
                        if (bytes <= 0) break;
                        write(sa, buf, bytes);
                }
                close(fd);
                close(sa);
        }
        return 0;
}
