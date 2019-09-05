#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUF_SIZE 4096
#define SERVER_PORT 12345

int main(int argc, char *argv[])
{
        int c, s, bytes;
        char buf[BUF_SIZE];
        struct hostent *server;
        struct sockaddr_in channel;

        if (argc != 3) {
                printf("Usage: client server_IP filename\n");
                exit(1);
        }
        s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        if (s < 0) {
                perror("Failed to create a socket!\n");
                exit(2);
        }
        memset(&channel, 0, sizeof(channel));
        channel.sin_family = AF_INET;
        inet_pton(AF_INET, argv[1], &channel.sin_addr);
        channel.sin_port = htons(SERVER_PORT);

        if (connect(s, &channel, sizeof(channel)) < 0) {
                perror("Failed to connect!\n");
                exit(2);
        }
        write(s, argv[2], strlen(argv[2])+1);

        while (1) {
                bytes = read(s, buf, BUF_SIZE);
                if (bytes < 0) exit(0);
                write(1, buf, bytes);
        }

        return 0;
}
