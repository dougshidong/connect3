#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/time.h>

void send_init(int, char *);

#define BUF_SIZE 100

void openconnection(int gameid, int player, int *sock)
{
    struct sockaddr_in serveraddr;
    struct hostent *hp, *gethostbyname();
    char buf[BUF_SIZE];
    char color[5];
    char serverIP[14] = "132.206.74.211";
    int serverPort = 12345;

    *sock = socket(AF_INET, SOCK_STREAM, 0);
    if(*sock < 0)
    {
        fprintf(stderr, "Error opening socket");
        exit(1);
    }

    serveraddr.sin_family = AF_INET;
    serveraddr.sin_port = htons(serverPort);

    if ((hp = gethostbyname(serverIP)) == 0)
    {
        fprintf(stderr, "unknown host: %s\n", serverIP);
        exit(1);
    }
    memcpy(&serveraddr.sin_addr, hp->h_addr, hp->h_length);

    fprintf (stderr, "Connecting to %s on port %d (TCP)\n", serverIP, serverPort);
    if (connect(*sock, (struct sockaddr *)&serveraddr, sizeof(serveraddr)) < 0)
    {
        fprintf(stderr, "Error connecting stream socket");
        exit(1);
    }
    if(player == 1) sprintf(color,"white");
    else sprintf(color,"black");

    sprintf (buf, "game%d %s\n", gameid, color);

    send_init(*sock, buf);
}
void send_init(int s, char *b)
{
    int nread;
    char buf[BUF_SIZE];

    sprintf(buf, "%s", b);
    write(s, buf, strlen(buf));
    if ((nread = read(s, buf, BUF_SIZE)) < 0) {
      fprintf(stderr, "reading from socket");
      exit (1);
    }
    strtok(buf, "\n");
    fprintf(stderr, "Server reply: %s\n", buf);
}
void send3(int s, int pos, int dir)
{
    int nread;
    char buf[BUF_SIZE];
    memset(&buf[0], 0, sizeof(buf));

    switch(dir)
    {
        case 1: sprintf(buf, "%dN\n", pos); break;
        case 2: sprintf(buf, "%dE\n", pos); break;
        case 3: sprintf(buf, "%dS\n", pos); break;
        case 4: sprintf(buf, "%dW\n", pos); break;
        default: fprintf(stderr, "Invalid send direction %d\n", dir); break;
    }
    fprintf(stderr, "Sending: %s", buf);
    write(s, buf, strlen(buf));

    nread = read(s, buf, BUF_SIZE);

    if(nread < 0) {
        fprintf(stderr, "Error reading reply");
        exit (1);
    }
}
void receive3(int s, int *pos, int *dir)
{
    int nread;
    char buf[BUF_SIZE];
    char *cdir;
    memset(&buf[0], 0, sizeof(buf));
    if ((nread = read(s, buf, BUF_SIZE)) < 0) {
      fprintf(stderr, "Error reading move");
      exit (1);
    }
    fprintf(stderr, "Received move from server: %s\n", buf);
    char *newline = strchr(buf,'\n');
    if(newline) *newline = 0;
    sscanf(buf, "%d%c", pos, cdir);
    switch(*cdir)
    {
        case 'N': *dir = 1; break;
        case 'E': *dir = 2; break;
        case 'S': *dir = 3; break;
        case 'W': *dir = 4; break;
        default : fprintf(stderr, "Invalid receive direction\n"); exit(1); break;
    }
}
