// thanks to http://www.cs.utsa.edu/~korkmaz/teaching/cn-resources/programs/capitalize-udp/client.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

main(int argc, char *argv[])
{ 
	struct sockaddr_in sad;
	int sock;
	struct hostent *ptrh;
	char *host = "telehash.org";
	int port = 42424;
	char *hello = "{'+end':'0eb2ad19a7b508cc09b2d52b4a506845db39fae2'}";
	char buff[2048];
	int n;
  
	sock = socket(PF_INET, SOCK_DGRAM, 0);
	if (sock < 0) {
		fprintf(stderr, "socket creation failed\n");
		exit(1);
	}
 
	memset((char *)&sad,0,sizeof(sad));
	sad.sin_family = AF_INET;
	sad.sin_port = htons((u_short)port);
	ptrh = gethostbyname(host);
	if ( ((char *)ptrh) == NULL ) {
		fprintf(stderr,"invalid host: %s\n", host);
		exit(1);
	}
	memcpy(&sad.sin_addr, ptrh->h_addr, ptrh->h_length);
  
	n=sendto(sock, hello, strlen(hello),0,(struct sockaddr *) &sad, sizeof(struct sockaddr));
	memset((char*)&buff,0,sizeof(buff));
	n=read(sock, buff, sizeof(buff));
  
	printf("Response: %s\n",buff);
	close(sock);
}


