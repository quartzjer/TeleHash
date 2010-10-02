// thanks to http://www.cs.utsa.edu/~korkmaz/teaching/cn-resources/programs/capitalize-udp/client.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netdb.h>
#include "telex.h"
#include "jw.h"
#include "line.h"

int js0n(unsigned char *js, unsigned int len, unsigned short *out);

main(int argc, char *argv[])
{ 
	struct sockaddr_in sad;
	int sock;
	struct hostent *ptrh;
	char *host = "telehash.org";
	int port = 42424;
	char buff[2048],localip[17];
	int n,i,j;
	unsigned short js[1024];
	line self = NULL;
	
	// first let's figure out our local IP
	sock = socket(PF_INET, SOCK_DGRAM, 0);
	if (sock < 0) {
		fprintf(stderr, "socket creation failed\n");
		exit(1);
	}
	memset((char *)&sad,0,sizeof(sad));
	sad.sin_family = AF_INET;
	sad.sin_port = htons((u_short)port);
	ptrh = gethostbyname("42.42.42.42"); // dummy just to trick connect to tell us default outgoing IP
	memcpy(&sad.sin_addr, ptrh->h_addr, ptrh->h_length);
	connect(sock,(struct sockaddr *)&sad,sizeof(struct sockaddr));
	i = sizeof(struct sockaddr);
	if (getsockname(sock, (struct sockaddr *)&sad, &i) == -1)
	{
		fprintf(stderr,"getsockname() for local ip lookup failed");
		exit(1);
	}
	sprintf(localip,"%s",inet_ntoa(sad.sin_addr));
	

	// reset for our real socket
	close(sock);
	sock = socket(PF_INET, SOCK_DGRAM, 0);
	if (sock < 0) {
		fprintf(stderr, "socket creation failed\n");
		exit(1);
	}	

	// our seed
	memset((char *)&sad,0,sizeof(sad));
	sad.sin_family = AF_INET;
	sad.sin_port = htons((u_short)port);
	ptrh = gethostbyname(host);
	if ( ((char *)ptrh) == NULL ) {
		fprintf(stderr,"invalid host: %s\n", host);
		exit(1);
	}
	memcpy(&sad.sin_addr, ptrh->h_addr, ptrh->h_length);
  
	bzero(buff,sizeof(buff));
	jw_str(buff,"+end",4,"0eb2ad19a7b508cc09b2d52b4a506845db39fae2",40);
	printf("Sending: %s\n",buff);
	n=sendto(sock, buff, strlen(buff),0,(struct sockaddr *) &sad, sizeof(struct sockaddr));
	bzero(buff,sizeof(buff));
	n=read(sock, buff, sizeof(buff));
  
	printf("Response: %s\n",buff);
	
	if(js0n(buff,n,js))
	{
		printf("parse failed :(\n");
		close(sock);
		exit(1);
	}

	for(i=0;js[i];i+=4)
	{
		printf("%.*s\t%.*s\n",js[i+1],buff+js[i],js[i+3],buff+js[i+2]);
		if(strncmp("_to",buff+js[i],3)==0) self = line_new(buff+js[i+2],js[i+3]);
	}
	if(!self)
	{
		printf("no _to??\n");
		close(sock);
		exit(1);
		
	}
	if(self) printf("SELF\tlocal ip %s\tremote ipp %s\tour +end=%s\n",localip,self->ipp,self->iph);

	close(sock);
}


