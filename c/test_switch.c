// thanks to http://www.cs.utsa.edu/~korkmaz/teaching/cn-resources/programs/capitalize-udp/client.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netdb.h>
#include "telex.h"
#include "jw.h"
#include "line.h"
#include "sha1.h"

#define bim 20										// bucket index max
char		buff[2048], istr[41], seestr[1024], sees[20][25], bkts[bim][25], bkthash[bim][41];
int		n, sock, si = 0, bi;
unsigned long	nph[4], bh4[bim][4];
struct		sockaddr_in sad;
struct		hostent *ptrh;
line		seed = NULL;
unsigned short	js[1024];

int js0n(unsigned char *js, unsigned int len, unsigned short *out);

void errhand( char *msg, int erno )							// general error handler (abort routine)
{	fprintf( stderr, "%s Error: %d.\n", msg, erno );				// give some reason for aborting.
	close( sock );									// close socket if one opened.
	exit( 1 );									// abort with error.
}

int dosha1( char *argv )								// compute 20 byte sha1 hash digest & 4ulong numeric.
{	SHA1Context sha;
	int		i, j, err;
	uint8_t		Message_Digest[20];
	char		aph[11];
	strcpy( istr, argv );
	if ( err = SHA1Reset(&sha) )							errhand( "SHA1Reset", err );
	if ( err = SHA1Input( &sha, (const unsigned char *)istr, strlen(istr) ) )	errhand( "SHA1Input", err );
	if ( err = SHA1Result( &sha, Message_Digest ) )					errhand( "SHA1Result", err );
	for(i = 0; i < 20 ; ++i)							// we have 20 bytes.
	{	sprintf(&istr[i*2], "%02x", Message_Digest[i]);				// store in string.
		nph[i/5] = ((nph[i/5] << 8) + Message_Digest[i])&0xffffffffff;		// and as 4 part ulong numeric.
	}
	return( 0 );									// return success.
}

int newdest( char *ipap )								// set-up for a new destination to send to.
{	char ip[16], prt[6];
	strcpy( ip, strtok( ipap, ":" ));						// IP portion
	strcpy( prt, strtok( NULL, ":" ));						// port # (still string)
	close(sock);									// reset for a new socket.
	if (( sock = socket(PF_INET, SOCK_DGRAM, 0) ) < 0 )	errhand( "socket creation", errno );
	memset((char *)&sad, 0, sizeof(sad));						// clear space
	sad.sin_family = AF_INET;							// type is INET
	sad.sin_port = htons((u_short)atoi(prt));					// set the port.
	if ( (char *)(ptrh = gethostbyname(ip)) == NULL )	errhand( "gethostbyname() invalid host", errno );
	memcpy( &sad.sin_addr, ptrh->h_addr, ptrh->h_length);				// address (IP)
	seed = line_new( ipap, strlen(ipap) );						// create line
	printf( "Attempting %s on port %s.\n", ip, prt );
	return( 0 );
}

void intoBuckets(int h)
{	int	d, e, f, g=bi-1, mf, obi;
	obi = bi;
	mf = 0;										// matchfound flag (also ialready inserted).
// before looking for an insert point, check if it can be appended.
	if (( nph[0] > bh4[g][0] ) | (( nph[0] == bh4[g][0] ) && (( nph[1] > bh4[g][1] ) | (( nph[1] == bh4[g][1] ) && (( nph[2] > bh4[g][2] ) | (( nph[2] == bh4[g][2] ) && ( nph[3] > bh4[g][3] )))))))
	{	if ( bi == bim )	bi--;
		strcpy( bkthash[bi], istr );
		strcpy( bkts[bi], sees[h] );
		for ( g=0; g<4; g++ )
			bh4[bi][g] = nph[g];
		bi++;
	}else
	for ( g=0; g<bi; g++ )								// find in bucket or place to insert.
	{	for ( f=0; f<4; f++ )							// 4 parts of numeric hash to check.
		{	if ( nph[f] < bh4[g][f] )					// found insert point.
			{	for ( e=bi-1; e>=g; e-- )
				{	if ( e == bim-1 )
						continue;
					strcpy( bkts[e+1], bkts[e] );
					strcpy( bkthash[e+1], bkthash[e] );
					for ( d=0; d<4; d++ )
						bh4[e+1][d] = bh4[e][d];
				}
				strcpy( bkts[g], sees[h] );
				strcpy( bkthash[g], istr );
				for ( e=0; e<4; e++ )
					bh4[g][e] = nph[e];
				bi++;
				mf = 1;
				break;							// no need to look further in f. Next g
			}else								// nph[f] must be > or = bh4[g][f]
			{	if ( nph[f] == bh4[g][f] )
				{	if ( f == 3 )					// if f=3 then match found.
					{	mf=1;	break;				// no need to look further in f. Next g
					}
				}
				else break;
			}
		}
		if ( mf == 1 )	break;
	}
	if ( bi != obi )
	{	printf( "%d items in buckets.\n", bi );
		for ( g=0; g<bi; g++ )
			printf( "bucket[%d]=%s   hash=%s  %010lx  %010lx  %010lx  %010lx\n", g, bkts[g], bkthash[g], bh4[g][0], bh4[g][1], bh4[g][2], bh4[g][3] );
	}
}

void moresees( char *seestr )								// process 'see' string.
{	char	tipas[25], *tptr = NULL, *next = NULL;
	int	h;
// first parse all of the sees for IP:port strings.
	if ( strlen( seestr ) > 10 )							// verify that there might be some there.
	{	printf( "seestr = %s\n", seestr );
		tptr = strtok( seestr, "\"" );						// find first '"' delimeter.
		while ( tptr[0] != ']' )						// watch for closing bracket (safer than waiting for null).
		{	strcpy( sees[si++], strtok( NULL, "\"" ));			// get 'sees' IP:port combination.
			tptr = strtok( NULL, "\"" );					// ignore ',' separater, but might be closing bracket.
		}
// now check against our buckets for any new ones.
		for ( h=0; h<si; h++ )							// for each 'see' string in our list
		{	strcpy( tipas, sees[h] );
			dosha1( tipas );						// create (string) hash digest
			intoBuckets( h );
		}
	}
	si = 0;
}

void transRecv()				// general Transmit and receive.
{	printf( "Sending: %s\n", buff );
	n = sendto( sock, buff, strlen(buff), 0, (struct sockaddr *) &sad, sizeof(struct sockaddr) );
	bzero( buff, sizeof(buff) );
	n = read( sock, buff, sizeof(buff) );  
	printf( "Response: %s\n", buff );
	if ( js0n( buff, n, js ))	errhand( "js0n() parse failed", errno );
	strncpy( seestr, buff+js[6], js[7] );
	moresees( seestr );
}

main(int argc, char *argv[])
{	char		*host = "telehash.org";
	int		port = 42424;
	char		localip[17], ripa[25], idstr[41], thoid[41], mytid[41];
	int		i, j, ix;
	line		self = NULL;

// first let's figure out our local IP
	if (( sock = socket(PF_INET, SOCK_DGRAM, 0) ) < 0 )	errhand( "socket creation", errno );
	memset((char *)&sad,0,sizeof(sad));
	sad.sin_family = AF_INET;
	sad.sin_port = htons((u_short)port);
	ptrh = gethostbyname("42.42.42.42"); // dummy just to trick connect to tell us default outgoing IP
	memcpy(&sad.sin_addr, ptrh->h_addr, ptrh->h_length);
	connect(sock,(struct sockaddr *)&sad,sizeof(struct sockaddr));
	i = sizeof(struct sockaddr);
	if (getsockname(sock, (struct sockaddr *)&sad, &i) == -1)	errhand( "getsockname()", errno );
	sprintf(localip,"%s",inet_ntoa(sad.sin_addr));
	sprintf( sees[0], "%s:%d", host, port );
	newdest( sees[0] );

	sprintf(buff,"%s:%d",inet_ntoa(sad.sin_addr),port);
	dosha1( buff );
	strcpy( thoid, istr );
  
	bzero(buff,sizeof(buff));
	jw_str(buff,"+end",4,thoid,40);
	jw_int(buff,"_ring",5,seed->ringout);
	transRecv();	// Transmit and Receive
	
	for ( i=0; js[i]; i+=4 )
	{
		printf("%.*s\t%.*s\n",js[i+1],buff+js[i],js[i+3],buff+js[i+2]);
		if(strncmp("_to",buff+js[i],3)==0)	self = line_new(buff+js[i+2],js[i+3]);
		if(strncmp("_ring",buff+js[i],5)==0)	line_check(seed,0,atoi(buff+js[i+2]));
		if(strncmp("_line",buff+js[i],5)==0 && line_check(seed,atoi(buff+js[i+2]),0))	errhand( "seed line failed", errno );
	}
	if(!self)	errhand( "no _to??", 0 );
	if(self) printf("SELF\tlocal ip %s\tremote ipp %s\tour +end=%s\n",localip,self->ipp,self->iph);
	strcpy( ripa, self->ipp );	printf( "ripa=%s\n", ripa );
	i = dosha1( ripa );
	if ( i == 0 ) printf( "istr = %s\n", istr );
	strcpy( mytid, istr );

// send as ourselves now
	bzero(buff,sizeof(buff));
	jw_str(buff,"+end",4,self->iph,40);
	jw_int(buff,"_line",5,seed->_line);
	transRecv();	// Transmit and Receive

	printf( "*************************************\n" );
	printf( "*                                   *\n" );
	printf( "*  send to everyone in my buckets.  *\n" );
	printf( "*                                   *\n" );
	printf( "*************************************\n" );
	for ( i=0; i<bi; i++ )
	{	newdest( bkts[i] );
		bzero(buff,sizeof(buff));
		jw_str(buff,"+end",4,self->iph,40);
		jw_int(buff,"_ring",5,seed->ringout);
		transRecv();
	}
	close(sock);
}
