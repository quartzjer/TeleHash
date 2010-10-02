#include "line.h"
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <stdio.h>
#include <openssl/sha.h>

// create+initialize
line line_new(char *ipp, int len)
{
	line l;
	unsigned char sha1bin[20];
	int i;
	
	l = malloc(sizeof(struct line_struct));
	bzero(l,sizeof(struct line_struct));
	memcpy(l->ipp,ipp,len);
	SHA1(ipp, len, sha1bin);
	for (i = 0; i < 20; i++) sprintf(l->iph+(i*2),"%02x", sha1bin[i]);
	return l;
}

// validate incoming line/ring values
int line_check(line l, int _line, int _ring)
{
	return 0;
}

// check+update line byte incoming status
int line_in(line l, int _br, int bin)
{
	return 0;
}

// check+update line byte outgoing status
int line_out(line l, int bout)
{
	return 0;
}