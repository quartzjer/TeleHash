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
	l->ringout = (unsigned short)(rand() % 32768)+1; // 1 to 32768
	return l;
}

// validate incoming line/ring values, != 0 is bad
int line_check(line l, int _line, int _ring)
{
	if(!l->_line && !_line)
	{
		l->_line = _ring * l->ringout;
		return 0;
	}
	// need to fill in the logic here badly
	if(!l->_line && _line)
	{
		if(_line % l->ringout != 0) return 1; // didn't use the ring we sent in the product??
		l->_line = _line;
		return 0;
	}
	return 1;
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