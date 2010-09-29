#include <stdio.h>
#include <string.h>

// make sure open/close always set
#define FIRSTY(len) if(len == 0){j[len] = '{';}else{if(j[len] == '}') j[len] = ',';} 

void jw_str(char *j, char *key, int klen, char *val, int vlen)
{
	int len,i;
	FIRSTY(strlen(j));
	len = strlen(j);
	len += sprintf(j+len,"\"%.*s\":\"",klen,key);
	// copy value while escaping
	for(i=0;i<vlen;i++)
	{
		if(val[i] == '"' || val[i] == '\\') j[len++]='\\';
		j[len++]=val[i];
	}
	j[len++]='"';
	j[len]='}';
}

void jw_int(char *j, char *key, int klen, int val)
{
	int len;
	FIRSTY(strlen(j));
	sprintf(j+strlen(j),"\"%.*s\":%d}",klen,key,val);
}

void jw_raw(char *j, char *key, int klen, char *raw, int rlen)
{
	int len;
	FIRSTY(strlen(j));
	sprintf(j+strlen(j),"\"%.*s\":%.*s}",klen,key,rlen,raw);
}

void jw_rawstr(char *j, char *key, int klen, char *rawstr, int rlen)
{
	int len;
	FIRSTY(strlen(j));
	sprintf(j+strlen(j),"\"%.*s\":\"%.*s\"}",klen,key,rlen,rawstr);
}

