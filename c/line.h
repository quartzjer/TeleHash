
typedef struct line_struct
{
	char ipp[23]; // IP:PORT
	char iph[41]; // sha1 of ipp
	unsigned short ringout;
	unsigned int _line;
	unsigned int seenat;
	unsigned int sentat;
	unsigned int lineat;
	unsigned int _br;
	unsigned int brout;
	unsigned int brin;
	unsigned int bsent;
	short visible;
	char neighbors[6][41]; // array of nearby iph's
} *line;

// create+initialize
line line_new(char *ipp, int len);

// validate incoming line/ring values
int line_check(line l, int _line, int _ring);

// check+update line byte incoming status
int line_in(line l, int _br, int bin);

// check+update line byte outgoing status
int line_out(line l, int bout);