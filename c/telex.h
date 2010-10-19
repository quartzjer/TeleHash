

// this telex type and functions are designed to be re-used (you usually only need a few around)
typedef struct telex_struct
{
	// where all the raw data is at
	char buff[4096];
	unsigned short bat;
	// offsets into the raw data
	unsigned short js[4096];
	
	// well known values extracted
	unsigned int _line;
	unsigned short _ring;
	unsigned int _br;
	short _hop;
	// these always point into buff
	char *end;
	char *to;
} *telex;

void telex_init(telex t, char *json, int len);

// copy all +signals from in to out
void telex_signals(telex in, char *out);
