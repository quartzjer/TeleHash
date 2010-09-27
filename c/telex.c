

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

bool telex_init(telex t, char *json, int len);

// replace/set values (for outgoing)
void telex_setc(telex t, char *key, char *val);
void telex_seti(telex t, char *key, unsigned int i);

// copy all +signals from in to out
void telex_signals(telex in, telex out);

// serialize to json and return
char *telex_json(telex t);
int telex_jlen(telex t);