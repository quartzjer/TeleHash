// json writer utils
// - these always assume *j is big enough, caller beware!
// - very simple, will write/append whatever to any json in *j

// append a string value (escaping the value)
void jw_str(char *j, char *key, int klen, char *val, int vlen);

// append an integer
void jw_int(char *j, char *key, int klen, int val);

// append a raw value
void jw_raw(char *j, char *key, int klen, char *raw, int rlen);

// raw string (don't want it escaped)
void jw_rawstr(char *j, char *key, int klen, char *rawstr, int rlen);
