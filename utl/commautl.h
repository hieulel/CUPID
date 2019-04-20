/*------------------------------------------------------------------------
							GetValStr()
	Places characters until a ',' into a 
	dest string and returns the src string
	after the ','. If no ',', returns NULL.
*/
char * 
GetValStr(char * Src, char * Dest);

/*------------------------------------------------------------------------
							GetStrStr()
	Places characters Inside enclosing '"'s and
	returns the string after the ',', if it exist,
	else returns NULL
*/
char * 
GetStrStr(char * Src, char * Dest);

