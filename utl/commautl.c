#include <string.h>
/*------------------------------------------------------------------------
							GetValStr()
	Places characters until a ',' into a 
	dest string and returns the src string
	after the ','
*/
char * 
GetValStr(char * Src, char * Dest) 
{

	char * commaPos;

	for (;*Src!='\0' && *Src!=',';Src++) {
	     *Dest=*Src;
	     Dest++;
	}
	*Dest='\0';
	if ( (commaPos = strchr(Src,',')) == NULL) {
		return commaPos;
	}
	return commaPos+1;
}
/*-------------------------------------------------------------------------
							GetStrStr()
*/
char *
GetStrStr(char * Src, char *Dest)
{

	char * quotePos;
	char * ptemp;

	/*--------
	 * Find First Double Quote */
	if ( (quotePos = strchr(Src,'"')) == NULL) {
	    Dest[0]='\0';
	}
	else {
	    /*------
	     * Find closing Quote */

	    quotePos++;
	    if ( strchr(quotePos,'"') == NULL) {
		Dest[0] = '\0';
	    }
	    else {
		for(;*quotePos!='"';quotePos++) {
		    *Dest=*quotePos;
		    Dest++;

		}
		*Dest='\0';

	    }
	}

	/*----------
	 * return the string after the comma if one
	 * exists, else return NULL;
	 */
	if ( (quotePos = strchr(Src,',')) == NULL) {
		return quotePos;
	}
	return quotePos+1;
}
