static char SCCSid[]="@(#)context.c	1.5 10/21/94";
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef  TRUE
#   define FALSE 0
#   define TRUE  1
#endif

main(argc,argv)
int	argc;
char ** argv;
{
FILE          * srcfh; 
char		buf[255];
char		name[40];
char 	      * pbuf;
char 	      * line;
int		lineno;
char	      * Start;
char	      * end;
char 	      * Place;
int		i;
int		j;
int		len;

FILE	      * infile;
int		NumKey;
#define		MAX_KEY_LEN 40
#define 	MAX_KEY	    100
char		Key[MAX_KEY][MAX_KEY_LEN];
int		LenKey[MAX_KEY];
int		Found;



   if (argc!=3) {
       printf("usage : %s sourcefile var[[,var]...]\n",argv[0]);
       return 1;
    }

    if ( (srcfh = fopen(argv[1],"r") ) == NULL ) {
	printf("Could not open %s\n",argv[1]);
	return 1;
    }

    pbuf = argv[2];

    NumKey = 0;
    while ( pbuf!=NULL) {
	Start = pbuf;
	if ( (pbuf = strchr(pbuf,',')) != NULL) {
	    *pbuf = '\0';
	    pbuf++;
	}
	LenKey[NumKey]=strlen(Start);
	if (LenKey[NumKey]>= MAX_KEY_LEN) {
	    printf("Variable name to long :%s:\n",Start);
	    return 1;
	}
	strcpy(Key[NumKey],Start);
	NumKey++;
	if (NumKey >= MAX_KEY) {
	    printf("Can not have more than %d variables in list.\n",MAX_KEY);
	    return 1;
	}
    }

   /* initialize the Current subroutine name to MAIN */

    name[0] = 'M';
    name[1] = 'A';
    name[2] = 'I';
    name[3] = 'N';
    name[4] = '\0';

    lineno = 0;
    fgets(buf,255,srcfh);
    while (!feof(srcfh)) {
	line = buf;
	lineno++;

	/* TAKE OUT COMMENT LINES */
	if (  (*line==' ') || (*line=='\t') || (isdigit(*line))  ) {
	    /* Get rid of starting white space */
	    for (; (*line==' ') || (*line=='\t') || isdigit(*line);line++);

	    /* CHANGE CURRENT SUBROUTINE IF APPROPRIATE */
	    if( strlen(line)>=10 && strncasecmp(line,"subroutine",10)==0  ) {
	
		for(line = line + 10;((*line==' ')||(*line=='\t') );++line);
		end = strchr(line,'(');
	        if (end) *end = '\0';
	        strcpy(name,line); 
	    }

	    /* GET RID OF COMMON STATEMENTS */
	    while(TRUE) {
	        if (  (strlen(line)>=6) && strncasecmp(line,"common",6)==0) {
		    while(TRUE) {
		        fgets(buf,255,srcfh); /* assume there is not endofile*/
			line = buf;	      /* immed after common */
			lineno++;
            	        if((strlen(line)>6) && (*(line+5) != ' ') ) {
                            if (!(  (*(line  ) == '\t' ) || (*(line+1) == '\t' ) ||
                                (*(line+2) == '\t' ) || (*(line+3) == '\t' ) ||
                            	(*(line+4) == '\t' ) || (*(line+5) == '\t' )
                   	    ) ) {
                    	    	/* This is a continuation line toss it */
			    }
			    else {
			     	break; /* Not a continuation line */
			    }
		    	}
		        else { /* Not a continuation line */
			    break;
			}
		    }
	    	    /* Get rid of starting white space */
	    	    for (; (*line==' ') || (*line=='\t') 
				|| isdigit(*line);line++);

		}
		else
		    break; /* Not a common Statement */
 	    } 

     	    /* REMOVE LEADING SPACES AND LABELS */
	    for(Start=line;((*Start==' ' )  ||
				(*Start=='\t')  ||
				(isdigit(*Start))   );++Start);
		
	    /* SEARCH FOR KEY VARIABLES */
	    for(i=0; i < NumKey; i++) {
		Found=FALSE;
	        if ( (Place = strstr(Start,Key[i])) != NULL) {
		    if (Place == Start)  {
			if( ! isalnum( *(Place+LenKey[i]) ) )
				Found = TRUE;
		    }
		    else   {
			if ( (! isalnum( *(Place-1         )))  &&
			         (! isalnum( *(Place+LenKey[i] ))) 
			       )
			    Found = TRUE;
		    }   
			
		    if( Found == TRUE ) {	
			printf("%s",Key[i]); 
	 		if ( (len = 10 - LenKey[i])  > 0 )
		   	    for(j=0; j < len; j++)
		      		putchar(' ');
			if ((pbuf=strrchr(argv[1],'/'))==NULL) pbuf=argv[1];
			else 				       pbuf++;
			printf("%s:%s",pbuf,name);
	 		if ( (len = 20 - (strlen(name)+strlen(pbuf)))  > 0 )
		   	    for(j=0; j < len; j++)
		      		putchar(' ');
			printf("%4d | %s",lineno,Start);
		    }
		}
	    }
	}
        fgets(buf,255,srcfh);
    }
}

