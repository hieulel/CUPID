static char SCCSid[]="@(#)cupdiff.c	1.3 10/03/94";
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

void
ProcessLine(FILE * fho,char * line1,char * line2,float PcntTol,char * heading,int line)
{
    int			len;
    char		strVal1[8];
    char		strVal2[8];
    float		val1,val2;
    int			noval;
    int			offset;
    int			i,j;
    int			DiffFound;

    if (strcmp(line1,line2)==0) return;

    len = strlen(line1);
    if (len != strlen(line2)) {
	fprintf(fho,"ERROR: lines are not the same length, for line %d\n",line);
	exit(1);
    }
    noval = (len -14)/7;
    offset = 14;
    strVal1[7]='\0';
    strVal2[7]='\0';
    DiffFound=0;
    for(i=0;i<noval;i++) {
	for(j=0;j<7;j++) {
	    strVal1[j] = line1[offset+i*7+j];
	    strVal2[j] = line2[offset+i*7+j];
	}
	val1 = atof(strVal1);
	val2 = atof(strVal2);
	if (!(val1==0 && val2==0)) {
	    if ( (val1==0) || (val2==0) ) {
	    	if (strcmp(strVal1,strVal2)!=0) {
		    DiffFound=i+1;
	    	}
	    }
	    else {
	    	if (   (((val1-val2)/val2)*100 > PcntTol) 
	   	    || (((val2-val1)/val2)*100 > PcntTol) 
	  		)  {
	            DiffFound=i+1;
	    	}
	    }
	}
	if (DiffFound!=0) {
	    fprintf(fho,"--%s",heading);
	    fprintf(fho,"< %s",line1);
	    fprintf(fho,"> %s",line2);
	    fprintf(fho,"  Val %d diff is %f\n",i+1, (val1-val2)/val2*100);
	    return;
	}
    }
}

main(int argc, char * argv[])
{

    FILE * 		fh1;
    FILE *		fh2;
    FILE *		fho;
    float		PcntTol;
    char		line1[255];
    char		line2[255];
    char		head1[255];
    char		head2[255];
    int			line;

    if (argc != 5) {
	printf("usage : %s cupidfile1 cupidfile2 diffoutfile PcntTolerance\n",argv[0]);
	return 0;
    }
    
    if ( (fh1=fopen(argv[1],"r") ) == (FILE *)NULL ) {
	printf("Could not open %s for reading.\n",argv[1]);
	return 0;
    }
    if ( (fh2=fopen(argv[2],"r") ) == (FILE *)NULL ) {
	printf("Could not open %s for reading.\n",argv[2]);
	return 0;
    }
    if ( (fho=fopen(argv[3],"w") ) == (FILE *)NULL ) {
	printf("Could not open %s for writing.\n",argv[3]);
	return 0;
    }
    if ( (PcntTol=atof(argv[4])) <= 0.0) {
	printf("Percent Tolerance must be greater than 0.0\n");
	return 0;
    }

    fprintf(fho,"DIFF between %s and %s for values differing by more than %s Percent\n\n",argv[1],argv[2],argv[4]);
    fprintf(fho,"%s : First 2 Lines\n",argv[1]);
    fgets(line1,255,fh1);
    fprintf(fho,"<  %s",line1);
    fgets(line1,255,fh1);
    fprintf(fho,"<  %s\n",line1);

    fprintf(fho,"%s : First 2 Lines\n",argv[2]);
    fgets(line2,255,fh2);
    fprintf(fho,">  %s",line2);
    fgets(line2,255,fh2);
    fprintf(fho,">  %s\n",line2);


    fgets(line1,255,fh1);
    fgets(line2,255,fh2);
    line = 3;
    while((!feof(fh1)) && (!feof(fh2)) ) {

	if (line1[1] == '1') {
	    if (line2[1] != '1') {
		fprintf(fho,"ERROR: Files are not synced on line %d\n",line);
		return 0;
	    }    
	    fgets(head1,255,fh1);
	    fgets(head2,255,fh2);
	    line++;
	}
	else if (line1[1] == '2') {
	    if (line2[1] != '2') {
		fprintf(fho,"ERROR: Files are not synced on line %d\n",line);
		return 0;
	    }
	    ProcessLine(fho,line1,line2,PcntTol,head1,line);
	}
	else {
	    fprintf(fho,"ERROR: Not 1 or 2 in second column of line %d\n",line);
	    return 0;
 	}
        fgets(line1,255,fh1);
        fgets(line2,255,fh2);
        line++;
	
    }
    if  (feof(fh1) && (!feof(fh2)) ) {
	fprintf(fho, "ERROR: File %s has more lines than File %s\n",argv[1],argv[2]);
	exit(1);
    }
    else if ((!feof(fh1)) && feof(fh2)) {
	fprintf(fho, "ERROR: File %s has fewer lines than File %s\n",argv[1],argv[2]);
	exit(1);
    }
    return 0;
}

