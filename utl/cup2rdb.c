#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <awserror.h>
#include <commautl.h>

#define MAXFLOAT      ((float)3.40282346638528860e+38)

#define PROCESS_CONTINUE 1111

#ifndef TRUE
#   define TRUE  1
#   define FALSE 0
#endif

#define MAX_RDB_FLD_NAME_LEN 50
#define MAX_CUPID_RDB_FLD 300

typedef struct FieldTypeTag {
    char name[MAX_RDB_FLD_NAME_LEN];
    int maj;
    int min;
    int pos;
    int angcls;
    int lyr;
} FieldType;

typedef struct RDBRecTypeTag {
    long idx;
    int doy;
    int ts;
    int angcls;
    int lyr;
    float * fld;
    struct RDBRecTypeTag * next;
} RDBRecType;


/*--------------------
 * bit 1 (2) LYR
 * bit 2 (4) ANGCLS
 * bit 3 (8) TIME
 * bit 4 (16) DOY
 */
#define DOYTIME 24
#define DOYTIMELYR 26
#define DOYTIMEANGCLS 28
#define DOYTIMELYRANGCLS 30

typedef struct RDBFileTypeTag {
    int UniqueIdx;
    int NoOfFld;
    FieldType * FldList;
    RDBRecType * head;
} RDBFileType; 

int InitRDBFile(RDBFileType * pRDBF, FILE * FLfh);
RDBRecType * CreateRDBRec(RDBFileType * pRDBF
			,int doy,int ts,int lyr, int angcls);
RDBRecType * FindRDBRec(RDBFileType * pRDBF
			,int doy,int ts,int lyr,int angcls);
int AddValToRDBF(RDBFileType * pRDBF,int doy, int ts, int lyr, int angcls,
		 int fldidx, float val); 
int ProcessLine(RDBFileType * pRDBF,char * line);
int ProcessLineRecurse(RDBFileType * pRDBF,char * line,int * pidx);
int PrintRDB(RDBFileType * pRDBF);
/*--------------------------------------------------------------------------
 *                                                            InitRDBFile()
 */
int 
InitRDBFile(RDBFileType * pRDBF, FILE * FLfh)
{

    char line[2048];
    char * pline;
    char buf[2048];
    char Sub[] = "Cup2RDB:InitRDBFile";
    int idx,doy,ts,lyr,angcls;
    int maj,min,pos;
    int nooffld;

    /*-------------------
     * First line has the folowing NoOfFlds
     * followed by for 0 or 1 values that determine
     * which of the cupid index fields constitute 
     * part of a unique index.
     */
    fgets(line,2048,FLfh);

    while ((line[0]=='#' || strlen(line)<5) && !feof(FLfh) ) 
	fgets(line,2048,FLfh);
    
    if (feof(FLfh)) {
	RegErr(Sub,"","No First Line");
        return AWS_FAIL;
    }
    sscanf(line,"%d,%d,%d,%d,%d",&(pRDBF->NoOfFld),&doy,&ts ,&angcls,&lyr);
    idx = doy*16+ts*8+angcls*4+lyr*2;
    if (pRDBF->NoOfFld<1 || pRDBF->NoOfFld>MAX_CUPID_RDB_FLD) {
	RegErr(Sub,"NoOfFlds should be first parameter",line);
	return AWS_FAIL;
    }
    if ( ! (  (idx==DOYTIME) || (idx==DOYTIMELYR) || (idx==DOYTIMEANGCLS)
	    ||(idx==DOYTIMELYRANGCLS) ) ) {
	RegErr(Sub,"Unknown Unique Index Combination",line);
	return AWS_FAIL;
    }
    pRDBF->UniqueIdx = idx;

    /*---------------
     * Init RDBRec head to null and allocate
     * the storage for fldlist.  fldlist is 
     * an array and the records are kept in an 
     * ordered singly linked list.
     */
    pRDBF->head = NULL;
    if ( (pRDBF->FldList = malloc(sizeof(FieldType) * pRDBF->NoOfFld) ) 
	           == NULL) {
	RegErr(Sub,"pRDBF->FldList",AWSERR_MALLOC);
	return AWS_FAIL;
    }
    
    /*--------------------
      * This is the main loop for the fields list.
      * there should be pRDBF field entries.  We check 
      * here to aid the user in getting this right.
      */
    nooffld = 0;
    while(fgets(line,2048,FLfh)) {
	/*-------
	 * Drop comment lines
	 */
	while ((line[0]=='#' || strlen(line)<5) && !feof(FLfh)) fgets(line,2048,FLfh);
	if (feof(FLfh) ) break;
	pline = line;
	/*-------
	 * Field name
	 */
	if ((pline=GetStrStr(pline,buf))==NULL) {
	    RegErr(Sub,"Error in field 1 or 2",line);
	    return AWS_FAIL;
	}
	if (strlen(buf)<1 || strlen(buf)>MAX_RDB_FLD_NAME_LEN) {
	    RegErr(Sub,"Field name is bad or too long",line);
	    return AWS_FAIL;
	}
	strcpy(pRDBF->FldList[nooffld].name,buf);
	
	/*-------
	 * maj
	 */
	if ((pline=GetValStr(pline,buf)) == NULL) {
	    RegErr(Sub,"Error in Field 2 or 3",line);
	    return AWS_FAIL;
	}
	maj = atoi(buf);
	if (maj < 1 || maj > 9) {
	    RegErr(Sub,"Major code < 1 or > 9",line);
	    return AWS_FAIL;
	}
	/*--------
	 * min
	 */
	if ((pline=GetValStr(pline,buf)) == NULL) {
	    RegErr(Sub,"Error in Fields 3 or 4",line);
	    return AWS_FAIL;
	}
	min = atoi(buf);	    
	if (min < 1 || min > 99 ) {
	    RegErr(Sub,"Minor Code < 1 or > 99", line);
	    return AWS_FAIL;
	}
	   
	/*--------
         * pos
         */
	if ((pline=GetValStr(pline,buf)) == NULL) {
	    RegErr(Sub,"Error in Fields 4 or 5",line);
	    return AWS_FAIL;
	}
	pos = atoi(buf);
	if (pos<1 || pos > 10) {
	    RegErr(Sub,"Position < 1 or > 10",line);
	    return AWS_FAIL;
	}

	/*--------
         * angcls
	 */
	if ((pline=GetValStr(pline,buf)) == NULL) {
	    RegErr(Sub,"Error in Fields 5 or 6",line);
	    return AWS_FAIL;
	}
	if (strlen(buf)<1) {
	    angcls = 0;
	}
	else {
	    angcls = atoi(buf);
	    if (angcls <0 || angcls >99) {
		RegErr(Sub,"Angle Class < 0 or > 99",line);
		return AWS_FAIL;
	    }
	}
	   
	/*--------
         * lyr
	 */
	if ((pline=GetValStr(pline,buf)) != NULL) {
	    RegErr(Sub,"Too many fields",line);
	    return AWS_FAIL;
	}
	if (strlen(buf)<1) {
	    lyr = 0;
	}
	else {
	    lyr = atoi(buf);
	    if (lyr < 0 || lyr > 99) {
		RegErr(Sub,"Layer  < 0 or > 99",line);
		return AWS_FAIL;
	    }
	}
	   
	pRDBF->FldList[nooffld].maj = maj;
	pRDBF->FldList[nooffld].min = min;
	pRDBF->FldList[nooffld].pos = pos;
	pRDBF->FldList[nooffld].angcls = angcls;
	pRDBF->FldList[nooffld].lyr = lyr;

	nooffld++;
    }
    if (nooffld!=pRDBF->NoOfFld) {
	sprintf(buf,"File Header said %d fields but %d were found"
		             ,pRDBF->NoOfFld,nooffld);
	RegErr(Sub,buf,line);
	return AWS_FAIL;
    }
    return AWS_OK;
}
/*--------------------------------------------------------------------------
 *                                                              FindRDBRec() 
 */
RDBRecType *
CreateRDBRec(RDBFileType * pRDBF,int doy,int ts,int angcls,int lyr)
{
    char Sub[] = "Cup2RDB:CreateRDBRec";
    RDBRecType * pRec;
    RDBRecType * pCur;
    RDBRecType * pPrev;
    long idx; 
    int i;

    if (pRDBF == NULL) {
	RegErr(Sub,"",AWSERR_NULLPARAM);
	return NULL;
    }
    
   /*-----------
     * Build the index based
     */
    switch (pRDBF->UniqueIdx) {
        case DOYTIME :
	    idx = doy * 1000000L + ts*10000L;
            break;
        case DOYTIMELYR :
	    idx = doy * 1000000L + ts*10000L + lyr;
            break;
	case DOYTIMEANGCLS :
	    idx = doy * 1000000L + ts*10000L + angcls*100;
            break;
        case DOYTIMELYRANGCLS :
	    idx = doy * 1000000L + ts*10000L + angcls*100 + lyr;
            break;
	defaults:
	    RegErr(Sub,"","Unknown Unique Index");
	    return NULL;
    }	       

    /*---------------
     * Allocate the memory
     */
    if ( (pRec=malloc(sizeof(RDBRecType)))==NULL) {
	RegErr(Sub,"",AWSERR_MALLOC);
	return NULL;
    }
    if ( (pRec->fld=malloc(sizeof(float)*pRDBF->NoOfFld))== NULL) {
	RegErr(Sub,"Flds",AWSERR_MALLOC);
	return NULL;
    }
    /*---------------
     * Fillin the data 
     */
    pRec->idx = idx;
    pRec->doy = doy;
    pRec->ts  = ts;
    pRec->angcls = angcls;
    pRec->lyr = lyr;
    for (i = 0;i<pRDBF->NoOfFld;i++) {
	pRec->fld[i] = MAXFLOAT;
    }

    /*---------------
     *      PLACE THE RECORD INTO THE LIST HERE.
     * The head points to the last element on the list.
     * The list is is in ascending order (singlely linked).
     */
    if (pRDBF->head == NULL) {
	/*------------
	 * Empty List
	 */
	pRDBF->head = pRec;
	pRec->next = pRec;
    }
    else {
	if (idx >= pRDBF->head->idx) {
	     /*---------
	      * greater than the greatest.
	      * new head.
	      * = sign added to keep from infinite
	      * loop if this is missued.  No precaution
	      * is taken to keep multiples from
	      * being added. Since this is not open
	      * interface code, we don't check for speed.
	      */
	     pRec->next = pRDBF->head->next;
	     pRDBF->head->next = pRec;
	     pRDBF->head = pRec;
	}
	else {
	    /*---------
	     * Not head Find first that
	     * exceeds and then place this before 
	     * the node that exceeds.
	     */
	    pPrev = pRDBF->head;
	    pCur  = pRDBF->head->next;
	    while (idx > pCur->idx) {
		pPrev=pCur;
		pCur = pCur->next;
	    }
	    pRec->next = pCur;
	    pPrev->next = pRec; 
	}
    }    
    return pRec;
}
/*--------------------------------------------------------------------------
 *                                                              FindRDBRec() 
 */
RDBRecType *
FindRDBRec(RDBFileType * pRDBF,int doy,int ts,int angcls,int lyr)
{
    char Sub[] = "Cup2RDB:FindRDBRec";
    RDBRecType * pRec;
    long idx;
    

    if (pRDBF == NULL) {
	RegErr(Sub,"",AWSERR_NULLPARAM);
	return NULL;
    }
    
    if (pRDBF->head == NULL) {
	/*------------
	 * Empty List
	 */
	return NULL;
    }

    /*-----------
     * Build the index based
     */
    switch (pRDBF->UniqueIdx) {
        case DOYTIME :
	    idx = doy * 1000000L + ts*10000L;
            break;
        case DOYTIMELYR :
	    idx = doy * 1000000L + ts*10000L + lyr;
            break;
	case DOYTIMEANGCLS :
	    idx = doy * 1000000L + ts*10000L + angcls*100;
            break;
        case DOYTIMELYRANGCLS :
	    idx = doy * 1000000L + ts*10000L + angcls*100 + lyr;
            break;
	defaults:
	    RegErr(Sub,"","Unknown Unique Index");
	    return NULL;
    }	       

    /*---------------
     * The head points to the last element on the list.
     * since we are mostly adding data sequentially we
     * can optimize alot by checking it first.
     */
    pRec = pRDBF->head;
    if (pRec->idx == idx) {
	return pRec;
    }
    else {
	/*---------
	 * Since this is ordered in ascending 
	 * order and the head points to the
	 * last element in the list ( the greatest)
	 * then we know if idx is greater than
	 * that it will not be in the list.
	 */
	if (pRec->idx < idx) {
	    return NULL;
	}
    }	

    pRec = pRec->next;
    while (pRec!=pRDBF->head)  {
	if (pRec->idx == idx) {
	    return pRec;
	}
	/*------------
	 * Since this is ordered in ascending 
	 * order we know that if the list idx
	 * gets larger than the idx then we
	 * have passed it without finding it.
	 */
	if (pRec->idx > idx) {
	    return NULL;
	}
	pRec = pRec->next;
    };
    /*------------
     * Not in the list
     */
    return NULL;
}
/*--------------------------------------------------------------------------
 *                                                            AddValToRDBF() 
 */
int 
AddValToRDBF(RDBFileType * pRDBF,int doy, int ts, int angcls, int lyr,
		 int fldidx, float val)
{

    char Sub[] = "Cup2RDB:AddValToRDBF";
    RDBRecType * pRec;
    char buf[255];

    if (pRDBF == NULL) {
	RegErr(Sub,"",AWSERR_NULLPARAM);
    }
    
    if ((pRec=FindRDBRec(pRDBF,doy,ts,angcls,lyr))==NULL) {
	if ( (pRec=CreateRDBRec(pRDBF,doy,ts,angcls,lyr)) == NULL) {
 	    RegErr(Sub,"Error Creating New RDB Rec",AWSERR_MALLOC);
	    return AWS_FAIL;
	}
    }
    if (fldidx >= pRDBF->NoOfFld) {
	sprintf(buf,"Field Index (%d) indexs past No Of Field (%d)"
	    ,fldidx,pRDBF->NoOfFld);
	return AWS_FAIL;
    }
    if (pRec->fld[fldidx] != MAXFLOAT) {
	sprintf(buf,"Over writing doy=%d ts=%d angcls=%d lyr=%d fldidx=%d",
		          doy,ts,angcls,lyr,fldidx);
	RegErr(Sub,"More than one value for Record Field",buf);
    }
    pRec->fld[fldidx] = val;

    return AWS_OK;
}



/*--------------------------------------------------------------------------
 *                                                             ProcessLine() 
 *                                                  and ProcessLineRecurse()
 * Since we can have multiple variables on a single 
 * line, or a single cupid variable mapped to 
 * a few rdb variables (differing layers etc) then
 * we need to process the line again even if we get a 
 * hit. This is accomplished with the scheme here.
 */
int 
ProcessLine(RDBFileType * pRDBF, char * line)
{
    int idx;
    int err;

    idx = 0;
    while ((err=ProcessLineRecurse(pRDBF,line,&idx))==PROCESS_CONTINUE);
    return err;
}
int
ProcessLineRecurse(RDBFileType * pRDBF, char * line,int * pidx)
{
    char Sub[] = "Cup2RDB:ProcessLine";
    char buf[255];
    int maj,min,pos,doy,ts,angcls,lyr;
    int i,fldidx;
    float val;
    FieldType * pFld;

    /*-------------
     * Check Parameters
     */
    if (pRDBF==NULL || line==NULL) {
	RegErr(Sub,"",AWSERR_NULLPARAM);
	return AWS_FAIL;
    }
    if (strlen(line)<15) {
	/*-------------
	 * Nothing to do with this one
	 */
	return AWS_OK;
    }
    if (line[1]!='2') {
	/*---------------
	 * Only Interested in 
	 * data lines, which have a 
	 * 2 in the 2nd char location.
	 */
	return AWS_OK;
    }
    /*------------------------------------------
     * OK, we have a data line.  The index fields
     * are
     * ' 2MNNDDDTTAALL'  where
     * M   = Major code
     * NN  = Minor Code
     * DDD = Day Of Year
     * TT  = Time Step
     * LL  = Layer
     * AA  = Angle Class
     */

    /*-------
     * First Check Major and Minor Codes
     */
    buf[0]= line[2];
    buf[1]= '\0';
    maj   = atoi(buf);
    buf[0]= line[3];
    buf[1]= line[4];
    buf[2]= '\0';
    min   = atoi(buf);
    fldidx = -1;

    /*--------------
     * pidx is the key to the
     * recursion.  Since more than one 
     * choosen variable can be on the same
     * line we have to recurse.
     */
    for (i=*pidx;i<pRDBF->NoOfFld;i++) {
	if (maj==pRDBF->FldList[i].maj && min==pRDBF->FldList[i].min) {
	    fldidx = i;
	    *pidx = i+1;
	    break;
	}
    }
    if (fldidx == -1) {
	/*--------
	 * Did not find matching 
	 * maj/Min in list.
	 */
	return AWS_OK;
    }
    pFld = &(pRDBF->FldList[fldidx]);
    /*--------------
     * check for angle 
     * class request
     */
    buf[0] = line[10];
    buf[1] = line[11];
    buf[2] = '\0';
    angcls=atoi(buf);
    if (pFld->angcls!=0) {
	if (pFld->angcls!=angcls) {
	    /*---------------
	     * A angle class was specified
	     * and the angle class of this line 
	     * is not the correct one.
	     */
	    return PROCESS_CONTINUE;
	}
    }
    /*------------
     * check for layer 
     * request.
     */
    buf[0] = line[12];
    buf[1] = line[13];
    buf[2] = '\0';
    lyr = atoi(buf);
    if (pFld->lyr!=0) {
	if (pFld->lyr!=lyr) {
	    /*---------------
	     * A layer was specified
	     * and the layer of this line 
	     * is not the correct one.
             * (but this alyer might be
	     * specified as another variable)
	     */
	    return PROCESS_CONTINUE;
	}
    }
    /*-----------------------------------------------------------
     * Ok, if we get to here we have a match, all we ahve to do if 
     * get the data and add it to the RDBFile structure.
     */

    /*---------
     * first check to see that the line is
     * long enough to contain the pos. And then copy
     */
    pos = pFld->pos;
    if (strlen(line) < 14+(pos-1)*7+7 ) {
        sprintf(buf,"Error finding pos %d on this line",pFld->pos);
	RegErr(Sub,"",buf);
	return AWS_FAIL;
    }
    /*----------
     * Get the value
     */
    memcpy(buf,&(line[14+(pos-1)*7]),7);
    buf[7]='\0';
    val = atof(buf);

    /*---------- 
     * Split out the other index fields
     */
    buf[0] = line[5];
    buf[1] = line[6];
    buf[2] = line[7];
    buf[3] = '\0';
    doy = atoi(buf);
    buf[0] = line[8];
    buf[1] = line[9];
    buf[2] = '\0';
    ts = atoi(buf);

    /*----------------
     * We now have all info from the line
     * now add val.
     */

   if (AddValToRDBF(pRDBF,doy,ts,angcls,lyr,fldidx,val) == AWS_FAIL) {
       sprintf(buf,
	  " val=%f, maj=%d, min=%d, pos=%d, lyr=%d, angcls=%d, doy=%d, ts=%d"
	       ,val,maj,min,pos,lyr,angcls,doy,ts);
       RegErr(Sub,"",buf);
       return AWS_FAIL;
   }
    
    return PROCESS_CONTINUE;
	
}
/*--------------------------------------------------------------------------
 *                                                                PrintRDB()
 */
int 
PrintRDB(RDBFileType * pRDBF)
{
    char Sub[]="Cup2RDB:PrintRDB";
    int i;
    int j;
    RDBRecType * pRec;

    if (pRDBF==NULL) {
	RegErr(Sub,"",AWSERR_NULLPARAM);
	return AWS_FAIL;
    }
    /*--------------------
     *                  Start with the header
     * This is 2 lines long.  The first has the tab delimited
     * field names.  The second line has the tab delimted types
     */

    /*=================================
     *                           LINE 1
     *=================================
     */
    switch (pRDBF->UniqueIdx) {
        case DOYTIME :
	    printf("idx\tdoy\tts");
            break;
        case DOYTIMELYR :
	    printf("idx\tdoy\tts\tlyr");
            break;
	case DOYTIMEANGCLS :
	    printf("idx\tdoy\tts\tangcls");
            break;
        case DOYTIMELYRANGCLS :
	    printf("idx\tdoy\tts\tangcls\tlyr");
            break;
	defaults:
	    RegErr(Sub,"","Unknown Unique Index");
	    return AWS_FAIL;
    }	       
    for (i=0;i<pRDBF->NoOfFld;i++) {
	printf("\t%s",pRDBF->FldList[i].name);
    }
    printf("\n");
    /*=================================
     *                           LINE 2
     *=================================
     */
    switch (pRDBF->UniqueIdx) {
        case DOYTIME :
	    printf("9\t3N\t2N");
            break;
        case DOYTIMELYR :
	    printf("9\t3N\t2N\t2N");
            break;
	case DOYTIMEANGCLS :
	    printf("9\t3N\t2N\t2N");
            break;
        case DOYTIMELYRANGCLS :
	    printf("9\t3N\t2N\t2N\t2N");
            break;
	default:
	    RegErr(Sub,"","Unknown Unique Index");
	    return AWS_FAIL;
    }	       
    for (i=0;i<pRDBF->NoOfFld;i++) {
	printf("\t7N");
    }
    printf("\n");
    /*=================================
     *                           BODY
     *=================================
     */
    pRec = pRDBF->head;
    if (pRec!=NULL) {
	do {
	    pRec = pRec->next; /* The first record is second after head */
	    switch (pRDBF->UniqueIdx) {
	        case DOYTIME :
	          printf("%ld\t%d\t%d",pRec->idx,pRec->doy,pRec->ts);
	          break;
		case DOYTIMELYR :
	          printf("%ld\t%d\t%d\t%d",
			 pRec->idx,pRec->doy,pRec->ts,pRec->lyr);
		  break;
		case DOYTIMEANGCLS :
		  printf("%ld\t%d\t%d\t%d",
			 pRec->idx,pRec->doy,pRec->ts,pRec->angcls);
		  break;
		case DOYTIMELYRANGCLS :
	          printf("%ld\t%d\t%d\t%d",
			 pRec->idx,pRec->doy,pRec->ts,pRec->angcls,pRec->lyr);
		  break;
		default:
		  RegErr(Sub,"","Unknown Unique Index");
		  return AWS_FAIL;
	    }	       
	    for (i=0;i<pRDBF->NoOfFld;i++) {
		if (pRec->fld[i] < -999.99||pRec->fld[i]>9999.99) {
		    printf("\t9999.99");
		}
		else {
		    printf("\t%.2f",pRec->fld[i]);
		}
	    }
	    printf("\n");
	} while (pRec != pRDBF->head);
    }
    return AWS_OK;
}
/*==========================================================================
 *                                 MAIN 
 */
main(int argc, char ** argv)
{

    char line[2048];
    int  lineno;
    char Sub[] = "Cup2RDB:main";
    char buf[2048];
    FILE * FLfh;
    FILE * COfh;
    FILE * RDBfh;

    int EndOfFile;
    /*-----------
     * RDB Out Variables
     */
    RDBFileType RDBFile;

    /*-----------
     * Send RegErr Messages to Standard Err
     * since the regular output will go to 
     * standard out.
     */
    StartErrPrint();
    /*-----------------
     * Check Parameters
     */
    if (argc!= 3) {
	goto USAGE;
    }
    if ( (FLfh=fopen(argv[1],"r") ) == NULL) {
	fprintf(stderr,"Could Not Open FieldList : %s\n",argv[1]);
	goto USAGE;
    }
    
    if ( (COfh=fopen(argv[2],"r") ) == NULL) {
	fprintf(stderr,"Could Not Open CupidOut : %s\n",argv[2]);
	goto USAGE;
    }

    /*----------------
     * Init RDBF structure
     * and load the fild file.
     */
    if (InitRDBFile(&RDBFile,FLfh) == AWS_FAIL) {
	RegErr(Sub,"Error Initializing RDBFile",argv[1]);
	fclose(FLfh);
	goto USAGE;
    }
    fclose(FLfh);

    /*------------------------------------------------------
     *                         Go Through the Cupid Out File
     */
    /*-------------
     * Toss first 2 lines
     * as comments
     */
      
    fgets(line,2048,COfh);
    fgets(line,2048,COfh);
    

    /*--------------
     * Only Go for lines that have a
     * 2 in the second position.
     */
    fgets(line,2048,COfh);
    lineno = 3;
    while(!feof(COfh)) {
	EndOfFile = FALSE;
	while (line[1] != '2') {
	    if(!fgets(line,2048,COfh)) {
		EndOfFile = TRUE;
		break;
	    }
	    lineno++;
	}
	if (EndOfFile==FALSE) {
	    if (ProcessLine(&RDBFile,line) == AWS_FAIL) {
		sprintf(buf,"Error processing line %d in %s",lineno,argv[2]);
		RegErr(Sub,buf,line);
		goto ERROR;
	    }
	} /* !EndOfFile */
	fgets(line,2048,COfh);
    } /* End of While Not End Of File */
    
    if( PrintRDB(&RDBFile) == AWS_FAIL) {
	RegErr(Sub,"","Couldn't Print the RDB File");
	goto ERROR;
    }
    EndErrPrint(); 
    return 0;
  ERROR:

    fclose(FLfh);
    fclose(COfh);
    EndErrPrint(); 
    return 1;

  USAGE:

    fprintf(stderr,"Usage : %s FieldList CupidOut > CupidRdb\n\n",argv[0]);
    fprintf(stderr,"The First line has the format:\n");
    fprintf(stderr,
	 "NoOfFld,UniqueDOY(1/0),UniqTS(1/0),UniqAngCls(1/0),UniqLyr(1/0)\n");
    fprintf(stderr,"\n");
    fprintf(stderr,"Each line in Field List has the format\n");
    fprintf(stderr," \"Field Name\",maj,min,pos,lyr,acls\n");
    fprintf(stderr,"Put 0 in for lyr and angleclass if you do not \n");
    fprintf(stderr,"have acls and lyr.  Example Field List File:\n\n");
    fprintf(stderr,"# This is a comment and it can go anywhere\n");
    fprintf(stderr,"# as long as the first column is a #\n");
    fprintf(stderr,"2,1,1,0,0\n");
    fprintf(stderr,"#See another comment!\n");
    fprintf(stderr,"\"Local Time\",4,1,1,0,0\n");
    fprintf(stderr,"\"Wind(m/s)\",4,2,3,0,0\n");
    return 1;
}

