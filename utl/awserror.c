static char rcsid[] = "$Header: /home/murdock/prg/src/utl/RCS/awserror.c,v 1.2 1995/04/25 04:04:55 murdock Exp $";
/*
   $Log: awserror.c,v $
 * Revision 1.2  1995/04/25  04:04:55  murdock
 * Fixed some long standing small bug.
 *
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <awserror.h>

char ErrMsg[MSG_LEN];
char ErrSub1[MSG_LEN];
char ErrSub2[MSG_LEN];

FILE * LogFH;
int    LogOpen = FALSE;
int    IsErr = FALSE;
int    ConPrint = FALSE;
int    ErrPrint = FALSE;

FILE *	MsgLogFH[10];
int	MsgLogOpen[10] = {FALSE,FALSE,FALSE,FALSE,FALSE,
			  FALSE,FALSE,FALSE,FALSE,FALSE};
int 	MsgLogConPrint[10] = {FALSE,FALSE,FALSE,FALSE,FALSE,
			      FALSE,FALSE,FALSE,FALSE,FALSE};

/*----------------------------------------------------------------------
							StartErrLog()
*/
int
StartErrLog(char * ErrorLogName)
{
    if ( (LogFH=fopen(ErrorLogName,"w")) == NULL) {
	return AWS_FAIL;
    }
    LogOpen = TRUE;
    return AWS_OK;
}
/*----------------------------------------------------------------------
							AppendErrLog()
*/
int
AppendErrLog(char * ErrorLogName)
{
    if ( (LogFH=fopen(ErrorLogName,"a")) == NULL) {
	return AWS_FAIL;
    }
    LogOpen = TRUE;
    return AWS_OK;
}
/*----------------------------------------------------------------------
							EndErrLog()
*/
void 
EndErrLog() {
    fclose(LogFH);
    LogOpen = FALSE;
}
/*----------------------------------------------------------------------
							StartConPrint()
*/
void
StartConPrint()
{
    ConPrint = TRUE;
}
/*----------------------------------------------------------------------
							EndConPrint()
*/
void
EndConPrint()
{
    ConPrint = FALSE;
}
/*----------------------------------------------------------------------
							StartErrPrint()
*/
void
StartErrPrint()
{
    ErrPrint = TRUE;
}
/*----------------------------------------------------------------------
							EndErrPrint()
*/
void
EndErrPrint()
{
    ErrPrint = FALSE;
}
/*----------------------------------------------------------------------
							RegErr()
*/
void
RegErr(char * lErrSub1,char * lErrSub2,char * lErrMsg) {
    if (   (lErrSub1== NULL)
	|| (lErrSub2== NULL)
	|| (lErrMsg == NULL) ) {
	RegErr("AwsError::RegErr","","One of the Parameters was NULL");
    }
    else  {
	if (    (strlen(lErrSub1) > MSG_LEN)
	     || (strlen(lErrSub2) > MSG_LEN)
	     || (strlen(lErrMsg) > MSG_LEN) ) {
	    RegErr("AwsError::RegErr","","One of the Parameters > MSG_LEN");
	}
	else {
	    strcpy(ErrSub1,lErrSub1);
	    strcpy(ErrSub2,lErrSub2);
	    strcpy(ErrMsg,lErrMsg);
	    IsErr = TRUE;
	    if (LogOpen == TRUE) {
		fprintf(LogFH,"Sub : %s | %s | Msg : %s\n",
			ErrSub1,ErrSub2,ErrMsg);
	    }
	    if (ConPrint == TRUE ) {
		printf("Sub : %s | %s | Msg : %s\n",
			ErrSub1,ErrSub2,ErrMsg);
	    }
	    if (ErrPrint == TRUE ) {
		fprintf(stderr,"Sub : %s | %s | Msg : %s\n",
			ErrSub1,ErrSub2,ErrMsg);
	    }
	    fflush(LogFH);
	}
    }
}
/*----------------------------------------------------------------------
							ClearErr()
*/
void
ClearErr()
{
    ErrSub1[0] = '\0';
    ErrSub2[0] = '\0';
    ErrMsg[0] = '\0';
    IsErr = FALSE;
}

/*----------------------------------------------------------------------
							GetErrMsg()
*/
char *
GetErrMsg() 
{
   if (IsErr == FALSE) ClearErr();
   return ErrMsg;
}
/*----------------------------------------------------------------------
							GetErrSub1()
*/
char *
GetErrSub1() 
{
   if (IsErr == FALSE) ClearErr();
   return ErrSub1;
}
/*----------------------------------------------------------------------
							GetErrSub2()
*/
char *
GetErrSub2() 
{
   if (IsErr == FALSE) ClearErr();
   return ErrSub2;
}
/*----------------------------------------------------------------------
							PrintErr()
*/
void
PrintErr() 
{
    if (IsErr==FALSE) {
	printf("No Error Registered");
    }
    else {
	printf("Sub : %s %s | Msg : %s\n",ErrSub1,ErrSub2,ErrMsg);
    }
}

/*----------------------------------------------------------------------
							FPrintErr()
*/
void
FPrintErr(FILE * fh) 
{
    if (IsErr==FALSE) {
	fprintf(fh,"No Error Registered");
    }
    else {
	fprintf(fh,"Sub : %s %s | Msg : %s\n",ErrSub1,ErrSub2,ErrMsg);
    }
}
/*============================================================================|
 *							MESSAGE LOG FUNCTIONS |
 *============================================================================|
 */
/*----------------------------------------------------------------------
							OpenMsgLog()
*/
int
OpenMsgLog(int MsgLogID,char * MsgLogName)
{
    char Sub[40];
    strcpy(Sub,"AwsError:OpenMsgLog");

    if (MsgLogID < 0 || MsgLogID > 9) {
	RegErr(Sub,"","MsgLogID should be between 0 and 9");
	return AWS_FAIL;
    }
    if (MsgLogName==NULL) {
	RegErr(Sub,MsgLogName,AWSERR_NULLPARAM);
	return AWS_FAIL;
    }

    if ( (MsgLogFH[MsgLogID]=fopen(MsgLogName,"w")) == NULL) {
	RegErr(Sub,MsgLogName,AWSERR_FOPEN);
	return AWS_FAIL;
    }
    MsgLogOpen[MsgLogID] = TRUE;
    return AWS_OK;
}
/*----------------------------------------------------------------------
							AppendMsgLog()
*/
int
AppendMsgLog(int MsgLogID,char * MsgLogName)
{
    char Sub[40];
    strcpy(Sub,"AwsError:AppendMsgLog");

    if (MsgLogID < 0 || MsgLogID > 9) {
	RegErr(Sub,"","MsgLogID should be between 0 and 9");
	return AWS_FAIL;
    }
    if (MsgLogName==NULL) {
	RegErr(Sub,MsgLogName,AWSERR_NULLPARAM);
	return AWS_FAIL;
    }

    if ( (MsgLogFH[MsgLogID]=fopen(MsgLogName,"a")) == NULL) {
	RegErr(Sub,MsgLogName,AWSERR_FOPEN);
	return AWS_FAIL;
    }
    MsgLogOpen[MsgLogID] = TRUE;
    return AWS_OK;
}
/*----------------------------------------------------------------------
							CloseMsgLog()
*/
int 
CloseMsgLog(int MsgLogID) {
    if (MsgLogID < 0 || MsgLogID > 9) {
	RegErr("AwsError:CloseMsgLog","","MsgLogID should be between 0 and 9");
	return AWS_FAIL;
    }
    fclose(MsgLogFH[MsgLogID]);
    MsgLogOpen[MsgLogID] = FALSE;
    return AWS_OK;
}
/*----------------------------------------------------------------------
						   StartMsgLogConPrint()
*/
int
StartMsgLogConPrint(int MsgLogID)
{
    if (MsgLogID < 0 || MsgLogID > 9) {
	RegErr("AwsError:StartMsgLogConPrint","",
		"MsgLogID should be between 0 and 9");
	return AWS_FAIL;
    }
    MsgLogConPrint[MsgLogID] = TRUE;
    return AWS_OK;
}
/*----------------------------------------------------------------------
						     EndMsgLogConPrint()
*/
int
EndMsgLogConPrint(int MsgLogID)
{
    if (MsgLogID < 0 || MsgLogID > 9) {
	RegErr("AwsError:EndMsgLogConPrint","",
		"MsgLogID should be between 0 and 9");
	return AWS_FAIL;
    }
    MsgLogConPrint[MsgLogID] = FALSE;
    return AWS_OK;
}
/*----------------------------------------------------------------------
							RegLogMsg()
*/
int
RegLogMsg(int MsgLogID,  char * msg) 
{
    char Sub[40];
	
    strcpy(Sub,"AwsError:RegLogMsg");

    if (MsgLogID < 0 || MsgLogID > 9) {
	RegErr(Sub,"", "MsgLogID should be between 0 and 9");
	return AWS_FAIL;
    }
    if (msg==NULL) {
	RegErr(Sub,"msg",AWSERR_NULLPARAM);
	return AWS_FAIL;
    }
    if (MsgLogConPrint[MsgLogID]==TRUE) {
	printf(msg);
    }
    else {
    	if (MsgLogOpen[MsgLogID]==TRUE) {
	    fprintf(MsgLogFH[MsgLogID],msg);
	}
	else {
	    /*--------------------
	     * Msg Log 0 can be used as a non log.  /dev/null...
 	     */
            if (MsgLogID!=0) {
	        RegErr(Sub,"Cant write",
				"Log File or Console Printing not opened");
	        return AWS_FAIL;
	    }
	}
    }
    return AWS_OK;
}
