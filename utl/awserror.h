/*
    $Id: awserror.h,v 1.2 1995/04/25 04:05:24 murdock Exp $
*/
#ifndef __AWSERROR_H
#define __AWSERROR_H
#include <stdio.h>

#ifndef TRUE
#    define TRUE	1
#    define FALSE	0
#endif

#define AWS_OK		0
#define AWS_SUCCESS	0
#define AWS_FAIL	-1
#define AWS_BADVAL	-2
#define AWS_ENDOFSTREAM	-3

#define AWSERR_MALLOC	"Error Allocating Memory"
#define AWSERR_FOPEN	"Error Opening File"
#define AWSERR_FWRITE	"Error Writing File"
#define AWSERR_NULLPARAM "Null Parameter Passed"
#define AWSERR_BADPARAM  "Bad Param Passed"

#define MSG_LEN	255

int StartErrLog(char * ErrorLogName);
int AppendErrLog(char * ErrorLogName);
void StartConPrint();
void EndConPrint();
void StartErrPrint();
void EndErrPrint();
void EndErrLog();
void RegErr(char * ErrSub1,char * ErrSub2, char * lErrMsg);
void ClearErr();
char * GetErrMsg();
char * GetErrSub1();
char * GetErrSub2();
void PrintErr();
void FPrintErr(FILE * fh);
/*-------------------
 * Message Log Functions
 */
int OpenMsgLog(int MsgLogID,char * MsgLogName);
int AppendMsgLog(int MsgLogID,char * MsgLogName);
int CloseMsgLog(int MsgLogID);
int StartMsgLogConPrint(int MsgLogID);
int EndMsgLogConPrint(int MsgLogID);
int RegLogMsg(int MsgLogID,  char * msg);

#endif
