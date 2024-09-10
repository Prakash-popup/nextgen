       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANHIST.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'TRANHIST------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-CALEN                 PIC S9(4) COMP.

      * Error Message structure
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' EXMPCMAN'.
           03 FILLER                   PIC X(11) VALUE ' REQUESTID='.
           03 EM-REQUEST-ID            PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-DETAIL                PIC X(50) VALUE SPACES.

      * Working variables
       01 WS-REQ.
         03 WS-ACCT-NO             PIC 9(15).

       01 WS-RES.
         03 WS-TRANS-ITEM OCCURS 10 TIMES.
           05 WS-TRANS-ID          PIC X(15).
           05 WS-TRANS-DATE        PIC X(10).
           05 WS-TRANS-REF         PIC X(20).
           05 WS-TRANS-AMOUNT      PIC X(11).
           05 WS-TRANS-TYPE        PIC X(2).
           05 WS-BALANCE           PIC X(11).

       01 WS-I                     PIC S9(4) COMP VALUE ZERO.
       01 WS-TRNQRY                PIC X(08) VALUE 'TRANQURY'.

      *01 WS-PTR1                  USAGE IS POINTER.
      *01 WS-PTR2                  USAGE IS POINTER.

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY TRANCOPY.

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.

       MAINLINE SECTION.

           PERFORM 1000-INITIALIZE-VARS.
           PERFORM 2000-GET-TRAN-HISTORY.
           PERFORM 3000-RETURN-TO-CALLER.

       MAINLINE-EXIT.
           EXIT.

       1000-INITIALIZE-VARS SECTION.

           INITIALIZE ERROR-MSG
                      WS-REQ
                      WS-RES.
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.

      * If NO commarea received issue an ABEND
           IF EIBCALEN IS EQUAL TO ZERO THEN
      *        MOVE ' NO COMMAREA RECEIVED' TO EM-DETAIL
      *        PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('EXCA') NODUMP END-EXEC
           ELSE
               MOVE DFHCOMMAREA(1:15) TO WS-ACCT-NO
               DISPLAY 'Input A/C no:' WS-ACCT-NO
           END-IF.

       1000-INITIALIZE-VARS-EXIT.
           EXIT.

       2000-GET-TRAN-HISTORY SECTION.

      *    CALL 'TRANQURY' USING WS-REQ WS-RES.

      *    CALL WS-TRNQRY USING WS-REQ WS-RES.
           CALL WS-TRNQRY USING DFHEIBLK DFHCOMMAREA WS-REQ WS-RES.

           MOVE 0 TO WS-I.

      *    Move work variable to comm area..
           PERFORM 10 TIMES
              ADD +1 TO WS-I
              MOVE WS-TRANS-ID(WS-I)     TO TR-TRANS-ID(WS-I)
              MOVE WS-TRANS-DATE(WS-I)   TO TR-TRANS-DATE(WS-I)
              MOVE WS-TRANS-REF(WS-I)    TO TR-TRANS-REF(WS-I)
              MOVE WS-TRANS-AMOUNT(WS-I)
                                         TO TR-TRANS-AMOUNT(WS-I)
              MOVE WS-TRANS-TYPE(WS-I)   TO TR-TRANS-TYPE(WS-I)
              MOVE WS-BALANCE(WS-I)      TO TR-BALANCE(WS-I)
      *       DISPLAY 'Copied-CA:'  TR-TRANS-ITEM(WS-I)
           END-PERFORM.

       2000-GET-TRAN-HISTORY-EXIT.
           EXIT.

       3000-RETURN-TO-CALLER SECTION.

           EXEC CICS RETURN END-EXEC.

       3000-RETURN-TO-CALLER-EXIT.
           EXIT.
