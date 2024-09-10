      ******************************************************************
      * DCLGEN TABLE(PROD.TRANSACTIONS)                                *
      *        LIBRARY(ISPW.DEMO.PROD.DCLG(TRANSDCL))                  *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE PROD.TRANSACTIONS TABLE
           ( ACCOUNT_NO                     INTEGER NOT NULL,
             TRANS_ID                       INTEGER NOT NULL,
             TRANS_DATE                     DATE NOT NULL,
             TRANS_REF                      CHAR(20) NOT NULL,
             TRANS_AMOUNT                   DECIMAL(10, 2) NOT NULL,
             TRANS_TYPE                     CHAR(2) NOT NULL,
             BALANCE                        DECIMAL(10, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE PROD.TRANSACTIONS                  *
      ******************************************************************
       01  DCLTRANSACTIONS.
           10 ACCOUNT-NO           PIC S9(9) USAGE COMP.
           10 TRANS-ID             PIC S9(9) USAGE COMP.
           10 TRANS-DATE           PIC X(10).
           10 TRANS-REF            PIC X(20).
           10 TRANS-AMOUNT         PIC S9(8)V9(2) USAGE COMP-3.
           10 TRANS-TYPE           PIC X(2).
           10 BALANCE              PIC S9(8)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
