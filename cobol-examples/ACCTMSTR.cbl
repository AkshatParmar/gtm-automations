      ******************************************************************
      * ACCTMSTR.cbl - Account Master File Management
      * Core banking account operations: create, read, update, close
      * Platform: IBM z/OS COBOL with VSAM KSDS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTMSTR.
       AUTHOR. LEGACY-BANKING-TEAM.
       DATE-WRITTEN. 1997-03-15.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-MASTER-FILE
               ASSIGN TO ACCTMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AM-ACCOUNT-NUM
               ALTERNATE RECORD KEY IS AM-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-FILE-STATUS.

           SELECT ACCT-HIST-FILE
               ASSIGN TO ACCTHIST
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-HIST-FILE-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO RPTFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-RPT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  ACCT-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  ACCT-MASTER-RECORD.
           05  AM-ACCOUNT-NUM          PIC X(10).
           05  AM-CUST-ID              PIC X(12).
           05  AM-ACCT-TYPE            PIC X(02).
               88  AM-TYPE-CHECKING        VALUE 'CK'.
               88  AM-TYPE-SAVINGS         VALUE 'SV'.
               88  AM-TYPE-MONEY-MARKET    VALUE 'MM'.
               88  AM-TYPE-CD              VALUE 'CD'.
               88  AM-TYPE-LOAN            VALUE 'LN'.
           05  AM-ACCT-STATUS          PIC X(01).
               88  AM-STATUS-ACTIVE        VALUE 'A'.
               88  AM-STATUS-FROZEN        VALUE 'F'.
               88  AM-STATUS-CLOSED        VALUE 'C'.
               88  AM-STATUS-DORMANT       VALUE 'D'.
           05  AM-BALANCE              PIC S9(13)V99 COMP-3.
           05  AM-AVAILABLE-BAL        PIC S9(13)V99 COMP-3.
           05  AM-HOLD-AMOUNT          PIC S9(13)V99 COMP-3.
           05  AM-OVERDRAFT-LIMIT      PIC S9(09)V99 COMP-3.
           05  AM-INTEREST-RATE        PIC S9(03)V9(6) COMP-3.
           05  AM-OPEN-DATE            PIC X(08).
           05  AM-CLOSE-DATE           PIC X(08).
           05  AM-LAST-TRANS-DATE      PIC X(08).
           05  AM-LAST-TRANS-TIME      PIC X(06).
           05  AM-BRANCH-CODE          PIC X(06).
           05  AM-OFFICER-ID           PIC X(08).
           05  AM-CURRENCY-CODE        PIC X(03).
           05  AM-DAILY-TRANS-COUNT    PIC S9(05) COMP-3.
           05  AM-DAILY-TRANS-TOTAL    PIC S9(13)V99 COMP-3.
           05  AM-MTD-INTEREST         PIC S9(09)V99 COMP-3.
           05  AM-YTD-INTEREST         PIC S9(11)V99 COMP-3.
           05  FILLER                  PIC X(40).

       FD  ACCT-HIST-FILE
           LABEL RECORDS ARE STANDARD.
       01  ACCT-HIST-RECORD.
           05  AH-ACCOUNT-NUM          PIC X(10).
           05  AH-ACTION-CODE          PIC X(02).
           05  AH-ACTION-DATE          PIC X(08).
           05  AH-ACTION-TIME          PIC X(06).
           05  AH-OLD-STATUS           PIC X(01).
           05  AH-NEW-STATUS           PIC X(01).
           05  AH-OPERATOR-ID          PIC X(08).
           05  AH-TERMINAL-ID          PIC X(08).
           05  AH-DESCRIPTION          PIC X(50).
           05  FILLER                  PIC X(16).

       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD.
       01  REPORT-RECORD              PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-ACCT-FILE-STATUS     PIC X(02).
               88  WS-ACCT-SUCCESS         VALUE '00'.
               88  WS-ACCT-DUP-KEY         VALUE '22'.
               88  WS-ACCT-NOT-FOUND       VALUE '23'.
               88  WS-ACCT-EOF             VALUE '10'.
           05  WS-HIST-FILE-STATUS     PIC X(02).
           05  WS-RPT-FILE-STATUS      PIC X(02).

       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE         PIC X(08).
           05  WS-CURRENT-TIME         PIC X(06).

       01  WS-FUNCTION-CODE           PIC X(02).
           88  WS-FUNC-CREATE             VALUE 'CR'.
           88  WS-FUNC-READ               VALUE 'RD'.
           88  WS-FUNC-UPDATE             VALUE 'UP'.
           88  WS-FUNC-CLOSE              VALUE 'CL'.
           88  WS-FUNC-FREEZE             VALUE 'FZ'.
           88  WS-FUNC-INQUIRY            VALUE 'IQ'.

       01  WS-RETURN-CODE              PIC S9(04) COMP.
           88  WS-RC-SUCCESS               VALUE 0.
           88  WS-RC-NOT-FOUND             VALUE 4.
           88  WS-RC-DUPLICATE             VALUE 8.
           88  WS-RC-INVALID-STATUS        VALUE 12.
           88  WS-RC-FILE-ERROR            VALUE 16.
           88  WS-RC-INVALID-FUNCTION      VALUE 20.

       01  WS-ACCOUNT-INPUT.
           05  WS-INP-ACCOUNT-NUM      PIC X(10).
           05  WS-INP-CUST-ID          PIC X(12).
           05  WS-INP-ACCT-TYPE        PIC X(02).
           05  WS-INP-BRANCH-CODE      PIC X(06).
           05  WS-INP-OFFICER-ID       PIC X(08).
           05  WS-INP-CURRENCY         PIC X(03).
           05  WS-INP-OVERDRAFT-LMT    PIC S9(09)V99.
           05  WS-INP-INTEREST-RATE    PIC S9(03)V9(6).

       01  WS-COUNTERS.
           05  WS-RECORDS-READ         PIC S9(07) COMP-3 VALUE 0.
           05  WS-RECORDS-WRITTEN      PIC S9(07) COMP-3 VALUE 0.
           05  WS-RECORDS-UPDATED      PIC S9(07) COMP-3 VALUE 0.
           05  WS-ERROR-COUNT          PIC S9(05) COMP-3 VALUE 0.

       01  WS-DISPLAY-BALANCE         PIC Z(12)9.99-.
       01  WS-DISPLAY-RATE            PIC Z9.999999.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-REQUEST
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-CURRENT-TIME
           OPEN I-O ACCT-MASTER-FILE
           IF NOT WS-ACCT-SUCCESS
               DISPLAY 'ACCTMSTR: ERROR OPENING MASTER FILE: '
                   WS-ACCT-FILE-STATUS
               MOVE 16 TO WS-RETURN-CODE
               PERFORM 9000-TERMINATE
               GOBACK
           END-IF
           OPEN EXTEND ACCT-HIST-FILE
           INITIALIZE WS-COUNTERS.

       2000-PROCESS-REQUEST.
           EVALUATE TRUE
               WHEN WS-FUNC-CREATE
                   PERFORM 3000-CREATE-ACCOUNT
               WHEN WS-FUNC-READ
               WHEN WS-FUNC-INQUIRY
                   PERFORM 4000-READ-ACCOUNT
               WHEN WS-FUNC-UPDATE
                   PERFORM 5000-UPDATE-ACCOUNT
               WHEN WS-FUNC-CLOSE
                   PERFORM 6000-CLOSE-ACCOUNT
               WHEN WS-FUNC-FREEZE
                   PERFORM 7000-FREEZE-ACCOUNT
               WHEN OTHER
                   SET WS-RC-INVALID-FUNCTION TO TRUE
                   DISPLAY 'ACCTMSTR: INVALID FUNCTION CODE: '
                       WS-FUNCTION-CODE
           END-EVALUATE.

       3000-CREATE-ACCOUNT.
           MOVE WS-INP-ACCOUNT-NUM TO AM-ACCOUNT-NUM
           MOVE WS-INP-CUST-ID     TO AM-CUST-ID
           MOVE WS-INP-ACCT-TYPE   TO AM-ACCT-TYPE
           SET  AM-STATUS-ACTIVE    TO TRUE
           MOVE ZEROS               TO AM-BALANCE
           MOVE ZEROS               TO AM-AVAILABLE-BAL
           MOVE ZEROS               TO AM-HOLD-AMOUNT
           MOVE WS-INP-OVERDRAFT-LMT
                                    TO AM-OVERDRAFT-LIMIT
           MOVE WS-INP-INTEREST-RATE
                                    TO AM-INTEREST-RATE
           MOVE WS-CURRENT-DATE     TO AM-OPEN-DATE
           MOVE SPACES               TO AM-CLOSE-DATE
           MOVE WS-CURRENT-DATE     TO AM-LAST-TRANS-DATE
           MOVE WS-CURRENT-TIME     TO AM-LAST-TRANS-TIME
           MOVE WS-INP-BRANCH-CODE  TO AM-BRANCH-CODE
           MOVE WS-INP-OFFICER-ID   TO AM-OFFICER-ID
           MOVE WS-INP-CURRENCY     TO AM-CURRENCY-CODE
           MOVE ZEROS               TO AM-DAILY-TRANS-COUNT
           MOVE ZEROS               TO AM-DAILY-TRANS-TOTAL
           MOVE ZEROS               TO AM-MTD-INTEREST
           MOVE ZEROS               TO AM-YTD-INTEREST

           WRITE ACCT-MASTER-RECORD
           EVALUATE TRUE
               WHEN WS-ACCT-SUCCESS
                   SET WS-RC-SUCCESS TO TRUE
                   ADD 1 TO WS-RECORDS-WRITTEN
                   PERFORM 8000-LOG-HISTORY
               WHEN WS-ACCT-DUP-KEY
                   SET WS-RC-DUPLICATE TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN OTHER
                   SET WS-RC-FILE-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY 'ACCTMSTR: WRITE ERROR: '
                       WS-ACCT-FILE-STATUS
           END-EVALUATE.

       4000-READ-ACCOUNT.
           MOVE WS-INP-ACCOUNT-NUM TO AM-ACCOUNT-NUM
           READ ACCT-MASTER-FILE INTO ACCT-MASTER-RECORD
               KEY IS AM-ACCOUNT-NUM
           EVALUATE TRUE
               WHEN WS-ACCT-SUCCESS
                   SET WS-RC-SUCCESS TO TRUE
                   ADD 1 TO WS-RECORDS-READ
               WHEN WS-ACCT-NOT-FOUND
                   SET WS-RC-NOT-FOUND TO TRUE
               WHEN OTHER
                   SET WS-RC-FILE-ERROR TO TRUE
                   DISPLAY 'ACCTMSTR: READ ERROR: '
                       WS-ACCT-FILE-STATUS
           END-EVALUATE.

       5000-UPDATE-ACCOUNT.
           MOVE WS-INP-ACCOUNT-NUM TO AM-ACCOUNT-NUM
           READ ACCT-MASTER-FILE INTO ACCT-MASTER-RECORD
               KEY IS AM-ACCOUNT-NUM
           IF NOT WS-ACCT-SUCCESS
               SET WS-RC-NOT-FOUND TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF AM-STATUS-CLOSED OR AM-STATUS-FROZEN
               SET WS-RC-INVALID-STATUS TO TRUE
               EXIT PARAGRAPH
           END-IF

           MOVE WS-CURRENT-DATE TO AM-LAST-TRANS-DATE
           MOVE WS-CURRENT-TIME TO AM-LAST-TRANS-TIME

           REWRITE ACCT-MASTER-RECORD
           IF WS-ACCT-SUCCESS
               SET WS-RC-SUCCESS TO TRUE
               ADD 1 TO WS-RECORDS-UPDATED
               PERFORM 8000-LOG-HISTORY
           ELSE
               SET WS-RC-FILE-ERROR TO TRUE
               ADD 1 TO WS-ERROR-COUNT
           END-IF.

       6000-CLOSE-ACCOUNT.
           MOVE WS-INP-ACCOUNT-NUM TO AM-ACCOUNT-NUM
           READ ACCT-MASTER-FILE INTO ACCT-MASTER-RECORD
               KEY IS AM-ACCOUNT-NUM
           IF NOT WS-ACCT-SUCCESS
               SET WS-RC-NOT-FOUND TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF AM-BALANCE NOT = ZEROS
               SET WS-RC-INVALID-STATUS TO TRUE
               DISPLAY 'ACCTMSTR: CANNOT CLOSE - BALANCE NOT ZERO'
               EXIT PARAGRAPH
           END-IF

           SET AM-STATUS-CLOSED TO TRUE
           MOVE WS-CURRENT-DATE TO AM-CLOSE-DATE
           MOVE WS-CURRENT-DATE TO AM-LAST-TRANS-DATE
           MOVE WS-CURRENT-TIME TO AM-LAST-TRANS-TIME

           REWRITE ACCT-MASTER-RECORD
           IF WS-ACCT-SUCCESS
               SET WS-RC-SUCCESS TO TRUE
               ADD 1 TO WS-RECORDS-UPDATED
               PERFORM 8000-LOG-HISTORY
           ELSE
               SET WS-RC-FILE-ERROR TO TRUE
           END-IF.

       7000-FREEZE-ACCOUNT.
           MOVE WS-INP-ACCOUNT-NUM TO AM-ACCOUNT-NUM
           READ ACCT-MASTER-FILE INTO ACCT-MASTER-RECORD
               KEY IS AM-ACCOUNT-NUM
           IF NOT WS-ACCT-SUCCESS
               SET WS-RC-NOT-FOUND TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF NOT AM-STATUS-ACTIVE
               SET WS-RC-INVALID-STATUS TO TRUE
               EXIT PARAGRAPH
           END-IF

           SET AM-STATUS-FROZEN TO TRUE
           MOVE WS-CURRENT-DATE TO AM-LAST-TRANS-DATE
           MOVE WS-CURRENT-TIME TO AM-LAST-TRANS-TIME

           REWRITE ACCT-MASTER-RECORD
           IF WS-ACCT-SUCCESS
               SET WS-RC-SUCCESS TO TRUE
               ADD 1 TO WS-RECORDS-UPDATED
               PERFORM 8000-LOG-HISTORY
           ELSE
               SET WS-RC-FILE-ERROR TO TRUE
           END-IF.

       8000-LOG-HISTORY.
           MOVE AM-ACCOUNT-NUM    TO AH-ACCOUNT-NUM
           MOVE WS-FUNCTION-CODE  TO AH-ACTION-CODE
           MOVE WS-CURRENT-DATE   TO AH-ACTION-DATE
           MOVE WS-CURRENT-TIME   TO AH-ACTION-TIME
           MOVE AM-ACCT-STATUS    TO AH-NEW-STATUS
           WRITE ACCT-HIST-RECORD.

       9000-TERMINATE.
           CLOSE ACCT-MASTER-FILE
           CLOSE ACCT-HIST-FILE
           DISPLAY 'ACCTMSTR: PROCESSING COMPLETE'
           DISPLAY 'ACCTMSTR: RECORDS READ:    ' WS-RECORDS-READ
           DISPLAY 'ACCTMSTR: RECORDS WRITTEN: ' WS-RECORDS-WRITTEN
           DISPLAY 'ACCTMSTR: RECORDS UPDATED: ' WS-RECORDS-UPDATED
           DISPLAY 'ACCTMSTR: ERRORS:          ' WS-ERROR-COUNT.
