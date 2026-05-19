      ******************************************************************
      * TRANPROC.cbl - Transaction Processing Engine
      * Handles deposits, withdrawals, transfers with full journaling
      * Real-time CICS online transaction processing
      * Platform: IBM z/OS COBOL, CICS TS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANPROC.
       AUTHOR. LEGACY-BANKING-TEAM.
       DATE-WRITTEN. 1998-06-20.
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
               ACCESS MODE IS RANDOM
               RECORD KEY IS AM-ACCOUNT-NUM
               FILE STATUS IS WS-ACCT-FILE-STATUS.

           SELECT TRANS-JOURNAL-FILE
               ASSIGN TO TRANSJNL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-JRNL-FILE-STATUS.

           SELECT TRANS-INPUT-FILE
               ASSIGN TO TRANSINP
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-INP-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  ACCT-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  ACCT-MASTER-RECORD.
           05  AM-ACCOUNT-NUM          PIC X(10).
           05  AM-CUST-ID              PIC X(12).
           05  AM-ACCT-TYPE            PIC X(02).
           05  AM-ACCT-STATUS          PIC X(01).
               88  AM-STATUS-ACTIVE        VALUE 'A'.
               88  AM-STATUS-FROZEN        VALUE 'F'.
               88  AM-STATUS-CLOSED        VALUE 'C'.
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
           05  FILLER                  PIC X(52).

       FD  TRANS-JOURNAL-FILE
           LABEL RECORDS ARE STANDARD.
       01  JOURNAL-RECORD.
           05  JR-TRANS-ID             PIC X(16).
           05  JR-TRANS-TYPE           PIC X(02).
               88  JR-TYPE-DEPOSIT         VALUE 'DP'.
               88  JR-TYPE-WITHDRAWAL      VALUE 'WD'.
               88  JR-TYPE-TRANSFER-OUT    VALUE 'TO'.
               88  JR-TYPE-TRANSFER-IN     VALUE 'TI'.
               88  JR-TYPE-FEE            VALUE 'FE'.
               88  JR-TYPE-INTEREST        VALUE 'IN'.
               88  JR-TYPE-ADJUSTMENT      VALUE 'AJ'.
           05  JR-ACCOUNT-NUM          PIC X(10).
           05  JR-RELATED-ACCT         PIC X(10).
           05  JR-AMOUNT               PIC S9(13)V99 COMP-3.
           05  JR-BALANCE-BEFORE       PIC S9(13)V99 COMP-3.
           05  JR-BALANCE-AFTER        PIC S9(13)V99 COMP-3.
           05  JR-TRANS-DATE           PIC X(08).
           05  JR-TRANS-TIME           PIC X(06).
           05  JR-OPERATOR-ID          PIC X(08).
           05  JR-TERMINAL-ID          PIC X(08).
           05  JR-STATUS-CODE          PIC X(02).
               88  JR-STATUS-SUCCESS       VALUE 'OK'.
               88  JR-STATUS-FAILED        VALUE 'FL'.
               88  JR-STATUS-REVERSED      VALUE 'RV'.
           05  JR-DESCRIPTION          PIC X(40).
           05  JR-REFERENCE-NUM        PIC X(20).
           05  FILLER                  PIC X(10).

       FD  TRANS-INPUT-FILE
           LABEL RECORDS ARE STANDARD.
       01  TRANS-INPUT-RECORD.
           05  TI-TRANS-TYPE           PIC X(02).
           05  TI-SOURCE-ACCT          PIC X(10).
           05  TI-DEST-ACCT            PIC X(10).
           05  TI-AMOUNT               PIC S9(13)V99.
           05  TI-DESCRIPTION          PIC X(40).
           05  TI-OPERATOR-ID          PIC X(08).
           05  TI-TERMINAL-ID          PIC X(08).
           05  TI-REFERENCE-NUM        PIC X(20).
           05  FILLER                  PIC X(22).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-ACCT-FILE-STATUS     PIC X(02).
               88  WS-ACCT-SUCCESS         VALUE '00'.
               88  WS-ACCT-NOT-FOUND       VALUE '23'.
           05  WS-JRNL-FILE-STATUS     PIC X(02).
           05  WS-INP-FILE-STATUS      PIC X(02).
               88  WS-INP-EOF              VALUE '10'.

       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE         PIC X(08).
           05  WS-CURRENT-TIME         PIC X(06).

       01  WS-TRANS-ID-COUNTER        PIC 9(10) VALUE 0.
       01  WS-TRANS-ID                 PIC X(16).

       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE 0.
           88  WS-RC-SUCCESS               VALUE 0.
           88  WS-RC-ACCT-NOT-FOUND        VALUE 4.
           88  WS-RC-INSUFFICIENT-FUNDS    VALUE 8.
           88  WS-RC-ACCT-FROZEN           VALUE 12.
           88  WS-RC-ACCT-CLOSED           VALUE 16.
           88  WS-RC-INVALID-AMOUNT        VALUE 20.
           88  WS-RC-FILE-ERROR            VALUE 24.
           88  WS-RC-DAILY-LIMIT           VALUE 28.

       01  WS-DAILY-LIMIT             PIC S9(11)V99 COMP-3
                                          VALUE 50000.00.
       01  WS-SAVED-BALANCE            PIC S9(13)V99 COMP-3.
       01  WS-NEW-BALANCE              PIC S9(13)V99 COMP-3.
       01  WS-DISPLAY-AMOUNT           PIC Z(12)9.99-.
       01  WS-DISPLAY-BALANCE          PIC Z(12)9.99-.

       01  WS-COUNTERS.
           05  WS-TRANS-PROCESSED      PIC S9(07) COMP-3 VALUE 0.
           05  WS-TRANS-SUCCEEDED      PIC S9(07) COMP-3 VALUE 0.
           05  WS-TRANS-FAILED         PIC S9(07) COMP-3 VALUE 0.
           05  WS-TOTAL-DEPOSITS       PIC S9(15)V99 COMP-3 VALUE 0.
           05  WS-TOTAL-WITHDRAWALS    PIC S9(15)V99 COMP-3 VALUE 0.
           05  WS-TOTAL-TRANSFERS      PIC S9(15)V99 COMP-3 VALUE 0.

       01  WS-EOF-FLAG                 PIC X(01) VALUE 'N'.
           88  WS-EOF                      VALUE 'Y'.
           88  WS-NOT-EOF                  VALUE 'N'.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-TRANSACTIONS
               UNTIL WS-EOF
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-CURRENT-TIME
           OPEN I-O   ACCT-MASTER-FILE
           OPEN EXTEND TRANS-JOURNAL-FILE
           OPEN INPUT  TRANS-INPUT-FILE
           IF NOT WS-ACCT-SUCCESS
               DISPLAY 'TRANPROC: ERROR OPENING ACCOUNT FILE'
               MOVE 16 TO RETURN-CODE
               GOBACK
           END-IF
           SET WS-NOT-EOF TO TRUE
           INITIALIZE WS-COUNTERS.

       2000-PROCESS-TRANSACTIONS.
           READ TRANS-INPUT-FILE INTO TRANS-INPUT-RECORD
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-TRANS-PROCESSED
                   PERFORM 3000-VALIDATE-TRANSACTION
                   IF WS-RC-SUCCESS
                       PERFORM 4000-EXECUTE-TRANSACTION
                   ELSE
                       PERFORM 7000-LOG-FAILURE
                   END-IF
           END-READ.

       3000-VALIDATE-TRANSACTION.
           SET WS-RC-SUCCESS TO TRUE

           IF TI-AMOUNT NOT > 0
               SET WS-RC-INVALID-AMOUNT TO TRUE
               EXIT PARAGRAPH
           END-IF

           MOVE TI-SOURCE-ACCT TO AM-ACCOUNT-NUM
           READ ACCT-MASTER-FILE INTO ACCT-MASTER-RECORD
               KEY IS AM-ACCOUNT-NUM
           IF NOT WS-ACCT-SUCCESS
               SET WS-RC-ACCT-NOT-FOUND TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF AM-STATUS-CLOSED
               SET WS-RC-ACCT-CLOSED TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF AM-STATUS-FROZEN
               SET WS-RC-ACCT-FROZEN TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF TI-TRANS-TYPE = 'WD' OR 'TO'
               COMPUTE WS-NEW-BALANCE =
                   AM-AVAILABLE-BAL - TI-AMOUNT
               IF WS-NEW-BALANCE < (0 - AM-OVERDRAFT-LIMIT)
                   SET WS-RC-INSUFFICIENT-FUNDS TO TRUE
                   EXIT PARAGRAPH
               END-IF
           END-IF

           COMPUTE WS-NEW-BALANCE =
               AM-DAILY-TRANS-TOTAL + TI-AMOUNT
           IF WS-NEW-BALANCE > WS-DAILY-LIMIT
               SET WS-RC-DAILY-LIMIT TO TRUE
           END-IF.

       4000-EXECUTE-TRANSACTION.
           PERFORM 4100-GENERATE-TRANS-ID

           EVALUATE TI-TRANS-TYPE
               WHEN 'DP'
                   PERFORM 5000-PROCESS-DEPOSIT
               WHEN 'WD'
                   PERFORM 5100-PROCESS-WITHDRAWAL
               WHEN 'TO'
                   PERFORM 5200-PROCESS-TRANSFER
               WHEN OTHER
                   DISPLAY 'TRANPROC: UNKNOWN TRANS TYPE: '
                       TI-TRANS-TYPE
                   ADD 1 TO WS-TRANS-FAILED
           END-EVALUATE.

       4100-GENERATE-TRANS-ID.
           ADD 1 TO WS-TRANS-ID-COUNTER
           STRING 'TXN' WS-CURRENT-DATE(3:6)
                  WS-TRANS-ID-COUNTER
               DELIMITED BY SIZE
               INTO WS-TRANS-ID.

       5000-PROCESS-DEPOSIT.
           MOVE AM-BALANCE TO WS-SAVED-BALANCE
           ADD TI-AMOUNT TO AM-BALANCE
           ADD TI-AMOUNT TO AM-AVAILABLE-BAL
           ADD TI-AMOUNT TO AM-DAILY-TRANS-TOTAL
           ADD 1         TO AM-DAILY-TRANS-COUNT
           MOVE WS-CURRENT-DATE TO AM-LAST-TRANS-DATE
           MOVE WS-CURRENT-TIME TO AM-LAST-TRANS-TIME

           REWRITE ACCT-MASTER-RECORD
           IF WS-ACCT-SUCCESS
               ADD 1 TO WS-TRANS-SUCCEEDED
               ADD TI-AMOUNT TO WS-TOTAL-DEPOSITS
               PERFORM 6000-WRITE-JOURNAL-SUCCESS
           ELSE
               ADD 1 TO WS-TRANS-FAILED
               PERFORM 7000-LOG-FAILURE
           END-IF.

       5100-PROCESS-WITHDRAWAL.
           MOVE AM-BALANCE TO WS-SAVED-BALANCE
           SUBTRACT TI-AMOUNT FROM AM-BALANCE
           SUBTRACT TI-AMOUNT FROM AM-AVAILABLE-BAL
           ADD TI-AMOUNT TO AM-DAILY-TRANS-TOTAL
           ADD 1         TO AM-DAILY-TRANS-COUNT
           MOVE WS-CURRENT-DATE TO AM-LAST-TRANS-DATE
           MOVE WS-CURRENT-TIME TO AM-LAST-TRANS-TIME

           REWRITE ACCT-MASTER-RECORD
           IF WS-ACCT-SUCCESS
               ADD 1 TO WS-TRANS-SUCCEEDED
               ADD TI-AMOUNT TO WS-TOTAL-WITHDRAWALS
               PERFORM 6000-WRITE-JOURNAL-SUCCESS
           ELSE
               ADD 1 TO WS-TRANS-FAILED
               PERFORM 7000-LOG-FAILURE
           END-IF.

       5200-PROCESS-TRANSFER.
      *    --------------------------------------------------------
      *    TWO-PHASE TRANSFER: DEBIT SOURCE, THEN CREDIT TARGET
      *    NOTE: In production, this would use CICS syncpoint
      *    for atomicity across both account updates.
      *    --------------------------------------------------------
           MOVE AM-BALANCE TO WS-SAVED-BALANCE
           SUBTRACT TI-AMOUNT FROM AM-BALANCE
           SUBTRACT TI-AMOUNT FROM AM-AVAILABLE-BAL
           ADD TI-AMOUNT TO AM-DAILY-TRANS-TOTAL
           ADD 1         TO AM-DAILY-TRANS-COUNT
           MOVE WS-CURRENT-DATE TO AM-LAST-TRANS-DATE
           MOVE WS-CURRENT-TIME TO AM-LAST-TRANS-TIME

           REWRITE ACCT-MASTER-RECORD
           IF NOT WS-ACCT-SUCCESS
               ADD 1 TO WS-TRANS-FAILED
               PERFORM 7000-LOG-FAILURE
               EXIT PARAGRAPH
           END-IF

      *    CREDIT DESTINATION ACCOUNT
           MOVE TI-DEST-ACCT TO AM-ACCOUNT-NUM
           READ ACCT-MASTER-FILE INTO ACCT-MASTER-RECORD
               KEY IS AM-ACCOUNT-NUM
           IF NOT WS-ACCT-SUCCESS
      *        ROLLBACK SOURCE DEBIT
               MOVE TI-SOURCE-ACCT TO AM-ACCOUNT-NUM
               READ ACCT-MASTER-FILE INTO ACCT-MASTER-RECORD
                   KEY IS AM-ACCOUNT-NUM
               ADD TI-AMOUNT TO AM-BALANCE
               ADD TI-AMOUNT TO AM-AVAILABLE-BAL
               REWRITE ACCT-MASTER-RECORD
               ADD 1 TO WS-TRANS-FAILED
               PERFORM 7000-LOG-FAILURE
               EXIT PARAGRAPH
           END-IF

           ADD TI-AMOUNT TO AM-BALANCE
           ADD TI-AMOUNT TO AM-AVAILABLE-BAL
           MOVE WS-CURRENT-DATE TO AM-LAST-TRANS-DATE
           MOVE WS-CURRENT-TIME TO AM-LAST-TRANS-TIME

           REWRITE ACCT-MASTER-RECORD
           IF WS-ACCT-SUCCESS
               ADD 1 TO WS-TRANS-SUCCEEDED
               ADD TI-AMOUNT TO WS-TOTAL-TRANSFERS
               PERFORM 6000-WRITE-JOURNAL-SUCCESS
           ELSE
               ADD 1 TO WS-TRANS-FAILED
               PERFORM 7000-LOG-FAILURE
           END-IF.

       6000-WRITE-JOURNAL-SUCCESS.
           MOVE WS-TRANS-ID       TO JR-TRANS-ID
           MOVE TI-TRANS-TYPE     TO JR-TRANS-TYPE
           MOVE TI-SOURCE-ACCT    TO JR-ACCOUNT-NUM
           MOVE TI-DEST-ACCT      TO JR-RELATED-ACCT
           MOVE TI-AMOUNT         TO JR-AMOUNT
           MOVE WS-SAVED-BALANCE  TO JR-BALANCE-BEFORE
           MOVE AM-BALANCE        TO JR-BALANCE-AFTER
           MOVE WS-CURRENT-DATE   TO JR-TRANS-DATE
           MOVE WS-CURRENT-TIME   TO JR-TRANS-TIME
           MOVE TI-OPERATOR-ID    TO JR-OPERATOR-ID
           MOVE TI-TERMINAL-ID    TO JR-TERMINAL-ID
           SET JR-STATUS-SUCCESS  TO TRUE
           MOVE TI-DESCRIPTION    TO JR-DESCRIPTION
           MOVE TI-REFERENCE-NUM  TO JR-REFERENCE-NUM
           WRITE JOURNAL-RECORD.

       7000-LOG-FAILURE.
           MOVE WS-TRANS-ID       TO JR-TRANS-ID
           MOVE TI-TRANS-TYPE     TO JR-TRANS-TYPE
           MOVE TI-SOURCE-ACCT    TO JR-ACCOUNT-NUM
           MOVE TI-DEST-ACCT      TO JR-RELATED-ACCT
           MOVE TI-AMOUNT         TO JR-AMOUNT
           MOVE ZEROS             TO JR-BALANCE-BEFORE
           MOVE ZEROS             TO JR-BALANCE-AFTER
           MOVE WS-CURRENT-DATE   TO JR-TRANS-DATE
           MOVE WS-CURRENT-TIME   TO JR-TRANS-TIME
           MOVE TI-OPERATOR-ID    TO JR-OPERATOR-ID
           MOVE TI-TERMINAL-ID    TO JR-TERMINAL-ID
           SET JR-STATUS-FAILED   TO TRUE
           MOVE TI-DESCRIPTION    TO JR-DESCRIPTION
           MOVE TI-REFERENCE-NUM  TO JR-REFERENCE-NUM
           WRITE JOURNAL-RECORD
           ADD 1 TO WS-TRANS-FAILED.

       9000-TERMINATE.
           CLOSE ACCT-MASTER-FILE
           CLOSE TRANS-JOURNAL-FILE
           CLOSE TRANS-INPUT-FILE
           DISPLAY 'TRANPROC: PROCESSING COMPLETE'
           DISPLAY 'TRANPROC: TRANSACTIONS PROCESSED: '
               WS-TRANS-PROCESSED
           DISPLAY 'TRANPROC: SUCCEEDED: ' WS-TRANS-SUCCEEDED
           DISPLAY 'TRANPROC: FAILED:    ' WS-TRANS-FAILED
           MOVE WS-TOTAL-DEPOSITS TO WS-DISPLAY-AMOUNT
           DISPLAY 'TRANPROC: TOTAL DEPOSITS:    ' WS-DISPLAY-AMOUNT
           MOVE WS-TOTAL-WITHDRAWALS TO WS-DISPLAY-AMOUNT
           DISPLAY 'TRANPROC: TOTAL WITHDRAWALS: ' WS-DISPLAY-AMOUNT
           MOVE WS-TOTAL-TRANSFERS TO WS-DISPLAY-AMOUNT
           DISPLAY 'TRANPROC: TOTAL TRANSFERS:   ' WS-DISPLAY-AMOUNT.
