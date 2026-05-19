      ******************************************************************
      * BATCHSET.cbl - Batch Settlement Processing
      * End-of-day settlement: interest calc, daily totals, reporting
      * Runs as JCL batch job, typically scheduled at COB (close of biz)
      * Platform: IBM z/OS COBOL batch with JCL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCHSET.
       AUTHOR. LEGACY-BANKING-TEAM.
       DATE-WRITTEN. 1999-01-10.
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
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AM-ACCOUNT-NUM
               FILE STATUS IS WS-ACCT-FILE-STATUS.

           SELECT TRANS-JOURNAL-FILE
               ASSIGN TO TRANSJNL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-JRNL-FILE-STATUS.

           SELECT SETTLE-OUTPUT-FILE
               ASSIGN TO SETTLOUT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STTL-FILE-STATUS.

           SELECT SETTLE-REPORT-FILE
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
           05  AM-ACCT-STATUS          PIC X(01).
               88  AM-STATUS-ACTIVE        VALUE 'A'.
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

       FD  TRANS-JOURNAL-FILE
           LABEL RECORDS ARE STANDARD.
       01  JOURNAL-RECORD.
           05  JR-TRANS-ID             PIC X(16).
           05  JR-TRANS-TYPE           PIC X(02).
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
           05  JR-DESCRIPTION          PIC X(40).
           05  JR-REFERENCE-NUM        PIC X(20).
           05  FILLER                  PIC X(10).

       FD  SETTLE-OUTPUT-FILE
           LABEL RECORDS ARE STANDARD.
       01  SETTLE-OUTPUT-RECORD.
           05  SO-SETTLE-DATE          PIC X(08).
           05  SO-ACCOUNT-NUM          PIC X(10).
           05  SO-ACCT-TYPE            PIC X(02).
           05  SO-OPEN-BALANCE         PIC S9(13)V99 COMP-3.
           05  SO-TOTAL-DEBITS         PIC S9(13)V99 COMP-3.
           05  SO-TOTAL-CREDITS        PIC S9(13)V99 COMP-3.
           05  SO-INTEREST-EARNED      PIC S9(09)V99 COMP-3.
           05  SO-FEES-CHARGED         PIC S9(09)V99 COMP-3.
           05  SO-CLOSE-BALANCE        PIC S9(13)V99 COMP-3.
           05  SO-TRANS-COUNT          PIC S9(05) COMP-3.
           05  FILLER                  PIC X(30).

       FD  SETTLE-REPORT-FILE
           LABEL RECORDS ARE STANDARD.
       01  REPORT-LINE                 PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-ACCT-FILE-STATUS     PIC X(02).
               88  WS-ACCT-SUCCESS         VALUE '00'.
               88  WS-ACCT-EOF             VALUE '10'.
           05  WS-JRNL-FILE-STATUS     PIC X(02).
               88  WS-JRNL-EOF             VALUE '10'.
           05  WS-STTL-FILE-STATUS     PIC X(02).
           05  WS-RPT-FILE-STATUS      PIC X(02).

       01  WS-CURRENT-DATE            PIC X(08).
       01  WS-EOF-FLAG                PIC X VALUE 'N'.
           88  WS-EOF                     VALUE 'Y'.
           88  WS-NOT-EOF                 VALUE 'N'.

       01  WS-DAILY-INTEREST           PIC S9(09)V9(8) COMP-3.
       01  WS-DAYS-IN-YEAR             PIC S9(03) COMP-3 VALUE 365.
       01  WS-MONTHLY-FEE              PIC S9(05)V99 COMP-3.
       01  WS-LOW-BAL-THRESHOLD        PIC S9(09)V99 COMP-3
                                           VALUE 1000.00.
       01  WS-LOW-BAL-FEE              PIC S9(05)V99 COMP-3
                                           VALUE 12.50.
       01  WS-OVERDRAFT-FEE            PIC S9(05)V99 COMP-3
                                           VALUE 35.00.

       01  WS-COUNTERS.
           05  WS-ACCTS-PROCESSED      PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCTS-INTEREST       PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCTS-FEES           PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-INTEREST       PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-TOTAL-FEES           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-TOTAL-BALANCES       PIC S9(15)V99 COMP-3 VALUE 0.

       01  WS-REPORT-HEADER.
           05  FILLER                  PIC X(20) VALUE
               'DAILY SETTLEMENT RPT'.
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  WS-RPT-DATE             PIC X(08).
           05  FILLER                  PIC X(99) VALUE SPACES.

       01  WS-REPORT-DETAIL.
           05  WS-RPT-ACCT-NUM        PIC X(10).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  WS-RPT-ACCT-TYPE        PIC X(02).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  WS-RPT-BALANCE          PIC Z(12)9.99-.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  WS-RPT-INTEREST         PIC Z(8)9.99-.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  WS-RPT-FEES             PIC Z(8)9.99-.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  WS-RPT-TRANS-CNT        PIC Z(4)9.
           05  FILLER                  PIC X(58) VALUE SPACES.

       01  WS-DISPLAY-AMOUNT           PIC Z(12)9.99-.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-ACCOUNTS
               UNTIL WS-EOF
           PERFORM 3000-WRITE-SUMMARY
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           OPEN I-O    ACCT-MASTER-FILE
           OPEN OUTPUT SETTLE-OUTPUT-FILE
           OPEN OUTPUT SETTLE-REPORT-FILE
           INITIALIZE WS-COUNTERS
           SET WS-NOT-EOF TO TRUE

           MOVE WS-CURRENT-DATE TO WS-RPT-DATE
           WRITE REPORT-LINE FROM WS-REPORT-HEADER
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE.

       2000-PROCESS-ACCOUNTS.
           READ ACCT-MASTER-FILE INTO ACCT-MASTER-RECORD
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-ACCTS-PROCESSED
                   IF AM-STATUS-ACTIVE
                       PERFORM 2100-CALCULATE-INTEREST
                       PERFORM 2200-ASSESS-FEES
                       PERFORM 2300-RESET-DAILY-COUNTERS
                       PERFORM 2400-UPDATE-ACCOUNT
                       PERFORM 2500-WRITE-SETTLE-RECORD
                       PERFORM 2600-WRITE-REPORT-LINE
                   END-IF
           END-READ.

       2100-CALCULATE-INTEREST.
           IF AM-BALANCE > 0
               COMPUTE WS-DAILY-INTEREST ROUNDED =
                   (AM-BALANCE * AM-INTEREST-RATE)
                   / WS-DAYS-IN-YEAR
               ADD WS-DAILY-INTEREST TO AM-BALANCE
               ADD WS-DAILY-INTEREST TO AM-AVAILABLE-BAL
               ADD WS-DAILY-INTEREST TO AM-MTD-INTEREST
               ADD WS-DAILY-INTEREST TO AM-YTD-INTEREST
               ADD WS-DAILY-INTEREST TO WS-TOTAL-INTEREST
               ADD 1 TO WS-ACCTS-INTEREST
           END-IF.

       2200-ASSESS-FEES.
           MOVE ZEROS TO WS-MONTHLY-FEE

      *    LOW BALANCE FEE FOR CHECKING ACCOUNTS
           IF AM-TYPE-CHECKING AND
              AM-BALANCE < WS-LOW-BAL-THRESHOLD
               ADD WS-LOW-BAL-FEE TO WS-MONTHLY-FEE
           END-IF

      *    OVERDRAFT FEE
           IF AM-BALANCE < 0
               ADD WS-OVERDRAFT-FEE TO WS-MONTHLY-FEE
           END-IF

           IF WS-MONTHLY-FEE > 0
               SUBTRACT WS-MONTHLY-FEE FROM AM-BALANCE
               SUBTRACT WS-MONTHLY-FEE FROM AM-AVAILABLE-BAL
               ADD WS-MONTHLY-FEE TO WS-TOTAL-FEES
               ADD 1 TO WS-ACCTS-FEES
           END-IF.

       2300-RESET-DAILY-COUNTERS.
           MOVE ZEROS TO AM-DAILY-TRANS-COUNT
           MOVE ZEROS TO AM-DAILY-TRANS-TOTAL.

       2400-UPDATE-ACCOUNT.
           REWRITE ACCT-MASTER-RECORD
           IF NOT WS-ACCT-SUCCESS
               DISPLAY 'BATCHSET: REWRITE ERROR FOR ACCT '
                   AM-ACCOUNT-NUM
                   ' STATUS: ' WS-ACCT-FILE-STATUS
           END-IF
           ADD AM-BALANCE TO WS-TOTAL-BALANCES.

       2500-WRITE-SETTLE-RECORD.
           MOVE WS-CURRENT-DATE    TO SO-SETTLE-DATE
           MOVE AM-ACCOUNT-NUM     TO SO-ACCOUNT-NUM
           MOVE AM-ACCT-TYPE       TO SO-ACCT-TYPE
           MOVE AM-BALANCE         TO SO-CLOSE-BALANCE
           MOVE WS-DAILY-INTEREST  TO SO-INTEREST-EARNED
           MOVE WS-MONTHLY-FEE     TO SO-FEES-CHARGED
           MOVE AM-DAILY-TRANS-COUNT TO SO-TRANS-COUNT
           WRITE SETTLE-OUTPUT-RECORD.

       2600-WRITE-REPORT-LINE.
           MOVE AM-ACCOUNT-NUM     TO WS-RPT-ACCT-NUM
           MOVE AM-ACCT-TYPE       TO WS-RPT-ACCT-TYPE
           MOVE AM-BALANCE         TO WS-RPT-BALANCE
           MOVE WS-DAILY-INTEREST  TO WS-RPT-INTEREST
           MOVE WS-MONTHLY-FEE     TO WS-RPT-FEES
           MOVE AM-DAILY-TRANS-COUNT TO WS-RPT-TRANS-CNT
           WRITE REPORT-LINE FROM WS-REPORT-DETAIL.

       3000-WRITE-SUMMARY.
           MOVE ALL '=' TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE

           STRING 'ACCOUNTS PROCESSED: '
                  WS-ACCTS-PROCESSED
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE

           MOVE WS-TOTAL-INTEREST TO WS-DISPLAY-AMOUNT
           MOVE SPACES TO REPORT-LINE
           STRING 'TOTAL INTEREST ACCRUED: ' WS-DISPLAY-AMOUNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE

           MOVE WS-TOTAL-FEES TO WS-DISPLAY-AMOUNT
           MOVE SPACES TO REPORT-LINE
           STRING 'TOTAL FEES CHARGED:    ' WS-DISPLAY-AMOUNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE

           MOVE WS-TOTAL-BALANCES TO WS-DISPLAY-AMOUNT
           MOVE SPACES TO REPORT-LINE
           STRING 'TOTAL PORTFOLIO VALUE: ' WS-DISPLAY-AMOUNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE.

       9000-TERMINATE.
           CLOSE ACCT-MASTER-FILE
           CLOSE SETTLE-OUTPUT-FILE
           CLOSE SETTLE-REPORT-FILE
           DISPLAY 'BATCHSET: SETTLEMENT COMPLETE FOR '
               WS-CURRENT-DATE
           DISPLAY 'BATCHSET: ACCOUNTS PROCESSED: '
               WS-ACCTS-PROCESSED.
