      ******************************************************************
      * AUDTLOG.cbl - Audit Trail and Compliance Logging
      * Regulatory audit logging for SOX/BSA/AML compliance
      * Generates reports for internal audit and regulatory filings
      * Platform: IBM z/OS COBOL batch
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUDTLOG.
       AUTHOR. LEGACY-BANKING-TEAM.
       DATE-WRITTEN. 2002-09-15.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUDIT-LOG-FILE
               ASSIGN TO AUDTFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUDIT-FILE-STATUS.

           SELECT TRANS-JOURNAL-FILE
               ASSIGN TO TRANSJNL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-JRNL-FILE-STATUS.

           SELECT CTR-REPORT-FILE
               ASSIGN TO CTRRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CTR-FILE-STATUS.

           SELECT SAR-REPORT-FILE
               ASSIGN TO SARRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-SAR-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  AUDIT-LOG-FILE
           LABEL RECORDS ARE STANDARD.
       01  AUDIT-LOG-RECORD.
           05  AL-LOG-ID               PIC X(20).
           05  AL-LOG-DATE             PIC X(08).
           05  AL-LOG-TIME             PIC X(06).
           05  AL-EVENT-TYPE           PIC X(03).
               88  AL-EVT-LOGIN            VALUE 'LGN'.
               88  AL-EVT-LOGOUT           VALUE 'LGO'.
               88  AL-EVT-TRANS            VALUE 'TXN'.
               88  AL-EVT-ACCT-CHANGE      VALUE 'ACH'.
               88  AL-EVT-CUST-CHANGE      VALUE 'CCH'.
               88  AL-EVT-AUTH-FAIL        VALUE 'AFI'.
               88  AL-EVT-OVERRIDE         VALUE 'OVR'.
               88  AL-EVT-LARGE-TRANS      VALUE 'LTX'.
               88  AL-EVT-SUSPICIOUS       VALUE 'SUS'.
           05  AL-SEVERITY             PIC X(01).
               88  AL-SEV-INFO             VALUE 'I'.
               88  AL-SEV-WARNING          VALUE 'W'.
               88  AL-SEV-CRITICAL         VALUE 'C'.
           05  AL-OPERATOR-ID          PIC X(08).
           05  AL-TERMINAL-ID          PIC X(08).
           05  AL-ACCOUNT-NUM          PIC X(10).
           05  AL-CUST-ID              PIC X(12).
           05  AL-AMOUNT               PIC S9(13)V99 COMP-3.
           05  AL-DESCRIPTION          PIC X(80).
           05  AL-IP-ADDRESS           PIC X(15).
           05  FILLER                  PIC X(10).

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

       FD  CTR-REPORT-FILE
           LABEL RECORDS ARE STANDARD.
       01  CTR-REPORT-LINE             PIC X(132).

       FD  SAR-REPORT-FILE
           LABEL RECORDS ARE STANDARD.
       01  SAR-REPORT-LINE             PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-AUDIT-FILE-STATUS    PIC X(02).
           05  WS-JRNL-FILE-STATUS     PIC X(02).
               88  WS-JRNL-EOF             VALUE '10'.
           05  WS-CTR-FILE-STATUS      PIC X(02).
           05  WS-SAR-FILE-STATUS      PIC X(02).

       01  WS-CTR-THRESHOLD            PIC S9(11)V99 COMP-3
                                           VALUE 10000.00.
       01  WS-LARGE-TRANS-THRESH       PIC S9(11)V99 COMP-3
                                           VALUE 50000.00.
       01  WS-STRUCTURING-THRESH       PIC S9(11)V99 COMP-3
                                           VALUE 9000.00.
       01  WS-STRUCTURING-COUNT        PIC S9(03) COMP-3 VALUE 0.
       01  WS-STRUCTURING-WINDOW       PIC S9(03) COMP-3 VALUE 3.

       01  WS-CURRENT-DATE             PIC X(08).
       01  WS-CURRENT-TIME             PIC X(06).
       01  WS-LOG-COUNTER              PIC 9(10) VALUE 0.

       01  WS-EOF-FLAG                 PIC X VALUE 'N'.
           88  WS-EOF                      VALUE 'Y'.
           88  WS-NOT-EOF                  VALUE 'N'.

       01  WS-PREV-ACCOUNT             PIC X(10) VALUE SPACES.
       01  WS-ACCT-DAILY-TOTAL         PIC S9(13)V99 COMP-3 VALUE 0.
       01  WS-ACCT-DAILY-COUNT         PIC S9(05) COMP-3 VALUE 0.

       01  WS-COUNTERS.
           05  WS-RECORDS-READ         PIC S9(09) COMP-3 VALUE 0.
           05  WS-CTR-GENERATED        PIC S9(07) COMP-3 VALUE 0.
           05  WS-SAR-GENERATED        PIC S9(07) COMP-3 VALUE 0.
           05  WS-LARGE-TRANS-COUNT    PIC S9(07) COMP-3 VALUE 0.
           05  WS-AUDIT-RECORDS        PIC S9(09) COMP-3 VALUE 0.

       01  WS-DISPLAY-AMOUNT           PIC Z(12)9.99-.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-SCAN-JOURNAL
               UNTIL WS-EOF
           PERFORM 2500-CHECK-FINAL-ACCOUNT
           PERFORM 3000-WRITE-SUMMARY
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-CURRENT-TIME
           OPEN INPUT  TRANS-JOURNAL-FILE
           OPEN EXTEND AUDIT-LOG-FILE
           OPEN OUTPUT CTR-REPORT-FILE
           OPEN OUTPUT SAR-REPORT-FILE
           INITIALIZE WS-COUNTERS
           SET WS-NOT-EOF TO TRUE.

       2000-SCAN-JOURNAL.
           READ TRANS-JOURNAL-FILE INTO JOURNAL-RECORD
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-RECORDS-READ
                   PERFORM 2100-CHECK-CTR-THRESHOLD
                   PERFORM 2200-CHECK-LARGE-TRANS
                   PERFORM 2300-CHECK-STRUCTURING
                   PERFORM 2400-TRACK-DAILY-TOTALS
           END-READ.

       2100-CHECK-CTR-THRESHOLD.
      *    BSA CURRENCY TRANSACTION REPORT: >$10,000
           IF JR-AMOUNT > WS-CTR-THRESHOLD
               PERFORM 4000-GENERATE-CTR
           END-IF.

       2200-CHECK-LARGE-TRANS.
      *    INTERNAL FLAG: TRANSACTIONS OVER $50,000
           IF JR-AMOUNT > WS-LARGE-TRANS-THRESH
               ADD 1 TO WS-LARGE-TRANS-COUNT
               PERFORM 5000-LOG-AUDIT-EVENT
           END-IF.

       2300-CHECK-STRUCTURING.
      *    DETECT POTENTIAL STRUCTURING:
      *    MULTIPLE TRANSACTIONS JUST UNDER $10K CTR THRESHOLD
           IF JR-AMOUNT >= WS-STRUCTURING-THRESH AND
              JR-AMOUNT < WS-CTR-THRESHOLD
               IF JR-ACCOUNT-NUM = WS-PREV-ACCOUNT
                   ADD 1 TO WS-STRUCTURING-COUNT
                   IF WS-STRUCTURING-COUNT >= WS-STRUCTURING-WINDOW
                       PERFORM 6000-GENERATE-SAR
                       MOVE 0 TO WS-STRUCTURING-COUNT
                   END-IF
               ELSE
                   MOVE 1 TO WS-STRUCTURING-COUNT
               END-IF
           END-IF.

       2400-TRACK-DAILY-TOTALS.
           IF JR-ACCOUNT-NUM NOT = WS-PREV-ACCOUNT
               IF WS-PREV-ACCOUNT NOT = SPACES
                   PERFORM 2500-CHECK-FINAL-ACCOUNT
               END-IF
               MOVE JR-ACCOUNT-NUM TO WS-PREV-ACCOUNT
               MOVE 0 TO WS-ACCT-DAILY-TOTAL
               MOVE 0 TO WS-ACCT-DAILY-COUNT
           END-IF
           ADD JR-AMOUNT TO WS-ACCT-DAILY-TOTAL
           ADD 1 TO WS-ACCT-DAILY-COUNT.

       2500-CHECK-FINAL-ACCOUNT.
      *    CHECK IF CUMULATIVE DAILY TOTAL EXCEEDS CTR THRESHOLD
           IF WS-ACCT-DAILY-TOTAL > WS-CTR-THRESHOLD
               PERFORM 4000-GENERATE-CTR
           END-IF.

       3000-WRITE-SUMMARY.
           MOVE SPACES TO CTR-REPORT-LINE
           STRING 'AUDIT SCAN COMPLETE - ' WS-CURRENT-DATE
               DELIMITED BY SIZE INTO CTR-REPORT-LINE
           WRITE CTR-REPORT-LINE

           MOVE SPACES TO CTR-REPORT-LINE
           STRING 'JOURNAL RECORDS SCANNED: ' WS-RECORDS-READ
               DELIMITED BY SIZE INTO CTR-REPORT-LINE
           WRITE CTR-REPORT-LINE

           MOVE SPACES TO CTR-REPORT-LINE
           STRING 'CTR REPORTS GENERATED:   ' WS-CTR-GENERATED
               DELIMITED BY SIZE INTO CTR-REPORT-LINE
           WRITE CTR-REPORT-LINE

           MOVE SPACES TO CTR-REPORT-LINE
           STRING 'SAR REPORTS GENERATED:   ' WS-SAR-GENERATED
               DELIMITED BY SIZE INTO CTR-REPORT-LINE
           WRITE CTR-REPORT-LINE.

       4000-GENERATE-CTR.
      *    CURRENCY TRANSACTION REPORT (FINCEN FORM 104)
           ADD 1 TO WS-CTR-GENERATED
           MOVE SPACES TO CTR-REPORT-LINE
           MOVE JR-AMOUNT TO WS-DISPLAY-AMOUNT
           STRING 'CTR: ACCT=' JR-ACCOUNT-NUM
                  ' AMT=' WS-DISPLAY-AMOUNT
                  ' DATE=' JR-TRANS-DATE
                  ' REF=' JR-REFERENCE-NUM
               DELIMITED BY SIZE INTO CTR-REPORT-LINE
           WRITE CTR-REPORT-LINE
           PERFORM 5000-LOG-AUDIT-EVENT.

       5000-LOG-AUDIT-EVENT.
           ADD 1 TO WS-LOG-COUNTER
           ADD 1 TO WS-AUDIT-RECORDS
           STRING 'AUD' WS-CURRENT-DATE WS-LOG-COUNTER
               DELIMITED BY SIZE INTO AL-LOG-ID
           MOVE WS-CURRENT-DATE    TO AL-LOG-DATE
           MOVE WS-CURRENT-TIME    TO AL-LOG-TIME
           SET AL-EVT-LARGE-TRANS  TO TRUE
           SET AL-SEV-WARNING      TO TRUE
           MOVE JR-OPERATOR-ID     TO AL-OPERATOR-ID
           MOVE JR-TERMINAL-ID     TO AL-TERMINAL-ID
           MOVE JR-ACCOUNT-NUM     TO AL-ACCOUNT-NUM
           MOVE JR-AMOUNT          TO AL-AMOUNT
           MOVE JR-DESCRIPTION     TO AL-DESCRIPTION
           WRITE AUDIT-LOG-RECORD.

       6000-GENERATE-SAR.
      *    SUSPICIOUS ACTIVITY REPORT (FINCEN FORM 111)
           ADD 1 TO WS-SAR-GENERATED
           MOVE SPACES TO SAR-REPORT-LINE
           STRING 'SAR: POTENTIAL STRUCTURING DETECTED'
                  ' ACCT=' JR-ACCOUNT-NUM
                  ' DATE=' JR-TRANS-DATE
               DELIMITED BY SIZE INTO SAR-REPORT-LINE
           WRITE SAR-REPORT-LINE

           SET AL-EVT-SUSPICIOUS TO TRUE
           SET AL-SEV-CRITICAL   TO TRUE
           PERFORM 5000-LOG-AUDIT-EVENT.

       9000-TERMINATE.
           CLOSE TRANS-JOURNAL-FILE
           CLOSE AUDIT-LOG-FILE
           CLOSE CTR-REPORT-FILE
           CLOSE SAR-REPORT-FILE
           DISPLAY 'AUDTLOG: PROCESSING COMPLETE'
           DISPLAY 'AUDTLOG: RECORDS SCANNED: ' WS-RECORDS-READ
           DISPLAY 'AUDTLOG: CTR GENERATED:   ' WS-CTR-GENERATED
           DISPLAY 'AUDTLOG: SAR GENERATED:   ' WS-SAR-GENERATED
           DISPLAY 'AUDTLOG: AUDIT ENTRIES:   ' WS-AUDIT-RECORDS.
