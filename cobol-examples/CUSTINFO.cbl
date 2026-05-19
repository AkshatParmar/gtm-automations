      ******************************************************************
      * CUSTINFO.cbl - Customer Information File Management
      * Customer master record CRUD with KYC data fields
      * Platform: IBM z/OS COBOL with VSAM KSDS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTINFO.
       AUTHOR. LEGACY-BANKING-TEAM.
       DATE-WRITTEN. 1997-05-22.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-MASTER-FILE
               ASSIGN TO CUSTMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CM-CUST-ID
               ALTERNATE RECORD KEY IS CM-SSN
               ALTERNATE RECORD KEY IS CM-LAST-NAME
                   WITH DUPLICATES
               FILE STATUS IS WS-CUST-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  CUST-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  CUST-MASTER-RECORD.
           05  CM-CUST-ID              PIC X(12).
           05  CM-SSN                  PIC X(09).
           05  CM-NAME-DATA.
               10  CM-FIRST-NAME       PIC X(25).
               10  CM-MIDDLE-INIT      PIC X(01).
               10  CM-LAST-NAME        PIC X(30).
               10  CM-SUFFIX           PIC X(05).
           05  CM-ADDRESS-DATA.
               10  CM-ADDR-LINE-1      PIC X(35).
               10  CM-ADDR-LINE-2      PIC X(35).
               10  CM-CITY             PIC X(25).
               10  CM-STATE            PIC X(02).
               10  CM-ZIP-CODE         PIC X(10).
               10  CM-COUNTRY          PIC X(03).
           05  CM-CONTACT-DATA.
               10  CM-HOME-PHONE       PIC X(15).
               10  CM-WORK-PHONE       PIC X(15).
               10  CM-MOBILE-PHONE     PIC X(15).
               10  CM-EMAIL            PIC X(50).
           05  CM-DOB                  PIC X(08).
           05  CM-CUST-TYPE            PIC X(01).
               88  CM-TYPE-INDIVIDUAL      VALUE 'I'.
               88  CM-TYPE-JOINT           VALUE 'J'.
               88  CM-TYPE-BUSINESS        VALUE 'B'.
               88  CM-TYPE-TRUST           VALUE 'T'.
           05  CM-CUST-STATUS          PIC X(01).
               88  CM-STATUS-ACTIVE        VALUE 'A'.
               88  CM-STATUS-INACTIVE      VALUE 'I'.
               88  CM-STATUS-BLOCKED       VALUE 'B'.
               88  CM-STATUS-DECEASED      VALUE 'D'.
           05  CM-KYC-DATA.
               10  CM-ID-TYPE          PIC X(02).
                   88  CM-ID-DRIVERS       VALUE 'DL'.
                   88  CM-ID-PASSPORT      VALUE 'PP'.
                   88  CM-ID-STATE-ID      VALUE 'SI'.
                   88  CM-ID-MILITARY      VALUE 'MI'.
               10  CM-ID-NUMBER        PIC X(20).
               10  CM-ID-ISSUE-DATE    PIC X(08).
               10  CM-ID-EXPIRY-DATE   PIC X(08).
               10  CM-ID-ISSUING-STATE PIC X(02).
               10  CM-KYC-VERIFIED     PIC X(01).
                   88  CM-KYC-YES          VALUE 'Y'.
                   88  CM-KYC-NO           VALUE 'N'.
               10  CM-KYC-VERIFY-DATE  PIC X(08).
               10  CM-RISK-RATING      PIC X(01).
                   88  CM-RISK-LOW         VALUE 'L'.
                   88  CM-RISK-MEDIUM      VALUE 'M'.
                   88  CM-RISK-HIGH        VALUE 'H'.
           05  CM-OPEN-DATE            PIC X(08).
           05  CM-LAST-UPDATE-DATE     PIC X(08).
           05  CM-NUM-ACCOUNTS         PIC S9(03) COMP-3.
           05  CM-TOTAL-RELATIONSHIP   PIC S9(13)V99 COMP-3.
           05  CM-BRANCH-CODE          PIC X(06).
           05  CM-OFFICER-ID           PIC X(08).
           05  FILLER                  PIC X(20).

       WORKING-STORAGE SECTION.
       01  WS-CUST-FILE-STATUS         PIC X(02).
           88  WS-CUST-SUCCESS             VALUE '00'.
           88  WS-CUST-DUP-KEY             VALUE '22'.
           88  WS-CUST-NOT-FOUND           VALUE '23'.
           88  WS-CUST-EOF                 VALUE '10'.

       01  WS-CURRENT-DATE             PIC X(08).
       01  WS-FUNCTION-CODE            PIC X(02).
           88  WS-FUNC-ADD                 VALUE 'AD'.
           88  WS-FUNC-READ                VALUE 'RD'.
           88  WS-FUNC-UPDATE              VALUE 'UP'.
           88  WS-FUNC-SEARCH              VALUE 'SR'.
           88  WS-FUNC-KYC-VERIFY          VALUE 'KY'.

       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE 0.
           88  WS-RC-SUCCESS               VALUE 0.
           88  WS-RC-NOT-FOUND             VALUE 4.
           88  WS-RC-DUPLICATE             VALUE 8.
           88  WS-RC-FILE-ERROR            VALUE 16.

       01  WS-SEARCH-KEY               PIC X(30).

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-REQUEST
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           OPEN I-O CUST-MASTER-FILE
           IF NOT WS-CUST-SUCCESS
               DISPLAY 'CUSTINFO: ERROR OPENING CUSTOMER FILE: '
                   WS-CUST-FILE-STATUS
               MOVE 16 TO WS-RETURN-CODE
               PERFORM 9000-TERMINATE
               GOBACK
           END-IF.

       2000-PROCESS-REQUEST.
           EVALUATE TRUE
               WHEN WS-FUNC-ADD
                   PERFORM 3000-ADD-CUSTOMER
               WHEN WS-FUNC-READ
                   PERFORM 4000-READ-CUSTOMER
               WHEN WS-FUNC-UPDATE
                   PERFORM 5000-UPDATE-CUSTOMER
               WHEN WS-FUNC-SEARCH
                   PERFORM 6000-SEARCH-CUSTOMER
               WHEN WS-FUNC-KYC-VERIFY
                   PERFORM 7000-KYC-VERIFICATION
               WHEN OTHER
                   DISPLAY 'CUSTINFO: INVALID FUNCTION: '
                       WS-FUNCTION-CODE
           END-EVALUATE.

       3000-ADD-CUSTOMER.
           SET CM-STATUS-ACTIVE TO TRUE
           SET CM-KYC-NO        TO TRUE
           SET CM-RISK-LOW      TO TRUE
           MOVE WS-CURRENT-DATE TO CM-OPEN-DATE
           MOVE WS-CURRENT-DATE TO CM-LAST-UPDATE-DATE
           MOVE ZEROS            TO CM-NUM-ACCOUNTS
           MOVE ZEROS            TO CM-TOTAL-RELATIONSHIP

           WRITE CUST-MASTER-RECORD
           EVALUATE TRUE
               WHEN WS-CUST-SUCCESS
                   SET WS-RC-SUCCESS TO TRUE
               WHEN WS-CUST-DUP-KEY
                   SET WS-RC-DUPLICATE TO TRUE
               WHEN OTHER
                   SET WS-RC-FILE-ERROR TO TRUE
                   DISPLAY 'CUSTINFO: WRITE ERROR: '
                       WS-CUST-FILE-STATUS
           END-EVALUATE.

       4000-READ-CUSTOMER.
           READ CUST-MASTER-FILE INTO CUST-MASTER-RECORD
               KEY IS CM-CUST-ID
           IF WS-CUST-SUCCESS
               SET WS-RC-SUCCESS TO TRUE
           ELSE IF WS-CUST-NOT-FOUND
               SET WS-RC-NOT-FOUND TO TRUE
           ELSE
               SET WS-RC-FILE-ERROR TO TRUE
           END-IF.

       5000-UPDATE-CUSTOMER.
           READ CUST-MASTER-FILE INTO CUST-MASTER-RECORD
               KEY IS CM-CUST-ID
           IF NOT WS-CUST-SUCCESS
               SET WS-RC-NOT-FOUND TO TRUE
               EXIT PARAGRAPH
           END-IF

           MOVE WS-CURRENT-DATE TO CM-LAST-UPDATE-DATE
           REWRITE CUST-MASTER-RECORD
           IF WS-CUST-SUCCESS
               SET WS-RC-SUCCESS TO TRUE
           ELSE
               SET WS-RC-FILE-ERROR TO TRUE
           END-IF.

       6000-SEARCH-CUSTOMER.
           MOVE WS-SEARCH-KEY TO CM-LAST-NAME
           START CUST-MASTER-FILE
               KEY IS >= CM-LAST-NAME
           IF NOT WS-CUST-SUCCESS
               SET WS-RC-NOT-FOUND TO TRUE
               EXIT PARAGRAPH
           END-IF

           READ CUST-MASTER-FILE NEXT INTO CUST-MASTER-RECORD
           IF WS-CUST-SUCCESS
               SET WS-RC-SUCCESS TO TRUE
           ELSE
               SET WS-RC-NOT-FOUND TO TRUE
           END-IF.

       7000-KYC-VERIFICATION.
           READ CUST-MASTER-FILE INTO CUST-MASTER-RECORD
               KEY IS CM-CUST-ID
           IF NOT WS-CUST-SUCCESS
               SET WS-RC-NOT-FOUND TO TRUE
               EXIT PARAGRAPH
           END-IF

           SET CM-KYC-YES TO TRUE
           MOVE WS-CURRENT-DATE TO CM-KYC-VERIFY-DATE
           MOVE WS-CURRENT-DATE TO CM-LAST-UPDATE-DATE

           REWRITE CUST-MASTER-RECORD
           IF WS-CUST-SUCCESS
               SET WS-RC-SUCCESS TO TRUE
               DISPLAY 'CUSTINFO: KYC VERIFIED FOR ' CM-CUST-ID
           ELSE
               SET WS-RC-FILE-ERROR TO TRUE
           END-IF.

       9000-TERMINATE.
           CLOSE CUST-MASTER-FILE.
