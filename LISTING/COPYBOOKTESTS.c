GnuCOBOL 3.2.0          COPYBOOKTESTS.cbl                                             Sun Nov 23 2025 18:24:14  Page 0001

LINE    PG/LN  A...B............................................................SEQUENCE

000001         IDENTIFICATION DIVISION.
000002         PROGRAM-ID. COPYBOOKTESTS.
000003
000004         DATA DIVISION.
000005         WORKING-STORAGE SECTION.
000006        *> 01  WS-CUSTOMER.
000007        *>     COPY CUSTMAST.
000008
000009         01  WS-TRANSACTION.
000010             COPY TRANREC.
000001C            05  TRAN-LINE             PIC X(300).
000002C
000011
000012         01  WS-TRAN-WTH.
000013             COPY TRANWTH.
000001C            05  WTH-CODE              PIC X.
000002C            05  WTH-CUSTOMER-ID       PIC X(10).
000003C            05  WTH-AMOUNT            PIC S9(7)V99 COMP-3.
000004C            05  WTH-DATE              PIC 9(8).
000005C            05  WTH-DESCRIPTION       PIC X(50).
000006C
000014
000015        *> COPY STATCODE.
000016
000017         PROCEDURE DIVISION.
000018             DISPLAY "Copybooks compile successfully."
000019             GOBACK.
000020
000021         END PROGRAM COPYBOOKTESTS.


0 warnings in compilation group
0 errors in compilation group
