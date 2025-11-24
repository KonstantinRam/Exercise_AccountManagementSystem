       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPYBOOKTESTS.


       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  WS-CUSTOMER.
           COPY CUSTMAST.
       
       01  WS-TRANSACTION.
           COPY TRANREC.
           
       01  WS-TRAN-WTH.
           COPY TRANWTH.

       01  WS-CUSTOMER-RAW.
           COPY CUSTFILE.
       COPY STATCODE.

       
       
       PROCEDURE DIVISION.
           DISPLAY "Copybooks compile successfully."
           GOBACK.

       END PROGRAM COPYBOOKTESTS.
