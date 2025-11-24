           05  RAW-CUST-ID              PIC X(10).
           05  RAW-CUST-EMAIL           PIC X(50).
           05  RAW-CUST-PERSONAL.
               10  RAW-CUST-LAST-NAME   PIC X(30).
               10  RAW-CUST-FIRST-NAME  PIC X(20).
           05  RAW-CUST-ADDRESS.
               10  RAW-CUST-STREET      PIC X(50).
               10  RAW-CUST-CITY        PIC X(30).
               10  RAW-CUST-STATE       PIC XX.
               10  RAW-CUST-ZIP         PIC X(10).
           05  RAW-CUST-ACCOUNT.
               10  RAW-CUST-BALANCE     PIC X(11).
               10  RAW-CUST-CREDIT-LIM  PIC X(11).
               10  RAW-CUST-STATUS      PIC X.
               10  RAW-CUST-OPEN-DATE   PIC X(8).
               10  RAW-CUST-LAST-TRAN   PIC X(8).
           05  RAW-CUST-STATS.
               10  RAW-CUST-TRAN-COUNT  PIC X(7).
               10  RAW-CUST-TOTAL-DEPS  PIC X(11).
               10  RAW-CUST-TOTAL-WTHS  PIC X(11).
