           05  CUST-ID              PIC X(10).
           05  CUST-EMAIL           PIC X(50).
           05  CUST-PERSONAL.
               10  CUST-LAST-NAME   PIC X(30).
               10  CUST-FIRST-NAME  PIC X(20).
           05  CUST-ADDRESS.
               10  CUST-STREET      PIC X(50).
               10  CUST-CITY        PIC X(30).
               10  CUST-STATE       PIC XX.
               10  CUST-ZIP         PIC X(10).
           05  CUST-ACCOUNT.
               10  CUST-BALANCE     PIC X(11).
               10  CUST-CREDIT-LIM  PIC X(11).
               10  CUST-STATUS      PIC X.
               10  CUST-OPEN-DATE   PIC X(8).
               10  CUST-LAST-TRAN   PIC X(8).
           05  CUST-STATS.
               10  CUST-TRAN-COUNT  PIC X(7).
               10  CUST-TOTAL-DEPS  PIC X(11).
               10  CUST-TOTAL-WTHS  PIC X(11).
