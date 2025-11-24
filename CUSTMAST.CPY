      *> Customer Master RECORD FROM Task
      *> okay. It has level 88, COMP and COMP-3 so I need to create another record to raw data retrival. CUSTFILE.CPY probably.
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
               10  CUST-BALANCE     PIC S9(9)V99 COMP-3.
               10  CUST-CREDIT-LIM  PIC S9(9)V99 COMP-3.
               10  CUST-STATUS      PIC X.
                   88  CUST-ACTIVE     VALUE 'A'.
                   88  CUST-SUSPENDED  VALUE 'S'.
                   88  CUST-CLOSED     VALUE 'C'.
               10  CUST-OPEN-DATE   PIC 9(8).
               10  CUST-LAST-TRAN   PIC 9(8).
           05  CUST-STATS.
               10  CUST-TRAN-COUNT  PIC S9(7) COMP. *> COMP not COMP-3 BC its just a counter.
               10  CUST-TOTAL-DEPS  PIC S9(11)V99 COMP-3.
               10  CUST-TOTAL-WTHS  PIC S9(11)V99 COMP-3.
               