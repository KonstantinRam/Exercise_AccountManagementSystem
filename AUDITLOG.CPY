           05  AUD-TIMESTAMP.
               10  AUD-DATE          PIC 9(8).    *> YYYYMMDD
               10  AUD-TIME          PIC 9(6).    *> HHMMSS
           05  AUD-TRANSACTION.
               10  AUD-RAW-LINE     PIC X(300).   *> Original input.
               10  AUD-TRAN-CODE     PIC X.       *> Parsed code D, W etc
               10  AUD-CUSTOMER-ID   PIC X(10).   *> Affected customer.
           05  AUD-PROCESSING.
               10  AUD-STATUS        PIC X.
                   88  AUD-SUCCESS   VALUE 'S'.
                   88  AUD-FAILED    VALUE 'F'.
                   88  AUD-REJECTED  VALUE 'R'.
               10  AUD-RETURN-CODE   PIC S9(4) COMP.
               10  AUD-ERROR-MSG     PIC X(80).
           05  AUD-RESULTS.
               10  AUD-BEFORE-BAL    PIC S9(9)V99 COMP-3.
               10  AUD-AFTER-BAL     PIC S9(9)V99 COMP-3.
               10  AUD-PROGRAM-ID    PIC X(8).    *> Which program
               10  AUD-USER-ID       PIC X(8).    *> Who ran it