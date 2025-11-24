      *> Project requirments: Return codes: 0=Success, 4=Warning, 8=Error, 12=Severe
       01  RETURN-CODES.
           05  RC-SUCCESS       PIC S9(4) COMP VALUE 0.
           05  RC-WARNING       PIC S9(4) COMP VALUE 4.
           05  RC-ERROR         PIC S9(4) COMP VALUE 8.
           05  RC-SEVERE        PIC S9(4) COMP VALUE 12.
           