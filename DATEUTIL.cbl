       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATEUTIL.
      *>****************************************************************
      *> Date validation and manipulation utility
      *>****************************************************************
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREAS.
           05  WS-YEAR           PIC 9(4).
           05 WS-IS-LEAP-YEAR   PIC X VALUE 'N'.
               88 LEAP-YEAR             VALUE 'Y'.
               88 NOT-LEAP-YEAR         VALUE 'N'.

           05  WS-MONTH          PIC 99.
           05  WS-DAY            PIC 99.
           
       01  WS-FEB-DAY-CHECK  PIC 99 VALUE 28.
       LINKAGE SECTION.
      *> Pointers from a caller.
       01  LS-DATE-YYYYMMDD     PIC 9(8).
       01  LS-RETURN-CODE        PIC S9(4) COMP.
       01  LS-ERROR-MESSAGE      PIC X(80).
       
       PROCEDURE DIVISION USING LS-DATE-YYYYMMDD
                                LS-RETURN-CODE
                                LS-ERROR-MESSAGE.
       
       VALIDATE-DATE.
           MOVE ZERO TO LS-RETURN-CODE
           MOVE SPACES TO LS-ERROR-MESSAGE
           
      *>   Extract components
           MOVE LS-DATE-YYYYMMDD(1:4) TO WS-YEAR
           MOVE LS-DATE-YYYYMMDD(5:2) TO WS-MONTH  
           MOVE LS-DATE-YYYYMMDD(7:2) TO WS-DAY
           
           IF WS-YEAR < 1900 OR WS-YEAR > 2100
               MOVE 8 TO LS-RETURN-CODE
               STRING "Invalid year: " WS-YEAR
                   INTO LS-ERROR-MESSAGE
               GOBACK
           END-IF
           
           IF WS-MONTH < 1 OR WS-MONTH > 12
               MOVE 8 TO LS-RETURN-CODE
               STRING "Invalid month: " WS-MONTH
                   INTO LS-ERROR-MESSAGE
               GOBACK
           END-IF
           
           IF WS-DAY < 1 OR WS-DAY > 31
               MOVE 8 TO LS-RETURN-CODE
               STRING "Invalid day: " WS-DAY
                   INTO LS-ERROR-MESSAGE
               GOBACK
           END-IF
           
      
           PERFORM CHECK-LEAP-YEAR
           IF LEAP-YEAR
               MOVE 29 TO WS-FEB-DAY-CHECK
           ELSE
               MOVE 28 TO WS-FEB-DAY-CHECK
           END-IF

           IF WS-MONTH = 2 AND WS-DAY > WS-FEB-DAY-CHECK
                   MOVE 8 TO LS-RETURN-CODE
                   STRING  "February for leap year: " WS-IS-LEAP-YEAR
                           ", cannot have > " WS-FEB-DAY-CHECK " days."
                   INTO LS-ERROR-MESSAGE
                   END-STRING
                   GOBACK
           END-IF

      *>   30-day months
           IF (WS-MONTH = 4 OR 6 OR 9 OR 11) AND WS-DAY > 30
               MOVE 8 TO LS-RETURN-CODE
               STRING  "Month " WS-MONTH
                       "cannot have > 30 days."
               INTO LS-ERROR-MESSAGE
               END-STRING

               GOBACK
           END-IF

           GOBACK
           . 

           CHECK-LEAP-YEAR.
           *> Div by 400 => leap year.
           IF FUNCTION REM(WS-YEAR, 400) = 0
               SET LEAP-YEAR TO TRUE
               EXIT PARAGRAPH
           END-IF
    
           *> Div by 100 => NOT leap year.
           IF FUNCTION REM(WS-YEAR, 100) = 0
               SET NOT-LEAP-YEAR TO TRUE
           EXIT PARAGRAPH
           END-IF
    
           *> Div by 4 => leap year.
           IF FUNCTION REM(WS-YEAR, 4) = 0
               SET LEAP-YEAR TO TRUE
               EXIT PARAGRAPH
           END-IF
    
           *> Otherwise => NOT leap year.
           SET NOT-LEAP-YEAR TO TRUE
           .
           
       END PROGRAM DATEUTIL.