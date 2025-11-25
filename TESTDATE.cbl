       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTDATE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TEST-DATE          PIC 9(8).
       01  WS-RETURN-CODE        PIC S9(4) COMP.
       01  WS-ERROR-MSG          PIC X(80).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Testing DATEUTIL..."
           
      *>   Test valid date.
           MOVE 20240315 TO WS-TEST-DATE
           CALL 'DATEUTIL' USING WS-TEST-DATE
                                 WS-RETURN-CODE
                                 WS-ERROR-MSG
           
           IF WS-RETURN-CODE = ZERO
               DISPLAY "20240315 is valid (RIGHT)"
           ELSE
               DISPLAY "20240315 invalid: " WS-ERROR-MSG
           END-IF
           
      *>   Test invalid date.
           MOVE 20240231 TO WS-TEST-DATE  
           CALL 'DATEUTIL' USING WS-TEST-DATE
                                 WS-RETURN-CODE
                                 WS-ERROR-MSG
                                 
           IF WS-RETURN-CODE = ZERO
               DISPLAY "20240231 is valid (WRONG)"
           ELSE
               DISPLAY "20240231 invalid: " WS-ERROR-MSG
           END-IF
           
           STOP RUN.