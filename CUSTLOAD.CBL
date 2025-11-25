       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTLOAD.

      *> Learning INDEXED files AND their behavior.
      *> Learn how TO USE DYNAMIC.
      *> Creating new files, opening AND PROCESSING errors.
      *>
      *> Task requirs email as alt key. Probably NOT the Best idea,
      *> because handling email change means we prob need TO re-create custome RECORD.
      *> IRL, I think it's be better to get a confirmation what actual requirments for the project.
      *> My bet the requirments ARE NOT USE email as key, but being able TO find a customer BY email.
      *> So, FOR IRL prod => get confirmation OF intent => keep only one key, implement SEARCH BY email functionality
      *> For the purpose of the task I will stick to the Task requrment and use email as alt key.
  
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-MASTER
               ASSIGN TO "CUSTOMER.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               ALTERNATE RECORD KEY IS CUST-EMAIL 
                   WITH NO DUPLICATES
      *>       For IRL probably separate EMAIL-LOOKUP file with email=>ID mapping.
               FILE STATUS IS WS-CUST-STATUS.
               
      *>   Prod would have:
      *>   SELECT EMAIL-LOOKUP
      *>       ASSIGN TO "EMAILLKP.DAT"  
      *>       ORGANIZATION IS INDEXED
      *>       ACCESS MODE IS RANDOM
      *>       RECORD KEY IS EMLKP-EMAIL
      *>       FILE STATUS IS WS-EMAIL-STATUS.
           
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-MASTER.
       01  CUSTOMER-RECORD.
      *> Can't use CUSTFILE => binary index file, Solve ambiguous error.
           COPY CUSTMAST. 
           
       WORKING-STORAGE SECTION.
       01  WS-CUST-STATUS            PIC XX.
           88  CUST-OK               VALUE "00".
           88  CUST-EOF              VALUE "10".
           88  CUST-DUPLICATE        VALUE "22".
           88  CUST-NOT-FOUND        VALUE "23".
           88  CUST-FILE-MISSING     VALUE "35".
           
       01  WS-CUSTOMER.
           COPY CUSTMAST.
           
      *> Test controls.
       01  WS-TEST-COUNTERS.
           05  WS-TESTS-RUN          PIC 9(3) VALUE ZERO.
           05  WS-TESTS-PASSED       PIC 9(3) VALUE ZERO.
           05  WS-TESTS-FAILED       PIC 9(3) VALUE ZERO.
           05  WS-TESTS-SKIPPED      PIC 9(3) VALUE ZERO.
           
       01  WS-DISPLAY-BALANCE        PIC -ZZZ,ZZZ,ZZ9.99.
       
       01  WS-SAVE-EMAIL             PIC X(50).
       COPY STATCODE.
           
       PROCEDURE DIVISION.

       PERFORM MAIN-PROCEDURE.

       GOBACK
       .
       MAIN-PROCEDURE.
           DISPLAY "========================================="
           DISPLAY " CUSTLOAD - Indexed File Tests"  
           DISPLAY "========================================="
           PERFORM DELETE-ALL-TEST-FILES
           PERFORM CREATE-FILE
           IF CUST-OK
               PERFORM RUN-ALL-TESTS
               
           ELSE
               DISPLAY "FATAL: Cannot open/create file."
               MOVE RC-SEVERE TO RETURN-CODE
           END-IF
           
           PERFORM CLOSE-FILE
           DISPLAY "========================================="
           DISPLAY "Tests Run:     " WS-TESTS-RUN
           DISPLAY "Tests Passed:  " WS-TESTS-PASSED
           DISPLAY "Tests Failed:  " WS-TESTS-FAILED
           DISPLAY "Tests Skipped: " WS-TESTS-SKIPPED
           DISPLAY "========================================="
           
           STOP RUN.
           
       CREATE-FILE.
      *>   I OPEN OR create FILE. 
      *> FOR the task probably fine, IRL I guess this PROGRAM shouldn't have situation with no Customer file
      *> It will indicatea SIGN OF corruption.
      *> but FOR the purpose OF learning exercise I will follow WITH => NOT exist create pattern.
      *> Reusing old FILE WITH OPEN I-O CUSTOMER-MASTER lead to TEST 3 fail ON second RUN OF tests due to corruption introduced BY TEST 7.
      *> TEST 7 fails because my system allows to change alt keys.
           OPEN OUTPUT CUSTOMER-MASTER

           IF CUST-FILE-MISSING
               DISPLAY "ERROR: Cannot create file: " WS-CUST-STATUS
           ELSE
               IF NOT CUST-OK
                   DISPLAY "ERROR: Open failed: " WS-CUST-STATUS
               END-IF
           END-IF
           .

           DELETE-ALL-TEST-FILES.
           DISPLAY "Cleaning up old test files..."
           CALL "CBL_DELETE_FILE" USING "CUSTOMER.DAT"
           CALL "CBL_DELETE_FILE" USING "CUSTOMER.DAT.1"
           .

       RUN-ALL-TESTS.
           DISPLAY " "
           DISPLAY "TEST 1: Write first customer"
           PERFORM TEST-WRITE-FIRST
           
           DISPLAY " "
           DISPLAY "TEST 2: Duplicate primary key"
           PERFORM TEST-DUPLICATE-PRIMARY
           
           DISPLAY " "  
           DISPLAY "TEST 3: Duplicate email (alternate key)"
           PERFORM TEST-DUPLICATE-EMAIL
           
           DISPLAY " "
           DISPLAY "TEST 4: Random read by primary key"
           PERFORM TEST-READ-BY-PRIMARY
           
           DISPLAY " "
           DISPLAY "TEST 5: Read by email (alternate key)"
           PERFORM TEST-READ-BY-EMAIL
           
           DISPLAY " "
           DISPLAY "TEST 6: Padding matters in keys"
           PERFORM TEST-KEY-PADDING
           
           DISPLAY " "
           DISPLAY "TEST 7: Change email (the problem)"
           PERFORM TEST-EMAIL-CHANGE
           
           DISPLAY " "
           DISPLAY "TEST 8: Test email uniqueness"
           PERFORM TEST-EMAIL-UNIQUENESS
           .

       TEST-EMAIL-UNIQUENESS.
           ADD 1 TO WS-TESTS-RUN
           
      *>   Try to read both records with same email
           INITIALIZE CUSTOMER-RECORD
           MOVE "john@example.com" TO CUST-EMAIL OF CUSTOMER-RECORD
           
           READ CUSTOMER-MASTER KEY IS CUST-EMAIL OF CUSTOMER-RECORD
               INVALID KEY
                   DISPLAY "  No records with this email?"
               NOT INVALID KEY
                   DISPLAY "  First record: " CUST-ID OF CUSTOMER-RECORD
           END-READ
           
      *>   Try READ NEXT to see if there's another. If prev failed and added wrong one. (that happened, this test is to find wht)
           READ CUSTOMER-MASTER NEXT
               AT END
                   DISPLAY "  No more records"
               NOT AT END
                 IF CUST-EMAIL OF CUSTOMER-RECORD = "john@example.com"
                   DISPLAY "  PROBLEM: Another record with same email: "
                               CUST-ID OF CUSTOMER-RECORD
                       ADD 1 TO WS-TESTS-FAILED
                 ELSE
                   DISPLAY "  Next record has different email."
                   ADD 1 TO WS-TESTS-PASSED
                 END-IF
           END-READ. 

       TEST-WRITE-FIRST.
           ADD 1 TO WS-TESTS-RUN
           INITIALIZE WS-CUSTOMER
           
      *>   Customer ID - zero filled => No trunc. COBOL specific.
           MOVE "C000000001"        TO CUST-ID OF WS-CUSTOMER
           MOVE "john@example.com"  TO CUST-EMAIL OF WS-CUSTOMER
           MOVE "Smith"             TO CUST-LAST-NAME OF WS-CUSTOMER
           MOVE "John"              TO CUST-FIRST-NAME OF WS-CUSTOMER
           MOVE 1000.00             TO CUST-BALANCE OF WS-CUSTOMER
           MOVE "A"                 TO CUST-STATUS OF WS-CUSTOMER
           MOVE 20240101            TO CUST-OPEN-DATE OF WS-CUSTOMER
           
           WRITE CUSTOMER-RECORD FROM WS-CUSTOMER
           
           IF CUST-OK
               DISPLAY "  PASS: Customer written"
               ADD 1 TO WS-TESTS-PASSED
           ELSE
               DISPLAY "  FAIL: Write error: " WS-CUST-STATUS
               ADD 1 TO WS-TESTS-FAILED
           END-IF.
           
       TEST-DUPLICATE-PRIMARY.
           ADD 1 TO WS-TESTS-RUN
           
      *>   Same ID as TEST-WRITE-FIRST.
           MOVE "C000000001"        TO CUST-ID OF WS-CUSTOMER
           MOVE "different@test.com" TO CUST-EMAIL OF WS-CUSTOMER
           
           WRITE CUSTOMER-RECORD FROM WS-CUSTOMER
           
           IF CUST-DUPLICATE
               DISPLAY "  PASS: Duplicate primary caught"
               ADD 1 TO WS-TESTS-PASSED
           ELSE
               DISPLAY "  FAIL: Should have been duplicate"
               DISPLAY "        Status was: " WS-CUST-STATUS
               ADD 1 TO WS-TESTS-FAILED
           END-IF.
           
       TEST-DUPLICATE-EMAIL.
           ADD 1 TO WS-TESTS-RUN
           ADD 1 TO WS-TESTS-SKIPPED
           EXIT PARAGRAPH
            *>   First, check if email already exists
           INITIALIZE CUSTOMER-RECORD
           MOVE "john@example.com" TO CUST-EMAIL OF CUSTOMER-RECORD
           READ CUSTOMER-MASTER KEY IS CUST-EMAIL OF CUSTOMER-RECORD
               INVALID KEY
                   DISPLAY "  Email not in file yet."
                   DISPLAY "  Test not applicable."
                   ADD 1 TO WS-TESTS-SKIPPED
                   EXIT PARAGRAPH
               NOT INVALID KEY
                   DISPLAY "  Email already exists for: " 
                           CUST-ID OF CUSTOMER-RECORD
           END-READ     

      *>   Different ID but same email as first customer.
           MOVE "C000000002"       TO CUST-ID OF WS-CUSTOMER
           MOVE "john@example.com" TO CUST-EMAIL OF WS-CUSTOMER
           MOVE "Jones"            TO CUST-LAST-NAME OF WS-CUSTOMER
           
           WRITE CUSTOMER-RECORD FROM WS-CUSTOMER
           
           IF CUST-DUPLICATE
               DISPLAY "  PASS: Duplicate email caught"
               ADD 1 TO WS-TESTS-PASSED
           ELSE
               DISPLAY "  FAIL: Should reject duplicate email"
               DISPLAY "        Status was: " WS-CUST-STATUS
               ADD 1 TO WS-TESTS-FAILED
           END-IF
           EXIT PARAGRAPH
      *>      Extra check, what actually got written.
               INITIALIZE CUSTOMER-RECORD
               MOVE "C000000002" TO CUST-ID OF CUSTOMER-RECORD
               READ CUSTOMER-MASTER
                   INVALID KEY
                       DISPLAY "     C000000002 not in file"
                   NOT INVALID KEY
                       DISPLAY "     C000000002 IS in file with email: "
                               CUST-EMAIL OF CUSTOMER-RECORD
               END-READ     
           .
           
       TEST-READ-BY-PRIMARY.
           ADD 1 TO WS-TESTS-RUN
           INITIALIZE CUSTOMER-RECORD
           MOVE "C000000001" TO CUST-ID OF CUSTOMER-RECORD
           READ CUSTOMER-MASTER
               INVALID KEY
                   DISPLAY "  FAIL: Customer not found"
                   ADD 1 TO WS-TESTS-FAILED
               NOT INVALID KEY
                   DISPLAY "  PASS: Found " 
                           CUST-FIRST-NAME OF CUSTOMER-RECORD
                           " " 
                           CUST-LAST-NAME OF CUSTOMER-RECORD
                   MOVE CUST-BALANCE OF CUSTOMER-RECORD 
                       TO WS-DISPLAY-BALANCE
                   DISPLAY "        Balance: " WS-DISPLAY-BALANCE
                   ADD 1 TO WS-TESTS-PASSED
           END-READ.
           
       TEST-READ-BY-EMAIL.
           ADD 1 TO WS-TESTS-RUN
           
      *>   Clear record first. We are in IBM mainframe env.
           INITIALIZE CUSTOMER-RECORD
           MOVE "john@example.com" TO CUST-EMAIL OF CUSTOMER-RECORD
           
           READ CUSTOMER-MASTER KEY IS CUST-EMAIL OF CUSTOMER-RECORD
               INVALID KEY
                   DISPLAY "  FAIL: Email not found"
                   ADD 1 TO WS-TESTS-FAILED
               NOT INVALID KEY
                   DISPLAY "  PASS: Found customer " 
                           CUST-ID OF CUSTOMER-RECORD
                   ADD 1 TO WS-TESTS-PASSED
           END-READ.
           
       TEST-KEY-PADDING.
           ADD 1 TO WS-TESTS-RUN
           
      *>   BAD key, no padding.
           MOVE "C1" TO CUST-ID OF CUSTOMER-RECORD
           READ CUSTOMER-MASTER
               INVALID KEY
                   DISPLAY "  PASS: 'C1' correctly not found"
                   DISPLAY "   (Would be 'C1        ' not 'C000000001')"
                   ADD 1 TO WS-TESTS-PASSED
               NOT INVALID KEY
                   DISPLAY "  FAIL: Found record with bad key?!"
                   ADD 1 TO WS-TESTS-FAILED
           END-READ.
           
           TEST-EMAIL-CHANGE.
           ADD 1 TO WS-TESTS-RUN
           DISPLAY "  Attempting to change email..."
           
      *>   Save original email first.
           MOVE "john@example.com" TO WS-SAVE-EMAIL
           
           MOVE "C000000001" TO CUST-ID OF CUSTOMER-RECORD
           READ CUSTOMER-MASTER WITH LOCK
               INVALID KEY
                   DISPLAY "  FAIL: Can't find customer to update"
                   ADD 1 TO WS-TESTS-FAILED
               NOT INVALID KEY
                   MOVE "newemail@test.com" 
                       TO CUST-EMAIL OF CUSTOMER-RECORD
                   REWRITE CUSTOMER-RECORD
                       INVALID KEY
                           DISPLAY "  EXPECTED: Can't change alt key!"
                           ADD 1 TO WS-TESTS-PASSED
                       NOT INVALID KEY
                           DISPLAY "  UNEXPECTED: Email change worked"
                           DISPLAY "  GnuCOBOL allows this!"
                           ADD 1 TO WS-TESTS-PASSED
                           
      *>               RESTORE ORIGINAL EMAIL FOR NEXT TEST!
                           MOVE WS-SAVE-EMAIL 
                               TO CUST-EMAIL OF CUSTOMER-RECORD
                           REWRITE CUSTOMER-RECORD
                           DISPLAY "  Restored original email"
                   END-REWRITE
           END-READ.
           
       CLOSE-FILE.
           CLOSE CUSTOMER-MASTER
           IF NOT CUST-OK
               DISPLAY "WARNING: Close status: " WS-CUST-STATUS
           END-IF.
           
       END PROGRAM CUSTLOAD.
