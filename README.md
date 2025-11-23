# Task 3: Modular Customer Account System

## Overview
Build a multi-program customer account management system using indexed files for random access, copybooks for shared layouts, and subprograms for business logic.

## Architecture
```
CUSTMAIN.COB (Main menu program)
    ├── CUSTADDD.COB (Add customer subprogram)
    ├── CUSTUPDT.COB (Update balance subprogram)
    ├── CUSTINQ.COB (Inquiry subprogram)
    ├── CUSTREPT.COB (Report generator subprogram)
    └── DATEUTIL.COB (Date validation utility)
    
Copybooks:
    ├── CUSTMAST.CPY (Customer master record layout)
    ├── TRANREC.CPY (Transaction record layout)
    └── STATCODE.CPY (Status codes)
```

## Requirements

### Part 1: Indexed Master File
Create indexed file `CUSTOMER.DAT` with:
- Customer ID as primary key (random access)
- Email as alternate key (must be unique)
- Support for READ by either key
- Record status tracking (Active/Suspended/Closed)

### Part 2: Transaction Processing
Process batch transactions from `TRANSACTIONS.TXT`:
- Deposits (D)
- Withdrawals (W)  
- Account creation (N)
- Account closure (C)
- Status changes (S)

Each transaction must:
- Validate against master file
- Check business rules (no overdrafts, suspended accounts can't transact)
- Update master record
- Write to audit trail

### Part 3: Modular Structure

**COPYBOOK Requirements (CUSTMAST.CPY):**
```cobol
      * Customer Master Record
       01  CUSTOMER-RECORD.
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
               10  CUST-TRAN-COUNT  PIC S9(7) COMP.
               10  CUST-TOTAL-DEPS  PIC S9(11)V99 COMP-3.
               10  CUST-TOTAL-WTHS  PIC S9(11)V99 COMP-3.
```

**SUBPROGRAM Requirements:**

1. **DATEUTIL.COB** - Called by all programs
   - Validate date format (YYYYMMDD)
   - Calculate days between dates
   - Format date for display
   - Parameters passed via LINKAGE SECTION

2. **CUSTADDD.COB** - Add new customer
   - Generate next customer ID automatically
   - Validate email uniqueness
   - Set initial values
   - Return status code to caller

3. **CUSTUPDT.COB** - Update balances
   - Apply transaction with validation
   - Update statistics
   - Handle concurrent access (record locking)
   - Log all changes

### Part 4: Main Menu Program
Interactive menu system:
```
    CUSTOMER ACCOUNT SYSTEM
    =======================
    1. Add New Customer
    2. Update Account Balance
    3. Customer Inquiry
    4. Generate Reports
    5. Process Batch Transactions
    6. Exit
    
    Enter choice: _
```

Each option calls appropriate subprogram using:
```cobol
CALL 'CUSTADDD' USING CUST-RECORD, RETURN-CODE.
```

### Part 5: Advanced Features

**Record Locking:**
```cobol
READ CUSTOMER-FILE WITH LOCK
    INVALID KEY
        SET CUSTOMER-NOT-FOUND TO TRUE
    NOT INVALID KEY
        PERFORM PROCESS-UPDATE
END-READ
```

**Email Alternate Key:**
```cobol
MOVE user-email TO CUST-EMAIL
READ CUSTOMER-FILE
    KEY IS CUST-EMAIL
    INVALID KEY
        DISPLAY "Email not found"
END-READ
```

**Transaction Validation Chain:**
- Check customer exists
- Verify account status
- Validate amount/limits
- Check date logic (no future dates)
- Apply business rules
- Update master
- Write audit

### File Formats

**CUSTOMER.DAT** (Indexed, binary format - created by program)

**TRANSACTIONS.TXT** (Sequential input):
```
N|C000000001|smith@email.com|Smith|John|123 Main St|New York|NY|10001|5000.00|10000.00
D|C000000001|1500.00|20240315|Payroll deposit
W|C000000001|200.00|20240316|ATM withdrawal
S|C000000001|S|20240320|Suspicious activity
D|C000000001|1500.00|20240320|Deposit declined - suspended
```

### Deliverables

1. **Source Files:**
   - CUSTMAIN.COB (main menu)
   - CUSTADDD.COB (add customer)
   - CUSTUPDT.COB (update balance)
   - CUSTINQ.COB (inquiry)
   - CUSTREPT.COB (reports)
   - DATEUTIL.COB (date utilities)

2. **Copybooks:**
   - CUSTMAST.CPY (record layout)
   - TRANREC.CPY (transaction layout)
   - STATCODE.CPY (return codes)

3. **Test Data:**
   - TRANSACTIONS.TXT (minimum 20 transactions)
   - Include error cases (overdrafts, invalid dates, missing customers)

4. **Reports:**
   - Customer listing (sorted by balance descending)
   - Transaction audit trail
   - Error report for failed transactions

### Technical Requirements

- Use `ORGANIZATION IS INDEXED` for master file
- Implement proper file locking for updates
- All numeric amounts use COMP-3
- Date validation must reject invalid dates (like 20240231)
- Use 88-levels for all status flags
- CALL statements with USING phrase for parameters
- COPY statements for all shared structures
- Return codes: 0=Success, 4=Warning, 8=Error, 12=Severe

### Success Criteria

- [ ] Indexed file with working primary/alternate keys
- [ ] All subprograms compile separately
- [ ] COPY books properly included
- [ ] Menu system calls subprograms correctly
- [ ] Transaction validation catches all error cases
- [ ] Record locking prevents corruption
- [ ] Audit trail captures all changes
- [ ] Reports generate correctly

### Bonus Challenges

1. Add START verb for browsing customers alphabetically
2. Implement REWRITE with optimistic locking
3. Add transaction rollback on errors
4. Create backup/restore functionality
5. Add password protection for sensitive operations

### Compilation Notes
```
# Compile copybooks first (just syntax check)
cobc -fsyntax-only CUSTMAST.CPY

# Compile subprograms as modules
cobc -c DATEUTIL.COB
cobc -c CUSTADDD.COB  
cobc -c CUSTUPDT.COB

# Compile and link main program
cobc -x CUSTMAIN.COB DATEUTIL.o CUSTADDD.o CUSTUPDT.o
```
