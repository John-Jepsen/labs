# File Operations in COBOL

This section covers file handling in COBOL, including different file organizations, access methods, and practical examples for both local development and mainframe environments.

## File Organizations

COBOL supports several file organizations:

1. **Sequential Files**

   - Records are accessed in sequence
   - Common for batch processing
   - Simple to implement

2. **Indexed Files**

   - Records accessed by key
   - Supports random access
   - Requires key field

3. **Relative Files**
   - Records accessed by relative record number
   - Good for fixed-length records
   - Direct access capability

## File Definition

### Basic File Definition

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
           ASSIGN TO "customers.dat"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05  CUST-ID        PIC 9(5).
           05  CUST-NAME      PIC X(30).
           05  CUST-BALANCE   PIC 9(7)V99.
```

### Indexed File Definition

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
           ASSIGN TO "customers.idx"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS CUST-ID
           FILE STATUS IS WS-FILE-STATUS.
```

## File Operations

### Opening Files

```cobol
       *> Open for input
       OPEN INPUT CUSTOMER-FILE

       *> Open for output
       OPEN OUTPUT CUSTOMER-FILE

       *> Open for input-output
       OPEN I-O CUSTOMER-FILE

       *> Open for extend
       OPEN EXTEND CUSTOMER-FILE
```

### Reading Files

```cobol
       *> Sequential read
       READ CUSTOMER-FILE
           AT END DISPLAY "End of file"
           NOT AT END
               DISPLAY "Customer: " CUST-NAME
       END-READ

       *> Random read (for indexed files)
       MOVE "12345" TO CUST-ID
       READ CUSTOMER-FILE
           INVALID KEY DISPLAY "Record not found"
           NOT INVALID KEY
               DISPLAY "Customer: " CUST-NAME
       END-READ
```

### Writing Records

```cobol
       *> Write a record
       MOVE "12345" TO CUST-ID
       MOVE "John Doe" TO CUST-NAME
       MOVE 1000.50 TO CUST-BALANCE
       WRITE CUSTOMER-RECORD
           INVALID KEY DISPLAY "Write failed"
           NOT INVALID KEY
               DISPLAY "Record written"
       END-WRITE
```

### Updating Records

```cobol
       *> Update existing record
       MOVE "12345" TO CUST-ID
       READ CUSTOMER-FILE
           INVALID KEY DISPLAY "Record not found"
           NOT INVALID KEY
               MOVE 2000.75 TO CUST-BALANCE
               REWRITE CUSTOMER-RECORD
                   INVALID KEY DISPLAY "Update failed"
                   NOT INVALID KEY
                       DISPLAY "Record updated"
               END-REWRITE
       END-READ
```

### Deleting Records

```cobol
       *> Delete a record
       MOVE "12345" TO CUST-ID
       DELETE CUSTOMER-FILE
           INVALID KEY DISPLAY "Delete failed"
           NOT INVALID KEY
               DISPLAY "Record deleted"
       END-DELETE
```

## File Status Codes

Common file status codes:

- "00": Successful operation
- "10": End of file
- "23": Record not found
- "35": File not found
- "39": File already exists
- "41": File already open
- "42": File not open

### Status Code Handling

```cobol
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS    PIC X(2).
       01  WS-ERROR-MESSAGE  PIC X(50).

       PROCEDURE DIVISION.
       CHECK-FILE-STATUS.
           EVALUATE WS-FILE-STATUS
               WHEN "00"
                   CONTINUE
               WHEN "10"
                   MOVE "End of file reached" TO WS-ERROR-MESSAGE
               WHEN "23"
                   MOVE "Record not found" TO WS-ERROR-MESSAGE
               WHEN OTHER
                   MOVE "File error occurred" TO WS-ERROR-MESSAGE
           END-EVALUATE
           DISPLAY WS-ERROR-MESSAGE
```

## Practical Examples

### Batch Processing Example

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH-PROCESS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
           ASSIGN TO "input.dat"
           ORGANIZATION IS SEQUENTIAL.

           SELECT OUTPUT-FILE
           ASSIGN TO "output.dat"
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  IN-ACCOUNT    PIC 9(10).
           05  IN-AMOUNT     PIC 9(7)V99.

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05  OUT-ACCOUNT   PIC 9(10).
           05  OUT-AMOUNT    PIC 9(7)V99.
           05  OUT-STATUS    PIC X(1).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM PROCESS-RECORDS UNTIL WS-EOF = "Y"

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       PROCESS-RECORDS.
           READ INPUT-FILE
               AT END MOVE "Y" TO WS-EOF
               NOT AT END
                   PERFORM PROCESS-SINGLE-RECORD
           END-READ.

       PROCESS-SINGLE-RECORD.
           MOVE IN-ACCOUNT TO OUT-ACCOUNT
           MOVE IN-AMOUNT TO OUT-AMOUNT
           IF IN-AMOUNT > 1000
               MOVE "H" TO OUT-STATUS
           ELSE
               MOVE "L" TO OUT-STATUS
           END-IF
           WRITE OUTPUT-RECORD.
```

## Best Practices

1. **Error Handling**

   - Always check file status
   - Implement proper error messages
   - Handle all possible conditions

2. **File Organization**

   - Choose appropriate organization
   - Consider access patterns
   - Plan for future growth

3. **Performance**

   - Use appropriate access mode
   - Buffer records when possible
   - Close files properly

4. **Security**
   - Validate file names
   - Check file permissions
   - Handle sensitive data properly

## Next Steps

Now that you understand file operations, proceed to [Mainframe Concepts](04-mainframe-concepts.md) to learn about mainframe-specific considerations.

## Additional Resources

- [COBOL File Handling Guide](https://www.tutorialspoint.com/cobol/cobol_file_handling.htm)
- [GnuCOBOL File Operations](https://gnucobol.sourceforge.io/guides.html)
- [IBM COBOL File Handling](https://www.ibm.com/docs/en/cobol-zos)
