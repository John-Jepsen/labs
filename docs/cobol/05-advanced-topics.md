# Advanced COBOL Topics

This section covers advanced COBOL concepts and their implementation in both local and mainframe environments.

## CICS Programming

### CICS Program Structure

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSPGM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COMMAREA.
           05  WS-TRANS-ID    PIC X(4).
           05  WS-AMOUNT      PIC 9(7)V99.
           05  WS-STATUS      PIC X(1).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           EXEC CICS HANDLE CONDITION
               ERROR(ERROR-ROUTINE)
           END-EXEC

           EXEC CICS RECEIVE
               INTO(WS-COMMAREA)
           END-EXEC

           PERFORM PROCESS-TRANSACTION

           EXEC CICS RETURN
           END-EXEC.
```

### CICS Commands

1. **Screen Handling**

   ```cobol
           EXEC CICS SEND
               FROM(WS-SCREEN)
               LENGTH(WS-LENGTH)
           END-EXEC
   ```

2. **Data Access**

   ```cobol
           EXEC CICS READ
               DATASET('CUSTOMER')
               INTO(WS-CUSTOMER)
               RIDFLD(WS-CUST-ID)
           END-EXEC
   ```

3. **Transaction Management**
   ```cobol
           EXEC CICS START
               TRANSID('TRN1')
               FROM(WS-DATA)
           END-EXEC
   ```

## DB2 Programming

### Program Structure with DB2

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2PGM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SQLCODE        PIC S9(9) COMP.
       01  WS-CUSTOMER.
           05  WS-CUST-ID    PIC 9(5).
           05  WS-CUST-NAME  PIC X(30).
           05  WS-BALANCE    PIC 9(7)V99.

       EXEC SQL
           INCLUDE SQLCA
       END-EXEC

       EXEC SQL
           DECLARE CUSTOMER_CURSOR CURSOR FOR
           SELECT CUST_ID, CUST_NAME, BALANCE
           FROM CUSTOMER
           WHERE BALANCE > :WS-MIN-BALANCE
       END-EXEC

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           EXEC SQL
               OPEN CUSTOMER_CURSOR
           END-EXEC

           PERFORM PROCESS-CUSTOMERS

           EXEC SQL
               CLOSE CUSTOMER_CURSOR
           END-EXEC

           STOP RUN.
```

### SQL Operations

1. **Select Statement**

   ```cobol
           EXEC SQL
               SELECT CUST_NAME, BALANCE
               INTO :WS-CUST-NAME, :WS-BALANCE
               FROM CUSTOMER
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC
   ```

2. **Insert Statement**

   ```cobol
           EXEC SQL
               INSERT INTO CUSTOMER
               (CUST_ID, CUST_NAME, BALANCE)
               VALUES
               (:WS-CUST-ID, :WS-CUST-NAME, :WS-BALANCE)
           END-EXEC
   ```

3. **Update Statement**
   ```cobol
           EXEC SQL
               UPDATE CUSTOMER
               SET BALANCE = :WS-NEW-BALANCE
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC
   ```

## Performance Optimization

### Program Optimization

1. **Data Access**

   ```cobol
       *> Use appropriate access method
       SELECT CUSTOMER-FILE
           ASSIGN TO "CUSTOMER.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS CUST-ID
   ```

2. **Memory Management**

   ```cobol
       *> Use appropriate data types
       01  WS-COUNTER    PIC 9(3) COMP-3.
       01  WS-AMOUNT     PIC 9(7)V99 COMP-3.
   ```

3. **I/O Optimization**
   ```cobol
       *> Buffer multiple records
       PERFORM READ-NEXT-RECORD
           UNTIL WS-EOF = "Y"
           OR WS-RECORD-COUNT >= 100
   ```

### SQL Optimization

1. **Index Usage**

   ```sql
   -- Create appropriate indexes
   CREATE INDEX CUST_IDX ON CUSTOMER(CUST_ID)
   ```

2. **Query Optimization**

   ```sql
   -- Use specific columns
   SELECT CUST_ID, CUST_NAME
   FROM CUSTOMER
   WHERE CUST_ID = :WS-CUST-ID
   ```

3. **Batch Processing**
   ```cobol
       *> Process multiple records
       EXEC SQL
           INSERT INTO CUSTOMER
           VALUES (:WS-CUSTOMER-ARRAY)
       END-EXEC
   ```

## Error Handling

### CICS Error Handling

```cobol
       ERROR-ROUTINE.
           EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE "Record not found" TO WS-ERROR-MSG
               WHEN OTHER
                   MOVE "System error" TO WS-ERROR-MSG
           END-EVALUATE

           EXEC CICS SEND
               FROM(WS-ERROR-MSG)
           END-EXEC

           EXEC CICS RETURN
           END-EXEC.
```

### DB2 Error Handling

```cobol
       CHECK-SQL-ERROR.
           IF SQLCODE < 0
               MOVE "SQL Error" TO WS-ERROR-MSG
               DISPLAY WS-ERROR-MSG
               DISPLAY "SQLCODE: " SQLCODE
               DISPLAY "SQLERRM: " SQLERRM
           END-IF.
```

## Security Considerations

### Data Protection

1. **Sensitive Data**

   ```cobol
       *> Mask sensitive data
       MOVE FUNCTION REVERSE(WS-ACCOUNT-NUM)
           TO WS-MASKED-ACCOUNT
   ```

2. **Access Control**
   ```cobol
       *> Check user authorization
       EXEC CICS QUERY SECURITY
           USERID(WS-USER-ID)
           RESULT(WS-AUTH-LEVEL)
       END-EXEC
   ```

### Transaction Security

```cobol
       SECURITY-CHECK.
           EXEC CICS VERIFY
               PASSWORD(WS-PASSWORD)
               USERID(WS-USER-ID)
           END-EXEC

           IF EIBRESP = DFHRESP(NORMAL)
               PERFORM PROCESS-TRANSACTION
           ELSE
               PERFORM SECURITY-ERROR
           END-IF.
```

## Testing and Debugging

### Unit Testing

```cobol
       TEST-SECTION.
           MOVE "12345" TO WS-CUST-ID
           PERFORM TEST-CUSTOMER-LOOKUP

           IF WS-RESULT = "SUCCESS"
               DISPLAY "Test passed"
           ELSE
               DISPLAY "Test failed"
           END-IF.
```

### Debugging Tools

1. **Trace Points**

   ```cobol
       *> Add trace points
       DISPLAY "DEBUG: " WS-VARIABLE
   ```

2. **Error Logging**
   ```cobol
       LOG-ERROR.
           OPEN EXTEND ERROR-LOG
           WRITE ERROR-RECORD
           CLOSE ERROR-LOG
   ```

## Best Practices

1. **Code Organization**

   - Use meaningful names
   - Document complex logic
   - Follow consistent patterns

2. **Performance**

   - Optimize data access
   - Use appropriate data types
   - Implement efficient algorithms

3. **Security**

   - Validate input data
   - Protect sensitive information
   - Implement proper access control

4. **Maintenance**
   - Write clear documentation
   - Include error handling
   - Plan for future changes

## Next Steps

Now that you understand advanced topics, proceed to [Practical Applications](06-practical-applications.md) to see how these concepts are applied in real-world scenarios.

## Additional Resources

- [CICS Programming Guide](https://www.ibm.com/docs/en/cics-ts)
- [DB2 Programming Guide](https://www.ibm.com/docs/en/db2-for-zos)
- [COBOL Performance Tuning](https://www.ibm.com/docs/en/cobol-zos)
- [Mainframe Security Best Practices](https://www.ibm.com/docs/en/zos)
