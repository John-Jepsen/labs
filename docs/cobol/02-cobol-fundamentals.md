# COBOL Fundamentals

This section covers the fundamental concepts and structure of COBOL programming. Understanding these basics is crucial for both local development and mainframe programming.

## Program Structure

COBOL programs are divided into four main divisions:

1. **IDENTIFICATION DIVISION**
2. **ENVIRONMENT DIVISION**
3. **DATA DIVISION**
4. **PROCEDURE DIVISION**

### Example Structure

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUNDAMENTALS.
       AUTHOR. Your Name.
       DATE-WRITTEN. 2024-03-20.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER    PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Hello from COBOL!".
           STOP RUN.
```

## Fixed-Column Format

COBOL traditionally uses a fixed-column format:

- Columns 1-6: Line numbers (optional)
- Column 7: Continuation character (\* or -)
- Columns 8-11: Area A (division/section headers)
- Columns 12-72: Area B (program statements)
- Columns 73-80: Comments (optional)

### Example with Fixed Columns

```cobol
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. FIXED-FORMAT.
000300
000400 ENVIRONMENT DIVISION.
000500
000600 DATA DIVISION.
000700 WORKING-STORAGE SECTION.
000800 01  WS-VARIABLE    PIC X(10) VALUE "HELLO".
000900
001000 PROCEDURE DIVISION.
001100 MAIN-LOGIC.
001200     DISPLAY WS-VARIABLE.
001300     STOP RUN.
```

## Data Types and Variables

### Basic Data Types

1. **Numeric Types**

   ```cobol
   01  WS-NUMBER    PIC 9(5).        *> 5-digit number
   01  WS-DECIMAL   PIC 9(5)V99.     *> 5 digits, 2 decimal places
   ```

2. **Alphanumeric Types**

   ```cobol
   01  WS-NAME      PIC X(20).       *> 20 characters
   01  WS-TEXT      PIC A(10).       *> 10 alphabetic characters
   ```

3. **Group Items**
   ```cobol
   01  WS-ADDRESS.
       05  WS-STREET    PIC X(30).
       05  WS-CITY      PIC X(20).
       05  WS-STATE     PIC X(2).
       05  WS-ZIP       PIC 9(5).
   ```

### Level Numbers

- 01: Group level
- 02-49: Elementary items
- 66: Renames
- 77: Independent items
- 88: Condition names

## Basic Operations

### Arithmetic Operations

```cobol
       COMPUTE WS-RESULT = WS-NUM1 + WS-NUM2
       ADD WS-NUM1 TO WS-NUM2
       SUBTRACT WS-NUM1 FROM WS-NUM2
       MULTIPLY WS-NUM1 BY WS-NUM2
       DIVIDE WS-NUM1 BY WS-NUM2
```

### String Operations

```cobol
       STRING WS-FIRST-NAME DELIMITED BY SPACE
              " " DELIMITED BY SIZE
              WS-LAST-NAME DELIMITED BY SPACE
              INTO WS-FULL-NAME

       UNSTRING WS-FULL-NAME DELIMITED BY SPACE
                INTO WS-FIRST-NAME
                     WS-LAST-NAME
```

## Control Structures

### IF Statements

```cobol
       IF WS-AGE >= 18
           DISPLAY "Adult"
       ELSE
           DISPLAY "Minor"
       END-IF
```

### PERFORM Loops

```cobol
       *> Simple loop
       PERFORM 5 TIMES
           DISPLAY "Loop iteration"
       END-PERFORM

       *> Varying loop
       PERFORM VARYING WS-COUNTER FROM 1 BY 1
           UNTIL WS-COUNTER > 10
           DISPLAY "Count: " WS-COUNTER
       END-PERFORM
```

## File Handling Basics

### File Definition

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  IN-NAME    PIC X(20).
           05  IN-AGE     PIC 9(2).
```

### File Operations

```cobol
       PROCEDURE DIVISION.
       OPEN INPUT INPUT-FILE
       READ INPUT-FILE
           AT END DISPLAY "End of file"
           NOT AT END DISPLAY IN-NAME
       END-READ
       CLOSE INPUT-FILE
```

## Best Practices

1. **Naming Conventions**

   - Use meaningful names
   - Prefix working storage with WS-
   - Use consistent naming patterns

2. **Code Organization**

   - Group related data items
   - Use proper indentation
   - Include comments for complex logic

3. **Error Handling**
   - Check file status after operations
   - Validate input data
   - Use proper error messages

## Common Pitfalls

1. **Fixed-Column Format**

   - Remember column positions
   - Use proper indentation
   - Watch for continuation lines

2. **Data Types**

   - Choose appropriate PIC clauses
   - Consider data size limits
   - Handle decimal places correctly

3. **Control Flow**
   - Properly close IF statements
   - Use END-PERFORM for loops
   - Avoid infinite loops

## Next Steps

Now that you understand the fundamentals, proceed to [File Operations](03-file-operations.md) to learn about working with files in COBOL.

## Additional Resources

- [COBOL Language Reference](https://www.ibm.com/docs/en/cobol-zos)
- [GnuCOBOL Programmer's Guide](https://gnucobol.sourceforge.io/guides.html)
- [COBOL Programming Examples](https://www.tutorialspoint.com/cobol/cobol_program_structure.htm)
