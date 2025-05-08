# Mainframe Concepts

This section introduces mainframe concepts and how they relate to COBOL programming. While you may be developing locally, understanding these concepts is crucial for working with mainframe systems.

## Introduction to z/OS

z/OS is IBM's mainframe operating system, designed for:

- High availability
- Security
- Scalability
- Batch processing
- Transaction processing

### Key Components

1. **TSO (Time Sharing Option)**

   - Interactive user interface
   - Command-line environment
   - File management

2. **ISPF (Interactive System Productivity Facility)**

   - Menu-driven interface
   - Text editor
   - Dataset management

3. **JES (Job Entry Subsystem)**
   - Job scheduling
   - Spooling
   - Output management

## Job Control Language (JCL)

JCL is the scripting language used to run programs on z/OS. It defines:

- Program execution
- Dataset allocation
- System resources
- Job parameters

### Basic JCL Structure

```jcl
//JOBNAME  JOB (ACCT),'DESCRIPTION',
//         CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//STEP1    EXEC PGM=COBOLPGM
//SYSOUT   DD SYSOUT=*
//INPUT    DD DSN=INPUT.DATASET,DISP=SHR
//OUTPUT   DD DSN=OUTPUT.DATASET,DISP=(NEW,CATLG),
//         SPACE=(TRK,(10,10)),LRECL=80
```

### Common JCL Statements

1. **JOB Statement**

   ```jcl
   //JOBNAME  JOB (ACCT),'DESCRIPTION',
   //         CLASS=A,MSGCLASS=X
   ```

2. **EXEC Statement**

   ```jcl
   //STEP1    EXEC PGM=COBOLPGM
   ```

3. **DD Statement**
   ```jcl
   //INPUT    DD DSN=INPUT.DATASET,DISP=SHR
   ```

### Dataset Allocation

```jcl
//OUTPUT   DD DSN=OUTPUT.DATASET,
//         DISP=(NEW,CATLG),
//         SPACE=(TRK,(10,10)),
//         LRECL=80,
//         RECFM=FB
```

## Dataset Concepts

### Dataset Types

1. **Sequential Datasets**

   - Records in sequence
   - Common for batch processing
   - Similar to local files

2. **Partitioned Datasets (PDS)**

   - Collection of members
   - Like a directory
   - Common for source code

3. **VSAM Datasets**
   - Indexed access
   - Key-sequenced
   - Relative record

### Dataset Naming

```
HLQ.SUBHLQ.DATASET
```

- HLQ: High-level qualifier
- SUBHLQ: Sub-qualifier
- DATASET: Dataset name

## Batch Processing

### Batch Job Flow

1. **Job Submission**

   ```jcl
   //BATCHJOB JOB (ACCT),'BATCH PROCESS',
   //         CLASS=A,MSGCLASS=X
   //STEP1    EXEC PGM=SORT
   //SORTIN   DD DSN=INPUT.DATASET,DISP=SHR
   //SORTOUT  DD DSN=OUTPUT.DATASET,
   //         DISP=(NEW,CATLG)
   //SYSIN    DD *
     SORT FIELDS=(1,5,CH,A)
   /*
   ```

2. **Program Execution**

   ```jcl
   //STEP2    EXEC PGM=COBOLPGM
   //INPUT    DD DSN=OUTPUT.DATASET,DISP=SHR
   //OUTPUT   DD SYSOUT=*
   ```

3. **Output Processing**
   ```jcl
   //STEP3    EXEC PGM=IEBGENER
   //SYSUT1   DD DSN=OUTPUT.DATASET,DISP=SHR
   //SYSUT2   DD SYSOUT=*
   ```

## CICS Overview

CICS (Customer Information Control System) is IBM's transaction processing system.

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

## DB2 Integration

### Embedded SQL in COBOL

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

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           EXEC SQL
               SELECT CUST_ID, CUST_NAME
               INTO :WS-CUST-ID, :WS-CUST-NAME
               FROM CUSTOMER
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC

           IF SQLCODE = 0
               DISPLAY "Customer found: " WS-CUST-NAME
           ELSE
               DISPLAY "Customer not found"
           END-IF.
```

## Local Development vs. Mainframe

### Key Differences

1. **File Systems**

   - Local: Regular file system
   - Mainframe: Dataset system

2. **Program Execution**

   - Local: Direct execution
   - Mainframe: JCL-controlled

3. **Resource Management**
   - Local: OS-managed
   - Mainframe: JES-managed

### Simulating Mainframe Environment

1. **Dataset Simulation**

   ```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
           ASSIGN TO "CUSTOMER.DAT"
           ORGANIZATION IS SEQUENTIAL.
   ```

2. **JCL Simulation**

   ```bash
   #!/bin/bash
   # Simulate JCL parameters
   export INPUT_DATASET="input.dat"
   export OUTPUT_DATASET="output.dat"
   export LRECL=80

   # Run COBOL program
   ./cobolpgm
   ```

## Best Practices

1. **Program Design**

   - Consider mainframe constraints
   - Plan for batch processing
   - Handle errors appropriately

2. **Dataset Management**

   - Use proper naming conventions
   - Consider space requirements
   - Plan for growth

3. **Performance**
   - Optimize I/O operations
   - Use appropriate access methods
   - Consider system resources

## Next Steps

Now that you understand mainframe concepts, proceed to [Advanced Topics](05-advanced-topics.md) to learn about CICS, DB2, and performance considerations.

## Additional Resources

- [IBM z/OS Documentation](https://www.ibm.com/docs/en/zos)
- [JCL Reference](https://www.ibm.com/docs/en/zos/2.4.0)
- [CICS Documentation](https://www.ibm.com/docs/en/cics-ts)
- [DB2 for z/OS Documentation](https://www.ibm.com/docs/en/db2-for-zos)
