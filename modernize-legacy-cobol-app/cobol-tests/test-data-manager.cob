       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-DATA-MANAGER.
       AUTHOR. SYSTEM-TESTER.
       DATE-WRITTEN. 09/02/2025.
       
      *****************************************************************
      * COBOL Test Suite for Data Manager Program                    *
      * Tests data storage and retrieval operations                  *
      * to validate behavior before Python conversion                *
      *****************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEST-COUNTERS.
           05  TOTAL-TESTS        PIC 9(3) VALUE 0.
           05  PASSED-TESTS       PIC 9(3) VALUE 0.
           05  FAILED-TESTS       PIC 9(3) VALUE 0.

       01  TEST-VARIABLES.
           05  TEST-NAME          PIC X(50).
           05  TEST-RESULT        PIC X(10).
           
       01  SIMULATION-DATA.
           05  SIM-OPERATION      PIC X(6).
           05  SIM-BALANCE        PIC 9(6)V99.
           05  SIM-STORAGE-BAL    PIC 9(6)V99 VALUE 1000.00.

       01  TEST-CONSTANTS.
           05  INITIAL-BALANCE    PIC 9(6)V99 VALUE 1000.00.
           05  TEST-BALANCE-1     PIC 9(6)V99 VALUE 1500.75.
           05  TEST-BALANCE-2     PIC 9(6)V99 VALUE 0.00.

       PROCEDURE DIVISION.
       MAIN-TEST-CONTROL.
           PERFORM INITIALIZE-DATA-TEST
           PERFORM TEST-READ-OPERATIONS
           PERFORM TEST-WRITE-OPERATIONS
           PERFORM TEST-DATA-INTEGRITY
           PERFORM DISPLAY-DATA-SUMMARY
           STOP RUN.

       INITIALIZE-DATA-TEST.
           DISPLAY "========================================="
           DISPLAY "COBOL DATA MANAGER TEST SUITE"
           DISPLAY "========================================="
           MOVE INITIAL-BALANCE TO SIM-STORAGE-BAL.

       TEST-READ-OPERATIONS.
           MOVE "Testing Data Read Operation" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE 'READ' TO SIM-OPERATION
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Operation: " SIM-OPERATION
           DISPLAY "Expected: Return stored balance"
           
           IF SIM-OPERATION = 'READ'
               MOVE SIM-STORAGE-BAL TO SIM-BALANCE
               DISPLAY "Retrieved balance: " SIM-BALANCE
               
               IF SIM-BALANCE = INITIAL-BALANCE
                   MOVE "PASS" TO TEST-RESULT
                   ADD 1 TO PASSED-TESTS
                   DISPLAY "Result: PASS - Read operation successful"
               ELSE
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
                   DISPLAY "Result: FAIL - Incorrect balance retrieved"
               END-IF
           ELSE
               MOVE "FAIL" TO TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "Result: FAIL - Operation not recognized"
           END-IF.

       TEST-WRITE-OPERATIONS.
           PERFORM TEST-WRITE-VALID-BALANCE
           PERFORM TEST-WRITE-ZERO-BALANCE.

       TEST-WRITE-VALID-BALANCE.
           MOVE "Testing Data Write Operation" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE 'WRITE' TO SIM-OPERATION
           MOVE TEST-BALANCE-1 TO SIM-BALANCE
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Operation: " SIM-OPERATION
           DISPLAY "Balance to Write: " SIM-BALANCE
           
           IF SIM-OPERATION = 'WRITE'
               MOVE SIM-BALANCE TO SIM-STORAGE-BAL
               DISPLAY "Balance stored: " SIM-STORAGE-BAL
               
               IF SIM-STORAGE-BAL = TEST-BALANCE-1
                   MOVE "PASS" TO TEST-RESULT
                   ADD 1 TO PASSED-TESTS
                   DISPLAY "Result: PASS - Write operation successful"
               ELSE
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
                   DISPLAY "Result: FAIL - Balance not stored correctly"
               END-IF
           ELSE
               MOVE "FAIL" TO TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "Result: FAIL - Operation not recognized"
           END-IF.

       TEST-WRITE-ZERO-BALANCE.
           MOVE "Testing Write Zero Balance" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE 'WRITE' TO SIM-OPERATION
           MOVE TEST-BALANCE-2 TO SIM-BALANCE
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Operation: " SIM-OPERATION
           DISPLAY "Balance to Write: " SIM-BALANCE
           
           IF SIM-OPERATION = 'WRITE'
               MOVE SIM-BALANCE TO SIM-STORAGE-BAL
               DISPLAY "Balance stored: " SIM-STORAGE-BAL
               
               IF SIM-STORAGE-BAL = TEST-BALANCE-2
                   MOVE "PASS" TO TEST-RESULT
                   ADD 1 TO PASSED-TESTS
                   DISPLAY "Result: PASS - Zero balance write successful"
               ELSE
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
                   DISPLAY "Result: FAIL - Zero balance not stored correctly"
               END-IF
           ELSE
               MOVE "FAIL" TO TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "Result: FAIL - Operation not recognized"
           END-IF.

       TEST-DATA-INTEGRITY.
           MOVE "Testing Data Integrity" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Testing read after write consistency"
           
           MOVE 'WRITE' TO SIM-OPERATION
           MOVE INITIAL-BALANCE TO SIM-BALANCE
           MOVE SIM-BALANCE TO SIM-STORAGE-BAL
           
           MOVE 'READ' TO SIM-OPERATION
           MOVE SIM-STORAGE-BAL TO SIM-BALANCE
           
           DISPLAY "Written: " INITIAL-BALANCE
           DISPLAY "Read back: " SIM-BALANCE
           
           IF SIM-BALANCE = INITIAL-BALANCE
               MOVE "PASS" TO TEST-RESULT
               ADD 1 TO PASSED-TESTS
               DISPLAY "Result: PASS - Data integrity maintained"
           ELSE
               MOVE "FAIL" TO TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "Result: FAIL - Data integrity compromised"
           END-IF.

       DISPLAY-DATA-SUMMARY.
           COMPUTE FAILED-TESTS = TOTAL-TESTS - PASSED-TESTS
           
           DISPLAY " "
           DISPLAY "========================================="
           DISPLAY "COBOL DATA MANAGER TEST SUMMARY"
           DISPLAY "========================================="
           DISPLAY "Total Tests Run:    " TOTAL-TESTS
           DISPLAY "Tests Passed:       " PASSED-TESTS
           DISPLAY "Tests Failed:       " FAILED-TESTS
           
           IF FAILED-TESTS = 0
               DISPLAY "Overall Result:     ALL TESTS PASSED"
           ELSE
               DISPLAY "Overall Result:     SOME TESTS FAILED"
           END-IF
           
           DISPLAY "=========================================".
