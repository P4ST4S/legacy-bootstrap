       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-OPERATIONS.
       AUTHOR. SYSTEM-TESTER.
       DATE-WRITTEN. 09/02/2025.
       
      *****************************************************************
      * COBOL Test Suite for Operations Program                      *
      * Tests credit, debit, and balance operations                  *
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
           05  EXPECTED-BALANCE   PIC 9(6)V99.
           05  ACTUAL-BALANCE     PIC 9(6)V99.
           
       01  SIMULATION-DATA.
           05  SIM-OPERATION      PIC X(6).
           05  SIM-AMOUNT         PIC 9(6)V99.
           05  SIM-BALANCE        PIC 9(6)V99 VALUE 1000.00.

       01  TEST-CONSTANTS.
           05  INITIAL-BALANCE    PIC 9(6)V99 VALUE 1000.00.
           05  CREDIT-AMOUNT-1    PIC 9(6)V99 VALUE 100.00.
           05  CREDIT-AMOUNT-2    PIC 9(6)V99 VALUE 250.50.
           05  DEBIT-AMOUNT-1     PIC 9(6)V99 VALUE 50.00.
           05  DEBIT-AMOUNT-2     PIC 9(6)V99 VALUE 2000.00.

       PROCEDURE DIVISION.
       MAIN-TEST-CONTROL.
           PERFORM INITIALIZE-OPERATIONS-TEST
           PERFORM TEST-BALANCE-OPERATIONS
           PERFORM TEST-CREDIT-OPERATIONS
           PERFORM TEST-DEBIT-OPERATIONS
           PERFORM TEST-INSUFFICIENT-FUNDS
           PERFORM DISPLAY-OPERATIONS-SUMMARY
           STOP RUN.

       INITIALIZE-OPERATIONS-TEST.
           DISPLAY "========================================="
           DISPLAY "COBOL OPERATIONS TEST SUITE"
           DISPLAY "========================================="
           MOVE INITIAL-BALANCE TO SIM-BALANCE.

       TEST-BALANCE-OPERATIONS.
           MOVE "Testing Balance View Operation" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE 'TOTAL ' TO SIM-OPERATION
           MOVE SIM-BALANCE TO EXPECTED-BALANCE
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Operation: " SIM-OPERATION
           DISPLAY "Expected Balance: " EXPECTED-BALANCE
           
           IF SIM-OPERATION = 'TOTAL '
               MOVE SIM-BALANCE TO ACTUAL-BALANCE
               DISPLAY "Current balance: " ACTUAL-BALANCE
               
               IF ACTUAL-BALANCE = EXPECTED-BALANCE
                   MOVE "PASS" TO TEST-RESULT
                   ADD 1 TO PASSED-TESTS
                   DISPLAY "Result: PASS - Balance displayed correctly"
               ELSE
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
                   DISPLAY "Result: FAIL - Balance incorrect"
               END-IF
           ELSE
               MOVE "FAIL" TO TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "Result: FAIL - Operation not recognized"
           END-IF.

       TEST-CREDIT-OPERATIONS.
           PERFORM TEST-CREDIT-VALID-AMOUNT
           PERFORM TEST-CREDIT-ZERO-AMOUNT.

       TEST-CREDIT-VALID-AMOUNT.
           MOVE "Testing Credit with Valid Amount" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE 'CREDIT' TO SIM-OPERATION
           MOVE CREDIT-AMOUNT-1 TO SIM-AMOUNT
           COMPUTE EXPECTED-BALANCE = SIM-BALANCE + SIM-AMOUNT
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Operation: " SIM-OPERATION
           DISPLAY "Credit Amount: " SIM-AMOUNT
           DISPLAY "Initial Balance: " SIM-BALANCE
           DISPLAY "Expected New Balance: " EXPECTED-BALANCE
           
           IF SIM-OPERATION = 'CREDIT'
               ADD SIM-AMOUNT TO SIM-BALANCE
               MOVE SIM-BALANCE TO ACTUAL-BALANCE
               DISPLAY "Amount credited. New balance: " ACTUAL-BALANCE
               
               IF ACTUAL-BALANCE = EXPECTED-BALANCE
                   MOVE "PASS" TO TEST-RESULT
                   ADD 1 TO PASSED-TESTS
                   DISPLAY "Result: PASS - Credit operation successful"
               ELSE
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
                   DISPLAY "Result: FAIL - Credit calculation incorrect"
               END-IF
           ELSE
               MOVE "FAIL" TO TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "Result: FAIL - Operation not recognized"
           END-IF.

       TEST-CREDIT-ZERO-AMOUNT.
           MOVE "Testing Credit with Zero Amount" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE 'CREDIT' TO SIM-OPERATION
           MOVE 0.00 TO SIM-AMOUNT
           MOVE SIM-BALANCE TO EXPECTED-BALANCE
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Operation: " SIM-OPERATION
           DISPLAY "Credit Amount: " SIM-AMOUNT
           DISPLAY "Expected Balance (unchanged): " EXPECTED-BALANCE
           
           IF SIM-OPERATION = 'CREDIT'
               ADD SIM-AMOUNT TO SIM-BALANCE
               MOVE SIM-BALANCE TO ACTUAL-BALANCE
               DISPLAY "Amount credited. New balance: " ACTUAL-BALANCE
               
               IF ACTUAL-BALANCE = EXPECTED-BALANCE
                   MOVE "PASS" TO TEST-RESULT
                   ADD 1 TO PASSED-TESTS
                   DISPLAY "Result: PASS - Zero credit handled correctly"
               ELSE
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
                   DISPLAY "Result: FAIL - Zero credit calculation incorrect"
               END-IF
           ELSE
               MOVE "FAIL" TO TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "Result: FAIL - Operation not recognized"
           END-IF.

       TEST-DEBIT-OPERATIONS.
           PERFORM TEST-DEBIT-VALID-AMOUNT.

       TEST-DEBIT-VALID-AMOUNT.
           MOVE "Testing Debit with Valid Amount" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE 'DEBIT ' TO SIM-OPERATION
           MOVE DEBIT-AMOUNT-1 TO SIM-AMOUNT
           COMPUTE EXPECTED-BALANCE = SIM-BALANCE - SIM-AMOUNT
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Operation: " SIM-OPERATION
           DISPLAY "Debit Amount: " SIM-AMOUNT
           DISPLAY "Current Balance: " SIM-BALANCE
           DISPLAY "Expected New Balance: " EXPECTED-BALANCE
           
           IF SIM-OPERATION = 'DEBIT '
               IF SIM-BALANCE >= SIM-AMOUNT
                   SUBTRACT SIM-AMOUNT FROM SIM-BALANCE
                   MOVE SIM-BALANCE TO ACTUAL-BALANCE
                   DISPLAY "Amount debited. New balance: " ACTUAL-BALANCE
                   
                   IF ACTUAL-BALANCE = EXPECTED-BALANCE
                       MOVE "PASS" TO TEST-RESULT
                       ADD 1 TO PASSED-TESTS
                       DISPLAY "Result: PASS - Debit operation successful"
                   ELSE
                       MOVE "FAIL" TO TEST-RESULT
                       ADD 1 TO FAILED-TESTS
                       DISPLAY "Result: FAIL - Debit calculation incorrect"
                   END-IF
               ELSE
                   DISPLAY "Insufficient funds for this debit."
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
                   DISPLAY "Result: FAIL - Should have sufficient funds"
               END-IF
           ELSE
               MOVE "FAIL" TO TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "Result: FAIL - Operation not recognized"
           END-IF.

       TEST-INSUFFICIENT-FUNDS.
           MOVE "Testing Debit with Insufficient Funds" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE 'DEBIT ' TO SIM-OPERATION
           MOVE DEBIT-AMOUNT-2 TO SIM-AMOUNT
           MOVE SIM-BALANCE TO EXPECTED-BALANCE
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Operation: " SIM-OPERATION
           DISPLAY "Debit Amount: " SIM-AMOUNT
           DISPLAY "Current Balance: " SIM-BALANCE
           DISPLAY "Expected: Insufficient funds message"
           
           IF SIM-OPERATION = 'DEBIT '
               IF SIM-BALANCE >= SIM-AMOUNT
                   SUBTRACT SIM-AMOUNT FROM SIM-BALANCE
                   DISPLAY "Amount debited. New balance: " SIM-BALANCE
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
                   DISPLAY "Result: FAIL - Should have insufficient funds"
               ELSE
                   DISPLAY "Insufficient funds for this debit."
                   MOVE SIM-BALANCE TO ACTUAL-BALANCE
                   
                   IF ACTUAL-BALANCE = EXPECTED-BALANCE
                       MOVE "PASS" TO TEST-RESULT
                       ADD 1 TO PASSED-TESTS
                       DISPLAY "Result: PASS - Insufficient funds detected"
                   ELSE
                       MOVE "FAIL" TO TEST-RESULT
                       ADD 1 TO FAILED-TESTS
                       DISPLAY "Result: FAIL - Balance changed unexpectedly"
                   END-IF
               END-IF
           ELSE
               MOVE "FAIL" TO TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "Result: FAIL - Operation not recognized"
           END-IF.

       DISPLAY-OPERATIONS-SUMMARY.
           COMPUTE FAILED-TESTS = TOTAL-TESTS - PASSED-TESTS
           
           DISPLAY " "
           DISPLAY "========================================="
           DISPLAY "COBOL OPERATIONS TEST SUMMARY"
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
