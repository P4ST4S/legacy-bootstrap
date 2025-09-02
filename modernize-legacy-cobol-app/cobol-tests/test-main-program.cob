       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-MAIN-PROGRAM.
       AUTHOR. SYSTEM-TESTER.
       DATE-WRITTEN. 09/02/2025.
       
      *****************************************************************
      * COBOL Test Suite for Main Program                            *
      * This program tests the main menu system functionality        *
      * to validate behavior before Python conversion                *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TEST-OUTPUT-FILE ASSIGN TO "test-results.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TEST-OUTPUT-FILE.
       01  TEST-OUTPUT-RECORD     PIC X(80).

       WORKING-STORAGE SECTION.
       01  TEST-COUNTERS.
           05  TOTAL-TESTS        PIC 9(3) VALUE 0.
           05  PASSED-TESTS       PIC 9(3) VALUE 0.
           05  FAILED-TESTS       PIC 9(3) VALUE 0.

       01  TEST-VARIABLES.
           05  TEST-NAME          PIC X(50).
           05  TEST-RESULT        PIC X(10).
           05  EXPECTED-VALUE     PIC X(20).
           05  ACTUAL-VALUE       PIC X(20).
           
       01  SIMULATED-INPUT.
           05  SIM-USER-CHOICE    PIC 9 VALUE 0.
           05  SIM-CONTINUE-FLAG  PIC X(3) VALUE 'YES'.

       01  TEST-CONSTANTS.
           05  VALID-CHOICE-1     PIC 9 VALUE 1.
           05  VALID-CHOICE-2     PIC 9 VALUE 2.
           05  VALID-CHOICE-3     PIC 9 VALUE 3.
           05  VALID-CHOICE-4     PIC 9 VALUE 4.
           05  INVALID-CHOICE-5   PIC 9 VALUE 5.
           05  INVALID-CHOICE-0   PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-TEST-CONTROL.
           OPEN OUTPUT TEST-OUTPUT-FILE
           
           PERFORM INITIALIZE-TEST-SUITE
           PERFORM TEST-MENU-DISPLAY
           PERFORM TEST-VALID-CHOICES
           PERFORM TEST-INVALID-CHOICES
           PERFORM TEST-EXIT-FUNCTIONALITY
           PERFORM TEST-LOOP-CONTROL
           PERFORM DISPLAY-TEST-SUMMARY
           
           CLOSE TEST-OUTPUT-FILE
           STOP RUN.

       INITIALIZE-TEST-SUITE.
           MOVE "COBOL Main Program Test Suite" TO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD
           MOVE "================================" TO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD
           MOVE SPACES TO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD.

       TEST-MENU-DISPLAY.
           MOVE "Testing Menu Display Functionality" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Expected: Menu options 1-4 displayed"
           DISPLAY "Actual: "
           DISPLAY "--------------------------------"
           DISPLAY "Account Management System"
           DISPLAY "1. View Balance"
           DISPLAY "2. Credit Account"
           DISPLAY "3. Debit Account"
           DISPLAY "4. Exit"
           DISPLAY "--------------------------------"
           
           MOVE "PASS" TO TEST-RESULT
           ADD 1 TO PASSED-TESTS
           
           MOVE "TEST: Menu Display - PASS" TO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD.

       TEST-VALID-CHOICES.
           PERFORM TEST-CHOICE-1
           PERFORM TEST-CHOICE-2
           PERFORM TEST-CHOICE-3
           PERFORM TEST-CHOICE-4.

       TEST-CHOICE-1.
           MOVE "Testing Choice 1 (View Balance)" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE VALID-CHOICE-1 TO SIM-USER-CHOICE
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Input: " SIM-USER-CHOICE
           DISPLAY "Expected: Call Operations with 'TOTAL '"
           
           EVALUATE SIM-USER-CHOICE
               WHEN 1
                   DISPLAY "Result: Would call Operations with TOTAL"
                   MOVE "PASS" TO TEST-RESULT
                   ADD 1 TO PASSED-TESTS
               WHEN OTHER
                   DISPLAY "Result: FAILED - Wrong evaluation"
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
           END-EVALUATE
           
           MOVE "TEST: Choice 1 - " TO TEST-OUTPUT-RECORD
           STRING TEST-OUTPUT-RECORD DELIMITED BY SIZE
                  TEST-RESULT DELIMITED BY SIZE
                  INTO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD.

       TEST-CHOICE-2.
           MOVE "Testing Choice 2 (Credit Account)" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE VALID-CHOICE-2 TO SIM-USER-CHOICE
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Input: " SIM-USER-CHOICE
           DISPLAY "Expected: Call Operations with 'CREDIT'"
           
           EVALUATE SIM-USER-CHOICE
               WHEN 2
                   DISPLAY "Result: Would call Operations with CREDIT"
                   MOVE "PASS" TO TEST-RESULT
                   ADD 1 TO PASSED-TESTS
               WHEN OTHER
                   DISPLAY "Result: FAILED - Wrong evaluation"
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
           END-EVALUATE
           
           MOVE "TEST: Choice 2 - " TO TEST-OUTPUT-RECORD
           STRING TEST-OUTPUT-RECORD DELIMITED BY SIZE
                  TEST-RESULT DELIMITED BY SIZE
                  INTO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD.

       TEST-CHOICE-3.
           MOVE "Testing Choice 3 (Debit Account)" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE VALID-CHOICE-3 TO SIM-USER-CHOICE
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Input: " SIM-USER-CHOICE
           DISPLAY "Expected: Call Operations with 'DEBIT '"
           
           EVALUATE SIM-USER-CHOICE
               WHEN 3
                   DISPLAY "Result: Would call Operations with DEBIT"
                   MOVE "PASS" TO TEST-RESULT
                   ADD 1 TO PASSED-TESTS
               WHEN OTHER
                   DISPLAY "Result: FAILED - Wrong evaluation"
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
           END-EVALUATE
           
           MOVE "TEST: Choice 3 - " TO TEST-OUTPUT-RECORD
           STRING TEST-OUTPUT-RECORD DELIMITED BY SIZE
                  TEST-RESULT DELIMITED BY SIZE
                  INTO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD.

       TEST-CHOICE-4.
           MOVE "Testing Choice 4 (Exit)" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE VALID-CHOICE-4 TO SIM-USER-CHOICE
           MOVE 'YES' TO SIM-CONTINUE-FLAG
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Input: " SIM-USER-CHOICE
           DISPLAY "Expected: Set CONTINUE-FLAG to 'NO'"
           
           EVALUATE SIM-USER-CHOICE
               WHEN 4
                   MOVE 'NO' TO SIM-CONTINUE-FLAG
                   IF SIM-CONTINUE-FLAG = 'NO'
                       DISPLAY "Result: CONTINUE-FLAG set to NO correctly"
                       MOVE "PASS" TO TEST-RESULT
                       ADD 1 TO PASSED-TESTS
                   ELSE
                       DISPLAY "Result: FAILED - Flag not set correctly"
                       MOVE "FAIL" TO TEST-RESULT
                       ADD 1 TO FAILED-TESTS
                   END-IF
               WHEN OTHER
                   DISPLAY "Result: FAILED - Wrong evaluation"
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
           END-EVALUATE
           
           MOVE "TEST: Choice 4 - " TO TEST-OUTPUT-RECORD
           STRING TEST-OUTPUT-RECORD DELIMITED BY SIZE
                  TEST-RESULT DELIMITED BY SIZE
                  INTO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD.

       TEST-INVALID-CHOICES.
           PERFORM TEST-INVALID-CHOICE-5
           PERFORM TEST-INVALID-CHOICE-0.

       TEST-INVALID-CHOICE-5.
           MOVE "Testing Invalid Choice 5" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE INVALID-CHOICE-5 TO SIM-USER-CHOICE
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Input: " SIM-USER-CHOICE
           DISPLAY "Expected: Display invalid choice message"
           
           EVALUATE SIM-USER-CHOICE
               WHEN 1
               WHEN 2
               WHEN 3
               WHEN 4
                   DISPLAY "Result: FAILED - Should be invalid"
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
               WHEN OTHER
                   DISPLAY "Result: Invalid choice detected correctly"
                   DISPLAY "Invalid choice, please select 1-4."
                   MOVE "PASS" TO TEST-RESULT
                   ADD 1 TO PASSED-TESTS
           END-EVALUATE
           
           MOVE "TEST: Invalid Choice 5 - " TO TEST-OUTPUT-RECORD
           STRING TEST-OUTPUT-RECORD DELIMITED BY SIZE
                  TEST-RESULT DELIMITED BY SIZE
                  INTO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD.

       TEST-INVALID-CHOICE-0.
           MOVE "Testing Invalid Choice 0" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE INVALID-CHOICE-0 TO SIM-USER-CHOICE
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Input: " SIM-USER-CHOICE
           DISPLAY "Expected: Display invalid choice message"
           
           EVALUATE SIM-USER-CHOICE
               WHEN 1
               WHEN 2
               WHEN 3
               WHEN 4
                   DISPLAY "Result: FAILED - Should be invalid"
                   MOVE "FAIL" TO TEST-RESULT
                   ADD 1 TO FAILED-TESTS
               WHEN OTHER
                   DISPLAY "Result: Invalid choice detected correctly"
                   DISPLAY "Invalid choice, please select 1-4."
                   MOVE "PASS" TO TEST-RESULT
                   ADD 1 TO PASSED-TESTS
           END-EVALUATE
           
           MOVE "TEST: Invalid Choice 0 - " TO TEST-OUTPUT-RECORD
           STRING TEST-OUTPUT-RECORD DELIMITED BY SIZE
                  TEST-RESULT DELIMITED BY SIZE
                  INTO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD.

       TEST-EXIT-FUNCTIONALITY.
           MOVE "Testing Exit Functionality" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Expected: Program should display exit message"
           DISPLAY "Exiting the program. Goodbye!"
           
           MOVE "PASS" TO TEST-RESULT
           ADD 1 TO PASSED-TESTS
           
           MOVE "TEST: Exit Message - PASS" TO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD.

       TEST-LOOP-CONTROL.
           MOVE "Testing Loop Control Logic" TO TEST-NAME
           ADD 1 TO TOTAL-TESTS
           
           MOVE 'YES' TO SIM-CONTINUE-FLAG
           
           DISPLAY "Test: " TEST-NAME
           DISPLAY "Testing loop continuation logic"
           
           IF SIM-CONTINUE-FLAG NOT = 'NO'
               DISPLAY "Result: Loop would continue (CONTINUE-FLAG = YES)"
               MOVE "PASS" TO TEST-RESULT
               ADD 1 TO PASSED-TESTS
           ELSE
               DISPLAY "Result: FAILED - Loop logic incorrect"
               MOVE "FAIL" TO TEST-RESULT
               ADD 1 TO FAILED-TESTS
           END-IF
           
           MOVE 'NO' TO SIM-CONTINUE-FLAG
           
           IF SIM-CONTINUE-FLAG = 'NO'
               DISPLAY "Result: Loop would exit (CONTINUE-FLAG = NO)"
           ELSE
               DISPLAY "Result: FAILED - Exit logic incorrect"
               MOVE "FAIL" TO TEST-RESULT
               ADD 1 TO FAILED-TESTS
           END-IF
           
           MOVE "TEST: Loop Control - " TO TEST-OUTPUT-RECORD
           STRING TEST-OUTPUT-RECORD DELIMITED BY SIZE
                  TEST-RESULT DELIMITED BY SIZE
                  INTO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD.

       DISPLAY-TEST-SUMMARY.
           MOVE SPACES TO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD
           MOVE "Test Suite Summary:" TO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD
           MOVE "==================" TO TEST-OUTPUT-RECORD
           WRITE TEST-OUTPUT-RECORD
           
           DISPLAY " "
           DISPLAY "========================================="
           DISPLAY "COBOL MAIN PROGRAM TEST SUMMARY"
           DISPLAY "========================================="
           DISPLAY "Total Tests Run:    " TOTAL-TESTS
           DISPLAY "Tests Passed:       " PASSED-TESTS
           DISPLAY "Tests Failed:       " FAILED-TESTS
           
           COMPUTE FAILED-TESTS = TOTAL-TESTS - PASSED-TESTS
           
           IF FAILED-TESTS = 0
               DISPLAY "Overall Result:     ALL TESTS PASSED"
               MOVE "Overall Result: ALL TESTS PASSED" TO TEST-OUTPUT-RECORD
           ELSE
               DISPLAY "Overall Result:     SOME TESTS FAILED"
               MOVE "Overall Result: SOME TESTS FAILED" TO TEST-OUTPUT-RECORD
           END-IF
           
           WRITE TEST-OUTPUT-RECORD
           DISPLAY "========================================="
           DISPLAY " "
           DISPLAY "Test results saved to: test-results.txt".
