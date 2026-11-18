       IDENTIFICATION DIVISION.
       PROGRAM-ID. COLLATZ-CONJECTURE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER PIC S9(8).
       01 WS-REM PIC S9(8).
       01 WS-STEPS PIC 9(4).
       01 WS-ERROR PIC X(35).
       PROCEDURE DIVISION.
       COLLATZ-CONJECTURE.

       IF WS-NUMBER LESS 1
          MOVE 'Only positive integers are allowed' TO WS-ERROR
       ELSE
          MOVE ZEROS TO WS-STEPS
             PERFORM UNTIL WS-NUMBER EQUAL 1
                MOVE FUNCTION REM(WS-NUMBER 2) TO WS-REM
                IF WS-REM EQUAL ZEROS
                   COMPUTE WS-NUMBER = WS-NUMBER / 2
                ELSE
                   COMPUTE WS-NUMBER = (WS-NUMBER * 3) + 1
                END-IF
                ADD 1 TO WS-STEPS
             END-PERFORM
       END-IF.

       DISPLAY WS-STEPS.
       DISPLAY WS-ERROR.