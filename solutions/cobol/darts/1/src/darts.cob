       IDENTIFICATION DIVISION.
       PROGRAM-ID. DARTS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-X PIC 9(3)V99.
       01 WS-Y PIC 9(3)V99.
       01 WS-X2 PIC 9(3)V99.
       01 WS-Y2 PIC 9(3)V99.
       01 WS-R PIC 9(3)V99.
       01 WS-E PIC 9(3)V99.
       01 WS-RESULT PIC 99.
       PROCEDURE DIVISION.
       DARTS.
       MOVE ZEROS TO WS-RESULT.
       COMPUTE WS-X2 = (WS-X ** 2).
       COMPUTE WS-Y2 = (WS-Y ** 2).
       COMPUTE WS-E = (WS-X2 + WS-Y2).
       COMPUTE WS-R = FUNCTION SQRT(WS-E).
       EVALUATE WS-R
         WHEN <= 1.0
            MOVE 10 TO WS-RESULT
         WHEN <= 5
            MOVE 5 TO WS-RESULT
         WHEN <= 10
            MOVE 1 TO WS-RESULT
         WHEN OTHER
            MOVE ZEROS TO WS-RESULT
       END-EVALUATE.