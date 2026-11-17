       IDENTIFICATION DIVISION.
       PROGRAM-ID. REVERSE-STRING.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STRING PIC X(64).
       01 WS-STRING-R PIC X(64).
       01 WS-STARTED PIC X.
       01 I PIC 99.
       01 K PIC 99.
       PROCEDURE DIVISION.
       REVERSE-STRING.
       MOVE 1 TO K.
       MOVE 'N' TO WS-STARTED.
       MOVE SPACES TO WS-STRING-R.
       PERFORM VARYING I FROM 64 BY -1 UNTIL I EQUAL ZEROS
          IF WS-STRING(I:1) EQUAL ' ' AND 
             WS-STRING(I - 1:1) EQUAL ' '
             CONTINUE
          ELSE
             IF WS-STARTED EQUAL 'N' AND 
                WS-STRING(I:1) EQUAL ' '
                MOVE 'S' TO WS-STARTED
             END-IF
             IF WS-STARTED EQUAL 'S'
                MOVE WS-STRING(I:1) TO WS-STRING-R(K - 1:1)
                ADD 1 TO K
             END-IF
          END-IF
       END-PERFORM.
       MOVE WS-STRING-R TO WS-STRING.
       DISPLAY WS-STRING.