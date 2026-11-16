        IDENTIFICATION DIVISION.
        PROGRAM-ID. PANGRAM.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-SENTENCE PIC X(60).
        01 WS-RESULT PIC 9.
        01 WS-ALPHA PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
        01 WS-TABLE.
           05 WS-T PIC X OCCURS 60 TIMES.
        01 I PIC 99.
        01 K PIC 99.
        PROCEDURE DIVISION.
        PANGRAM.
            MOVE ZEROS TO WS-RESULT.
            MOVE FUNCTION UPPER-CASE(WS-SENTENCE) TO WS-TABLE.
            SORT WS-T ON ASCENDING KEY WS-T.
            MOVE 1 TO I
                      K.
            PERFORM UNTIL I GREATER 26 OR K GREATER 60
               EVALUATE TRUE
                  WHEN WS-ALPHA(I:1) EQUAL WS-T(K)
                     ADD 1 TO I
                              K
                  WHEN WS-ALPHA(I:1) LESS WS-T(K) 
                     EXIT PERFORM
                  WHEN OTHER
                     ADD 1 TO K
               END-EVALUATE
            END-PERFORM.
            IF I GREATER 26
               MOVE 1 TO WS-RESULT
            END-IF.