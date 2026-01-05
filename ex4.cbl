       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX4.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  FIRST-NUMBER    PIC 99.
       01  SECOND-NUMBER   PIC 99.
       01  THIRD-NUMBER    PIC 99.
       01  THE-RESULT      PIC 9999.
       01  COUNT-NUM       PIC 9.

       PROCEDURE DIVISION.

       PROGRAM-BEGIN.

           MOVE 0 TO COUNT-NUM.
           PERFORM ADVISE-USER.
           PERFORM INPUT-NUMBERS.
           ACCEPT FIRST-NUMBER
           END-ACCEPT.
           PERFORM INPUT-NUMBERS.
           ACCEPT SECOND-NUMBER
           END-ACCEPT.
           PERFORM INPUT-NUMBERS.
           ACCEPT THIRD-NUMBER
           END-ACCEPT.
           PERFORM OUTPUT-RESULT.


       PROGRAM-DONE.
           STOP RUN.

       ADVISE-USER.
           DISPLAY "ADDITION OF 3 NUMBERS."
           END-DISPLAY.

       INPUT-NUMBERS.
           PERFORM ADD-COUNT-NUM.
           DISPLAY "ADD " COUNT-NUM " NUMBER: "
           END-DISPLAY.


       ADD-COUNT-NUM.
           ADD 1 TO COUNT-NUM
           END-ADD.

       OUTPUT-RESULT.
           COMPUTE THE-RESULT = FIRST-NUMBER +
                               SECOND-NUMBER +
                               THIRD-NUMBER
           END-COMPUTE.

           DISPLAY "tHE RESULT IS : " THE-RESULT
           END-DISPLAY.
