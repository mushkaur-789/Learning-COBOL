       IDENTIFICATION DIVISION.
       PROGRAM-ID. PHNRD01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL PHONE-FILE
               ASSIGN TO "phone.dat"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  PHONE-FILE.
      *    LABEL RECORDS ARE STANDARD.

       01  PHONE-RECORD.
           05  PHONE-FIRST-NAME        PIC X(15).
           05  PHONE-LAST-NAME         PIC X(15).
           05  PHONE-NUMBER            PIC 9(10).


       WORKING-STORAGE SECTION.

       01  FIELDS-TO-DISPLAY.
           05  TEXT-1                  PIC X(11) VALUE "FIRST NAME:".
           05  DISPLAY-FIRST-NAME      PIC X(15).
           05  TEXT-2                  PIC X(10) VALUE "LAST NAME:".
           05  DISPLAY-LAST-NAME       PIC X(15).
           05  TEXT-3                  PIC X(13) VALUE "PHONE NUMBER:".
           05  DISPLAY-PHONE-NUM       PIC X(10).

       01  SCREEN-LINES                PIC 99.
       01  A-DUMMY                     PIC X.
       01  END-FILE                    PIC X.

       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.

       PROGRAM-BEGIN.

           MOVE "N" TO END-FILE.
           MOVE 0 TO SCREEN-LINES.
           PERFORM OPENING-PROCEDURE.
           PERFORM READ-NEXT-RECORD.
           PERFORM DISPLAY-RECORD
               UNTIL END-FILE = "Y".
           PERFORM CLOSING-PROCEDURE.

       PROGRAM-DONE.
           STOP RUN.

      *---------------------------------------------------------------
       OPENING-PROCEDURE.
           OPEN INPUT PHONE-FILE.

       CLOSING-PROCEDURE.
           CLOSE PHONE-FILE.

       DISPLAY-RECORD.
           PERFORM DISPLAY-FIELDS.
           PERFORM READ-NEXT-RECORD.

       DISPLAY-FIELDS.
           IF SCREEN-LINES = 5
               PERFORM PRESS-ENTER.
           MOVE PHONE-FIRST-NAME TO DISPLAY-FIRST-NAME.
           MOVE PHONE-LAST-NAME TO DISPLAY-LAST-NAME.
           MOVE PHONE-NUMBER TO DISPLAY-PHONE-NUM.
           DISPLAY FIELDS-TO-DISPLAY.
           ADD 1 TO SCREEN-LINES.

       READ-NEXT-RECORD.
           READ PHONE-FILE NEXT RECORD
               AT END
               MOVE "Y" TO END-FILE.

       PRESS-ENTER.
           DISPLAY "PRESS ENTER TO CONTINUE...".
           ACCEPT A-DUMMY.
           MOVE ZERO TO SCREEN-LINES.
