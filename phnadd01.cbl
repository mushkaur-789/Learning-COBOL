       IDENTIFICATION DIVISION.
       PROGRAM-ID. PHNADD01.
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

       01  ENTRY-OK                    PIC X.
       01  YES-NO                      PIC X.

       01  TEXT-1                      PIC X(10) VALUE "FIRST NAME".
       01  TEXT-2                      PIC X(9) VALUE "LAST NAME".
       01  TEXT-3                      PIC X(12) VALUE "PHONE NUMBER".

       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.

       PROGRAM-BEGIN.

           PERFORM OPENING-PROCEDURE.
           MOVE "Y" TO YES-NO.
           PERFORM GET-RECORD
               UNTIL YES-NO = "N".
           PERFORM CLOSING-PROCEDURE.

       PROGRAM-DONE.
           STOP RUN.

      *---------------------------------------------------------------
       OPENING-PROCEDURE.
           OPEN OUTPUT PHONE-FILE.

       CLOSING-PROCEDURE.
           CLOSE PHONE-FILE.

       GET-RECORD.
           MOVE SPACE TO PHONE-RECORD.
           PERFORM GET-FIELDS.
           PERFORM CHECK-INPUT.
           PERFORM ADD-RECORD.
           PERFORM GO-AGAIN.

       CHECK-INPUT.
           IF PHONE-FIRST-NAME = SPACE OR
              PHONE-LAST-NAME = SPACE OR
              PHONE-NUMBER = SPACE
              DISPLAY "INPUT NOT VALID. PLEASE TRY AGAIN."
              PERFORM GET-FIELDS.

       GET-FIELDS.
           DISPLAY TEXT-1 ": ".
           ACCEPT PHONE-FIRST-NAME.
           DISPLAY TEXT-2 ": ".
           ACCEPT PHONE-LAST-NAME.
           DISPLAY TEXT-3 ": ".
           ACCEPT PHONE-NUMBER.

       ADD-RECORD.
           WRITE PHONE-RECORD.

       GO-AGAIN.
           DISPLAY "DO YOU WANT TO ADD ANOTHER RECORD?".
           ACCEPT YES-NO.
           IF YES-NO = "Y" OR YES-NO = "y"
               MOVE "Y" TO YES-NO
           ELSE MOVE "N" TO YES-NO.
