       IDENTIFICATION DIVISION.
       PROGRAM-ID. RIPASSO01.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  NAME              PIC X(20).
       01  D-YEAR            PIC 9999.
       01  D-MONTH           PIC 99.
       01  D-DAY             PIC 99.
       01  AGE               PIC 999.
       01  COUNT-AGE         PIC 999.
       01  NEW-Y             PIC 9999.

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.

           PERFORM ASK-NAME.
           DISPLAY "USER PLEASE ENTER NOW YOUR DATE OF BIRTH"
           END-DISPLAY.
           PERFORM ASK-YEAR.
           PERFORM ASK-MONTH.
           PERFORM ASK-DAY.
           MOVE 0 TO AGE.
           MOVE D-YEAR TO NEW-Y
           PERFORM CALC-AGE.

       PROGRAM-END.
           STOP RUN.

       ASK-NAME.
           DISPLAY "DEAR USER ENTER YOUR NAME" END-DISPLAY.
           ACCEPT NAME END-ACCEPT.

       PROGRAM-ASK-DATE-OF-BIRTH.
       ASK-YEAR.
           DISPLAY "YEAR: " END-DISPLAY.
           ACCEPT D-YEAR END-ACCEPT.
           IF D-YEAR < 1900 OR D-YEAR > 2026
               DISPLAY "PLEASE ENTER A VALID YEAR" END-DISPLAY
               PERFORM ASK-YEAR
           END-IF.

       ASK-MONTH.
           DISPLAY "MONTH: " END-DISPLAY.
           ACCEPT D-MONTH END-ACCEPT.
           IF D-MONTH < 1 OR D-MONTH > 12
               DISPLAY "PLEASE ENTER A VALID MONTH" END-DISPLAY
               PERFORM ASK-MONTH
           END-IF.

       ASK-DAY.
           DISPLAY "DAY: " END-DISPLAY.
           ACCEPT D-DAY END-ACCEPT.
           IF D-MONTH = 1
               OR D-MONTH = 3
               OR D-MONTH = 5
               OR D-MONTH = 7
               OR D-MONTH = 8
               OR D-MONTH = 10
               OR D-MONTH = 12
               IF D-DAY < 1 OR D-DAY > 31
                   DISPLAY "PLEASE ENTER A VALID DAY" END-DISPLAY
                   PERFORM ASK-DAY
                END-IF
           END-IF.

           IF D-MONTH = 4
               OR D-MONTH = 6
               OR D-MONTH = 9
               OR D-MONTH = 11
               IF D-DAY < 1 OR D-DAY > 30
                   DISPLAY "PLEASE ENTER A VALID DAY" END-DISPLAY
                   PERFORM ASK-DAY
                END-IF
           END-IF.

           IF D-MONTH = 2
               IF D-YEAR = 2024
                  OR D-YEAR = 2020
                  OR D-YEAR = 2016
                  OR D-YEAR = 2012
                  OR D-YEAR = 2008
                  OR D-YEAR = 2004
                   IF D-DAY < 1 OR D-DAY > 29
                       DISPLAY "PLEASE ENTER A VALID DAY" END-DISPLAY
                       PERFORM ASK-DAY
                   END-IF
               ELSE
                   IF D-DAY < 1 OR D-DAY > 28
                       DISPLAY "PLEASE ENTER A VALID DAY" END-DISPLAY
                       PERFORM ASK-DAY
                    END-IF
               END-IF
           END-IF.

       CALC-AGE.
           IF NEW-Y < 2026
               COMPUTE NEW-Y = NEW-Y + 1 END-COMPUTE
               ADD 1 TO AGE END-ADD
               PERFORM CALC-AGE
           ELSE
               DISPLAY "YOUR AGE IS " AGE END-DISPLAY
           END-IF.
