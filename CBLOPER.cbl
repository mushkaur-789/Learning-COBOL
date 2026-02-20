       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLOPER.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
      *INPUT DATA FROM THE USER
       01  THE-CHOICE          PIC 9.
       01  ADD-MORE            PIC X VALUE IS SPACE.
       01  ENTRY-NUM-1         PIC -Z(20).99.
       01  ENTRY-NUM-2         PIC -Z(20).99.
      *DISPLAY RESULT OF OPERATION
       01  THE-SUM             PIC -Z(20).99.
       01  THE-DIFF            PIC -Z(20).99.
       01  THE-PRODUCT         PIC -Z(20).99.
       01  THE-DIVIDEND        PIC -Z(20).99.
       01  THE-PERCENTAGE      PIC -Z(20).99.
       01  THE-REMAINDER       PIC 9(6).99.
      *DATA FOR OPERATION
       01  NUM-1               PIC S9(20)V99.
       01  NUM-2               PIC S9(20)V99.
       01  NUM-3               PIC S9(20)V99.
       01  THE-RESULT          PIC S9(20)V99 VALUE IS ZERO.

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           PERFORM DISPLAY-MENU.

           IF THE-CHOICE = 1
               DISPLAY "ADDITION" END-DISPLAY
               PERFORM ADD-OPER
           END-IF.

           IF THE-CHOICE = 2
               DISPLAY "SUBTRACTION" END-DISPLAY
               DISPLAY "ENTER THE MINUEND:" END-DISPLAY
               ACCEPT ENTRY-NUM-1 END-ACCEPT
               MOVE ENTRY-NUM-1 TO THE-RESULT
               PERFORM SUB-OPER
           END-IF.

           IF THE-CHOICE = 3
               DISPLAY "ENTER THE MULTIPLICAND:" END-DISPLAY
               ACCEPT ENTRY-NUM-1 END-ACCEPT
               MOVE ENTRY-NUM-1 TO THE-RESULT
               PERFORM MUL-OPER
           END-IF.

           IF THE-CHOICE = 4
               DISPLAY "ENTER THE QUOTIENT:" END-DISPLAY
               ACCEPT ENTRY-NUM-1 END-ACCEPT
               MOVE ENTRY-NUM-1 TO NUM-1

               DISPLAY "ENTER THE DIVISOR:" END-DISPLAY
               ACCEPT ENTRY-NUM-2 END-ACCEPT
               MOVE ENTRY-NUM-2 TO NUM-2
               PERFORM DIV-OPER
           END-IF.

           IF THE-CHOICE = 5
               DISPLAY "ENTER THE NUMBER:" END-DISPLAY
               ACCEPT ENTRY-NUM-1 END-ACCEPT
               MOVE ENTRY-NUM-1 TO NUM-1

               DISPLAY "ENTER THE PERCENTAGE:" END-DISPLAY
               ACCEPT ENTRY-NUM-2 END-ACCEPT
               MOVE ENTRY-NUM-2 TO NUM-2
               PERFORM PERC-OPER
           END-IF.

           IF THE-CHOICE = 0
               PERFORM PROGRAM-END
           END-IF.

       PROGRAM-END.
           STOP RUN.

       DISPLAY-MENU.
           DISPLAY "PLEASE CHOOSE AN OPERATION"
           END-DISPLAY.

           DISPLAY "1- ADDITION" END-DISPLAY.
           DISPLAY "2- SUBTRACTION" END-DISPLAY.
           DISPLAY "3- MULTIPLICATION" END-DISPLAY.
           DISPLAY "4- DIVISION" END-DISPLAY.
           DISPLAY "5- CALCULATE PER PERCENTAGE" END-DISPLAY.
           DISPLAY "0- END PROGRAM" END-DISPLAY.
           DISPLAY "CHOOSE ONE OPTION" END-DISPLAY.
           ACCEPT THE-CHOICE END-ACCEPT.

           IF THE-CHOICE < 0 OR THE-CHOICE > 5
               DISPLAY "CHOICE NOT VALID" END-DISPLAY
               PERFORM DISPLAY-MENU
           END-IF.

       ADD-NUMBER.
           DISPLAY "CONTINUE WITH ANOTHER NUMBER? (Y = YES / N = NO)"
           END-DISPLAY.
           ACCEPT ADD-MORE END-ACCEPT.

           IF ADD-MORE = "y"
              MOVE "Y" TO ADD-MORE
           ELSE
               IF ADD-MORE = "n"
                  MOVE "N" TO ADD-MORE
               END-IF
           END-IF.

           IF ADD-MORE NOT= "Y"  AND ADD-MORE NOT= "N"
               DISPLAY "CHOICE NOT VALID. TRY AGAIN" END-DISPLAY
               PERFORM ADD-NUMBER
           END-IF.

       ADD-OPER.

           DISPLAY "ENTER NUMBER:" END-DISPLAY.
           ACCEPT ENTRY-NUM-1 END-ACCEPT.
           MOVE ENTRY-NUM-1 TO NUM-1.
           ADD NUM-1 TO THE-RESULT END-ADD.
           PERFORM ADD-NUMBER.
           IF ADD-MORE = "Y"
               PERFORM ADD-OPER
           ELSE
               MOVE THE-RESULT TO THE-SUM
               DISPLAY "THE SUM IS: " THE-SUM END-DISPLAY
               PERFORM PROGRAM-END
           END-IF.

       SUB-OPER.
           DISPLAY "ENTER THE SUBTRAHEND:" END-DISPLAY.
           ACCEPT ENTRY-NUM-2 END-ACCEPT.
           MOVE ENTRY-NUM-2 TO NUM-1.

           SUBTRACT NUM-1 FROM THE-RESULT
           GIVING THE-RESULT
           END-SUBTRACT.
           PERFORM ADD-NUMBER.

           IF ADD-MORE = "Y"
               PERFORM SUB-OPER
           ELSE
               MOVE THE-RESULT TO THE-DIFF
               DISPLAY "THE DIFFERENCE IS: " THE-DIFF END-DISPLAY
               PERFORM PROGRAM-END
           END-IF.

       MUL-OPER.
           DISPLAY "ENTER THE MULTIPLICATOR:" END-DISPLAY.
           ACCEPT ENTRY-NUM-2 END-ACCEPT.
           MOVE ENTRY-NUM-2 TO NUM-2.

           MULTIPLY THE-RESULT BY NUM-2
           GIVING THE-RESULT
           END-MULTIPLY.
           MOVE THE-RESULT TO THE-PRODUCT.
           DISPLAY "THE PRODUCT IS: " THE-PRODUCT END-DISPLAY.
           PERFORM ADD-NUMBER.

           IF ADD-MORE = "Y"
               PERFORM MUL-OPER
           ELSE
               MOVE THE-RESULT TO THE-PRODUCT
               DISPLAY "THE PRODUCT IS: " THE-PRODUCT END-DISPLAY
               PERFORM PROGRAM-END
           END-IF.

       DIV-OPER.
           DIVIDE NUM-1 BY NUM-2
           GIVING THE-RESULT
           REMAINDER THE-REMAINDER
           END-DIVIDE.

           MOVE THE-RESULT TO THE-DIVIDEND
           DISPLAY ENTRY-NUM-1 " / " ENTRY-NUM-2 END-DISPLAY.
           DISPLAY "THE DIVIDEND IS: " THE-DIVIDEND END-DISPLAY.
           DISPLAY "THE REMAINDER IS: " THE-REMAINDER END-DISPLAY.

       PERC-OPER. 
           MULTIPLY NUM-1 BY NUM-2
           GIVING NUM-3 
           END-MULTIPLY. 
               
           DIVIDE NUM-3 BY 100
           GIVING THE-RESULT 
           END-DIVIDE.
           
           MOVE THE-RESULT TO THE-PERCENTAGE
           DISPLAY "THE "ENTRY-NUM-2"% OF " ENTRY-NUM-1 " IS: "
      -    THE-PERCENTAGE END-DISPLAY.

