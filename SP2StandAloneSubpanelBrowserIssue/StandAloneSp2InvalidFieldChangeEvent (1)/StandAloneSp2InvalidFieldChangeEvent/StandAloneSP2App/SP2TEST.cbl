       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN AS "GSSERP.SP2TEST".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       REPOSITORY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       
       COPY "SP2.CPY".
       COPY "ITS014.SP2".
      ******************************************************************
      ** COUNTERS 
      ******************************************************************
       01  CTR1                         PIC 9(16) VALUE ZEROS.
       01  PARM-CNT                     PIC 9(4)  VALUE ZEROS.
       01  WS-CTR1                      PIC 9(9)  VALUE ZERO.
       01  WS-CTR2                      PIC 9(9)  VALUE ZERO.
       01  WS-CTR3                      PIC 9(9)  VALUE ZERO.
       01  WS-CTR4                      PIC 9(9)  VALUE ZERO.
       01  WS-CTR5                      PIC 9(9)  VALUE ZERO.
       01  WS-CTR6                      PIC 9(9)  VALUE ZERO.
       01  WS-CTR7                      PIC 9(9)  VALUE ZERO.
       01  WS-CTR8                      PIC 9(9)  VALUE ZERO.
       01  WS-CTR9                      PIC 9(9)  VALUE ZERO.
       77  WS-COUNT                     PIC 9(3)  VALUE ZERO.
       01  WS-COUNT2                    PIC 9(9)  VALUE ZERO.
       77  WS-COUNTER                   PIC 9(9)  VALUE ZERO.
      
       01  WS-SCREEN-EXIT               PIC X     VALUE "N".
           88  SCREEN-EXIT                        VALUE "Y".
       01  WS-SCREEN-OPEN               PIC X     VALUE "N".
           88  SCREEN-OPEN                        VALUE "Y".
           
                           
       01  CP-SP2-P1                       PIC S9(4)   COMP-5.                    
       01  CP-SP2-P2                       POINTER.
       01  CP-SP2-P2-R                     REDEFINES 
           CP-SP2-P2                       PIC S9(9)   COMP-5.
       
       PROCEDURE DIVISION.
       MAINLINE.
           PERFORM SCREEN-PROCESSING
           PERFORM END-PROGRAM
           .
       
       END-PROGRAM.
           IF SCREEN-OPEN
               CALL "SP2" USING SP2-CLOSE-WINDOW SP2-NULL-PARM 
               INITIALIZE WS-SCREEN-OPEN
           END-IF
           GOBACK
           .
           
       SCREEN-PROCESSING.
           PERFORM DISPLAY-SCREEN
           PERFORM UNTIL SCREEN-EXIT
               PERFORM CONVERSE
               PERFORM EVENT-HANDLER
           END-PERFORM
           .
           
       DISPLAY-SCREEN.                                             
           MOVE LOW-VALUES TO SP2-WD-DATA.                                         
           MOVE "ITS014" TO SP2-WD-PANEL-NAME. 
      *    CALL "SP2" USING SP2-SET-RECORD ITS014-PANEL-RECORD                                 
           MOVE SP2-SET-RECORD TO CP-SP2-P1                       
           SET CP-SP2-P2 TO ADDRESS OF ITS014-PANEL-RECORD        
           PERFORM COMPROC-CALL-SP2                           .   
      *    MOVE "x" TO SP2-WD-HIDE-SW   
      *    CALL "SP2" USING SP2-OPEN-WINDOW SP2-WINDOW-DEF                         
           MOVE SP2-OPEN-WINDOW TO CP-SP2-P1                      
           SET CP-SP2-P2 TO ADDRESS OF SP2-WINDOW-DEF             
           PERFORM COMPROC-CALL-SP2                       .       
           MOVE LOW-VALUES TO ITS014-DATA.                        
           MOVE "ITS014" TO ITS014-NEXT-PANEL.                    
           MOVE LOW-VALUES TO ITS014-FIELDS.
      *    MOVE SPACES     TO ITS014-FIELDS.                      
           MOVE LOW-VALUES TO ITS014-COLRS.                       
           MOVE LOW-VALUES TO ITS014-TYPES.                       
           INITIALIZE WS-SCREEN-EXIT
           SET SCREEN-OPEN TO TRUE
           
                                                                  
           MOVE "n" TO ITS014-WAIT-SW.    
      *    CALL "SP2" USING SP2-CONVERSE-PANEL ITS014-CONVERSE-DATA                        
           MOVE SP2-CONVERSE-PANEL TO CP-SP2-P1                   
           SET CP-SP2-P2 TO ADDRESS OF ITS014-CONVERSE-DATA       
           PERFORM COMPROC-CALL-SP2    
           MOVE LOW-VALUES  TO ITS014-WAIT-SW                           
           .
           
       CONVERSE.     
      *    CALL "SP2" USING SP2-CONVERSE-PANEL ITS014-CONVERSE-DATA                                              
           MOVE SP2-CONVERSE-PANEL TO CP-SP2-P1                   
           SET CP-SP2-P2 TO ADDRESS OF ITS014-CONVERSE-DATA       
           PERFORM COMPROC-CALL-SP2
           .                  
           
       EVENT-HANDLER.
           IF ITS014-KEY = SP2-KEY-ESCAPE OR                                       
              ITS014-KEY = SP2-KEY-CLOSE
               SET SCREEN-EXIT TO TRUE
           END-IF 
           
      ******************************************************************                                                                          
       COMPROC-CALL-SP2.
      ******************************************************************                                                                          
      *    TRY
               CALL "GSSERP.SP2" USING CP-SP2-P1 CP-SP2-P2-R
                   ON OVERFLOW
                       CONTINUE
               END-CALL
      *    CATCH
      *        CONTINUE
      *    END-TRY.
           
           
       END PROGRAM MAIN.
