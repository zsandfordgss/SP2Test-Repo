       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN AS "GSSERP.SP2TEST".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       REPOSITORY.
           CLASS cEnvironment AS "System.Environment"
           CLASS cMsgBox AS "System.Windows.Forms.MessageBox"
           CLASS cString As "System.String"
           .
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       
       COPY "SP2.CPY".
       COPY "SP2NET.CPY".
       COPY "PPSM001.SP2".
      *COPY "SCR001.LK".                                             *>TG141121
      *COPY "SCR001.WS".        
       01  formatString         OBJECT REFERENCE cString.
       01  DISPLAY-NUM          PIC ----9.
       01  WS-ONE               PIC 9 VALUE 1.

      ******************************************************************
      ** COUNTERS 
      ******************************************************************
                                                                          
       01  FORMATTING-INFO.                                                        
           05  MONTH-ORDER             PIC 9     VALUE 2.                          
           05  DAY-ORDER               PIC 9     VALUE 1.                          
           05  YEAR-ORDER              PIC 9     VALUE 3.                          
           05  DATE-SEP                PIC X     VALUE "-".                        
           05  WS-SHOW-CENTURY         PIC X     VALUE "H".                        
               88  SHOW-CENTURY        VALUE "S" .        
               88  SHOW-CENTURY-FALSE VALUE "H"           .
           05  DATE-FORMAT             PIC X(30) VALUE "dd-mm-yy".               
           05  DATE-FORMAT-LEN         PIC 9(2)  VALUE 8. 
           05  DATE-DIGITS             PIC 9(2)  VALUE 6. 
       01  WS-FD-USER-DATA             PIC X(256)  VALUE SPACE.
       01  WS-DATEPROPS-APPEARANCE-PTR PIC X(30)  VALUE SPACE.           *>SM#23002
       01  WS-DATEPROPS-APRNC-OPT-PTR  PIC X(30)  VALUE SPACE.           *>SM#23002
       01  WS-DATEPROPS-APRNC-TXTOPT-PTR                                 *>SM#23002
                                       PIC X(30)  VALUE SPACE.           *>SM#23002
           
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
       01  WS-JBS0052-EXIT              PIC X     VALUE "N".
           88  JBS0052-EXIT                       VALUE "Y".
       
       01  WS-JBS0052-OPEN              PIC X     VALUE "N".
           88  JBS0052-OPEN                       VALUE "Y".
       01  WS-IS-TEXT                   PIC X     VALUE " ".
           88  IS-TEXT                  VALUE SPACES.
           88  IS-NUMERIC               VALUE "N".
           88  IS-DATE                  VALUE "D".
       01  WS-HOLD-INITIAL-VALUE         PIC X(30)  VALUE SPACE.
       01  FLIP-DAYS-FLAG                   PIC X(01)   VALUE SPACES.    *>EB#53051
           88  FLIP-DAYS                    VALUE "Y".                   *>EB#53051
           
       01  WS-WORK-ZEROES               PIC 9(16) VALUE ZEROS.
       01  WS-LOOP-CTR                  PIC 9(9)  VALUE ZERO.
       01  WS-NEG                          PIC X(01) VALUE "N".          *>EB100401
           88  NEG                         VALUE "Y" .                   *>FUJITSU 
           88  NEG-FALSE VALUE "N"                            .          *>FUJITSU 
           88  LEADING-NEG                 VALUE "L".                    *>SM#23002
       01  WS-POS-STR-CTR               PIC 9(6)  VALUE ZERO.            *>SM#23002
       01  WS-NEG-STR-CTR               PIC 9(6)  VALUE ZERO.            *>SM#23002
       01  WS-WORK-STRING               PIC X(100).                      *>SM#23002
       01  CUR-POS-IND                  PIC 9(05).
       01  WS-START                     PIC 9(4)  VALUE ZERO.
       01  DEC-DIGITS                      PIC 9(06) VALUE ZERO.         *>EB100401
       01  NON-DEC-DIGITS                  PIC 9(06) VALUE ZERO.         *>EB100401
       01  WS-DEC-DIGITS.                                                *>SM#23002
           05  WS-DEC-DIGITS-NUM           PIC 9(6).                     *>SM#23002
       01  WS-NON-DEC-DIGITS-X.
           05  WS-NON-DEC-DIGITS           PIC 9(6).                     *>SM#2300
       01  MAX-DISPLAY-DEC                 PIC S9(2).                    *>SM#23002
       01  MAX-DISPLAY-LEN                 PIC 9(6).                     *>SM#23002
       01  UNSIGNED-2-BYTE-NUM             PIC 9(2).                     *>SM#23002
       01  WS-SIGNED-NUM                    PIC X     VALUE "Y".         *>SM#23002
           88  WS-SIGNED-NUM-YES                      VALUE "Y".         *>SM#23002
           88  WS-SIGNED-NUM-YES-FALSE                VALUE "N".         *>SM#2300
       01  MAX-LEN                         PIC 9(06) VALUE ZERO.         *>EB100401
       01  WS-LEN                       PIC 9(4)  VALUE ZERO.
       01  WS-STR-CTR                   PIC 9(6)  VALUE ZEROS. 
       01  WS-STRINGS.                                                   *>EB091012
           05  WS-STRING         PIC X(100) VALUE SPACES                 *>EB091012
                                 OCCURS 10 TIMES.                        *>EB091012
       01  SETVAL-DATA                      PIC X(2000).    
                           
       01  CP-SP2-P1                       PIC S9(4)   COMP-5.                    
       01  CP-SP2-P2                       POINTER.
       01  CP-SP2-P2-R                     REDEFINES 
           CP-SP2-P2                       PIC S9(9)   COMP-5.
           
           
       77  WS-HOLD-TEXT        PIC X(4000).                                        
      ******************************************************************
      **   Hold field ID and field name so they will stay the same.
      **   If field ID and/or field name change in our date module, our
      **   calling programs will lose accessiblity to the new field.
      ******************************************************************
       01  WS-HOLD-FD-ID                PIC S9(4)  COMP-5 VALUE ZERO.    *>SM#23002
       01  WS-HOLD-FD-NAME              PIC X(30)  VALUE SPACE.          *>SM#23002
       01  WS-HOLD-FD-HEIGHT            PIC S9(4)  COMP-5 VALUE ZERO.    *>SM#23002
       01  WS-HOLD-FD-WIDTH             PIC S9(4)  COMP-5 VALUE ZERO.    *>SM#23002
       01  WS-HOLD-FD-ROW               PIC S9(4)  COMP-5 VALUE ZERO.    *>SM#23002
       01  WS-HOLD-FD-COL               PIC S9(4)  COMP-5 VALUE ZERO.    *>SM#23002
       01  WS-LBL-FD-HEIGHT             PIC S9(4)  COMP-5 VALUE ZERO.    *>SM#23002
       01  WS-LBL-FD-WIDTH              PIC S9(4)  COMP-5 VALUE ZERO.    *>SM#23002
       01  WS-LBL-FD-ROW                PIC S9(4)  COMP-5 VALUE ZERO.    *>SM#23002
       01  WS-LBL-FD-COL                PIC S9(4)  COMP-5 VALUE ZERO.    *>SM#23002
       01  WS-HOLD-USAGE-OPTION         PIC X      VALUE SPACE.          *>SM#23002
           88  WS-HOLD-USAGE-MLE                   VALUE "m" "s" "h".    *>SM#23002
       01  WS-HOLD-FD-PROTECTION        PIC X      VALUE SPACE.          *>SM#23002
           88  WS-HOLD-FD-DISP-ONLY                VALUE 'y'.            *>SM#23002
           88  WS-HOLD-FD-GREYED                   VALUE 'g'.            *>SM#23002
           88  WS-HOLD-FD-HIDDEN                   VALUE 'h'.            *>SM#23002
           88  WS-HOLD-FD-PROTECTED                VALUE 'p'.            *>SM#23002
           88  WS-HOLD-FD-SECURED                  VALUE 's'.            *>SM#23002
       01  WS-HOLD-FORMAT-STRING        PIC X(50)  VALUE SPACE.          *>SM#23002
       01  WS-HOLD-FD-TYPE              PIC X      VALUE SPACE.          *>SM#23002
           88  WS-HOLD-FD-TYPE-NUMERIC             VALUE 'n'.            *>SM#23002
       01  WS-FD-USER-DATA-ALL          PIC X(256) VALUE SPACES.         *>SM#23002
       01  WS-HOLD-INT                  PIC 9(9)   VALUE ZERO.           *>SM#23002
       01  WS-HOLD-FONT-ID              PIC S9(4)  COMP-5.               *>SM#23002
       01  WS-HOLD-FONT-FACE            PIC X(30)  VALUE SPACE.          *>SM#23002
       01  WS-HOLD-STRING               PIC X(30)  VALUE SPACE.          *>SM#23002
       01  WS-EDIT-OBJ-NAME             PIC X(300) VALUE SPACE.          *>SM#23002
       01  WS-HOLD-BTN-TEXT             PIC X(100) VALUE SPACE.          *>SM#23002
       01  WS-HOLD-VAR-LENS             PIC S9(9) COMP-5 VALUE ZERO.     *>SM#23002
       01  WS-HOLD-DISCRETE-VALUES      PIC X(2)   VALUE SPACE.          *>SM#23002
       01  CHAR                         PIC X      VALUE LOW-VALUES.     *>SM#23002
       01  INT                          PIC 9(9)   VALUE ZERO.           *>SM#23002
       01  BIT-ARRAY.                                                    *>SM#23002
           05 BIT-ELEMENT               PIC 9      VALUE ZERO            *>SM#23002
                                        OCCURS 8 TIMES.                  *>SM#23002
       01  WS-THEME-NAME                PIC X(30)     VALUE SPACE.       *>SM#23002
           88  WS-THEME-OFFICE-13                     VALUE              *>SM#23002
               'Office2013'.                                             *>SM#23002
           88  WS-THEME-METRO-LIGHT                   VALUE              *>SM#23002
               'MetropolisLight'.                                        *>SM#23002
           88  WS-THEME-METRO-DARK                    VALUE              *>SM#23002
               'MetropolisDark'.                                         *>SM#23002
           88  WS-THEME-DX                            VALUE              *>SM#23002
               'DXStyle'.                                                *>SM#23002
           88  WS-THEME-OFFICE-10-BLACK               VALUE              *>SM#23002
               'Office2010Black'.                                        *>SM#23002
           88  WS-THEME-OFFICE-10-BLUE                VALUE              *>SM#23002
               'Office2010Blue'.                                         *>SM#23002
           88  WS-THEME-SEVEN                         VALUE              *>SM#23002
               'Seven'.                                                  *>SM#23002
           88  WS-THEME-VS-10                         VALUE              *>SM#23002
               'VisualStudio2010'.                                       *>SM#23002
           88  WS-THEME-OFFICE-7-BLUE                 VALUE              *>SM#23002
               'Office2007Blue'.                                         *>SM#23002
           88  WS-THEME-OFFICE-7-SILVER               VALUE              *>SM#23002
               'Office2007Silver'.                                       *>SM#23002
           88  WS-THEME-OFFICE-7-BLACK                VALUE              *>SM#23002
               'Office2007Black'.                                        *>SM#23002
       01  LONG-STRING                  PIC X(8000)   VALUE SPACES.      *>SM#23002
       01  LONG-STRING-THEME-PARM.                                       *>SM#23002
           05  LONG-STRING-THEME-PARM-NAME                               *>SM#23002
                                        PIC X(40)     VALUE SPACES.      *>SM#23002
           05  LONG-STRING-THEME-PARM-VALUE                              *>SM#23002
                                        PIC X(8000)   VALUE SPACES.      *>SM#23002
       01  COLOR-ARRAY.                                                  *>SM#23002
      ****************************************************************   *>SM#23002
      **   Color rows mapped to theme colors. Each row contains:         *>SM#23002
      **   6 byte alpha color code read out of environment variable      *>SM#23002
      **   4-byte numeric value used to set SP2-COLOR                    *>SM#23002
      **   1 byte color num returned from SP2 SET COLOR.                 *>SM#23002
      ****************************************************************   *>SM#23002
           02  DETAILED-ROW-AREA.                                        *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  BACK-COLOR               PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  BACK-HEX-COLOR.                                   *>SM#23002
                       15  BACK-HEX-COLOR-R     PIC X.                   *>SM#23002
                       15  BACK-HEX-COLOR-G     PIC X.                   *>SM#23002
                       15  BACK-HEX-COLOR-B     PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FORE-COLOR               PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FORE-HEX-COLOR.                                   *>SM#23002
                       15  FORE-HEX-COLOR-R     PIC X.                   *>SM#23002
                       15  FORE-HEX-COLOR-G     PIC X.                   *>SM#23002
                       15  FORE-HEX-COLOR-B     PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  BORDER-COLOR             PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  BORDER-HEX-COLOR.                                 *>SM#23002
                       15  BORDER-HEX-COLOR-R   PIC X.                   *>SM#23002
                       15  BORDER-HEX-COLOR-G   PIC X.                   *>SM#23002
                       15  BORDER-HEX-COLOR-B   PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FIELD-BACK-COLOR         PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FIELD-BACK-HEX-COLOR.                             *>SM#23002
                       15  FIELD-BACK-HEX-COLOR-R                        *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  FIELD-BACK-HEX-COLOR-G                        *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  FIELD-BACK-HEX-COLOR-B                        *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FIELD-FORE-COLOR         PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FIELD-FORE-HEX-COLOR.                             *>SM#23002
                       15  FIELD-FORE-HEX-COLOR-R                        *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  FIELD-FORE-HEX-COLOR-G                        *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  FIELD-FORE-HEX-COLOR-B                        *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  SEL-FOCUS-COLOR          PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  SEL-FOCUS-HEX-COLOR.                              *>SM#23002
                       15  SEL-FOCUS-HEX-COLOR-R                         *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  SEL-FOCUS-HEX-COLOR-G                         *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  SEL-FOCUS-HEX-COLOR-B                         *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  SEL-UNFOCUS-COLOR        PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  SEL-UNFOCUS-HEX-COLOR.                            *>SM#23002
                       15  SEL-UNFOCUS-HEX-COLOR-R                       *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  SEL-UNFOCUS-HEX-COLOR-G                       *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  SEL-UNFOCUS-HEX-COLOR-B                       *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  BUTTON-BACK-COLOR        PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  BUTTON-BACK-HEX-COLOR.                            *>SM#23002
                       15  BUTTON-BACK-HEX-COLOR-R                       *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  BUTTON-BACK-HEX-COLOR-G                       *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  BUTTON-BACK-HEX-COLOR-B                       *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  CHK-BUTTON-BACK-COLOR    PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  CHK-BUTTON-BACK-HEX-COLOR.                        *>SM#23002
                       15  CHK-BUTTON-BACK-HEX-COLOR-R                   *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  CHK-BUTTON-BACK-HEX-COLOR-G                   *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  CHK-BUTTON-BACK-HEX-COLOR-B                   *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  BUTTON-MD-BACK-COLOR     PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  BUTTON-MD-BACK-HEX-COLOR.                         *>SM#23002
                       15  BUTTON-MD-BACK-HEX-COLOR-R                    *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  BUTTON-MD-BACK-HEX-COLOR-G                    *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  BUTTON-MD-BACK-HEX-COLOR-B                    *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  BUTTON-MO-BACK-COLOR     PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  BUTTON-MO-BACK-HEX-COLOR.                         *>SM#23002
                       15  BUTTON-MO-BACK-HEX-COLOR-R                    *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  BUTTON-MO-BACK-HEX-COLOR-G                    *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  BUTTON-MO-BACK-HEX-COLOR-B                    *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-LINES-COLOR         PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-LINES-HEX-COLOR.                             *>SM#23002
                       15  GRID-LINES-HEX-COLOR-R                        *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-LINES-HEX-COLOR-G                        *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-LINES-HEX-COLOR-B                        *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-BACK-COLOR          PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-BACK-HEX-COLOR-ROW.                          *>SM#23002
                       15  GRID-BACK-HEX-COLOR-R                         *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-BACK-HEX-COLOR-G                         *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-BACK-HEX-COLOR-B                         *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-CH-BACK-COLOR       PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-CH-BACK-HEX-COLOR.                           *>SM#23002
                       15  GRID-CH-BACK-HEX-COLOR-R                      *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CH-BACK-HEX-COLOR-G                      *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CH-BACK-HEX-COLOR-B                      *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-CH-FORE-COLOR       PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-CH-FORE-HEX-COLOR.                           *>SM#23002
                       15  GRID-CH-FORE-HEX-COLOR-R                      *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CH-FORE-HEX-COLOR-G                      *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CH-FORE-HEX-COLOR-B                      *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-CH-SEL-BACK-COLOR   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-CH-SEL-BACK-HEX-COLOR.                       *>SM#23002
                       15  GRID-CH-SEL-BACK-HEX-COLOR-R                  *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CH-SEL-BACK-HEX-COLOR-G                  *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CH-SEL-BACK-HEX-COLOR-B                  *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-CH-SEL-FORE-COLOR   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-CH-SEL-FORE-HEX-COLOR.                       *>SM#23002
                       15  GRID-CH-SEL-FORE-HEX-COLOR-R                  *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CH-SEL-FORE-HEX-COLOR-G                  *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CH-SEL-FORE-HEX-COLOR-B                  *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-RH-BACK-COLOR       PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-RH-BACK-HEX-COLOR.                           *>SM#23002
                       15  GRID-RH-BACK-HEX-COLOR-R                      *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-RH-BACK-HEX-COLOR-G                      *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-RH-BACK-HEX-COLOR-B                      *>SM#23002
                                                PIC X.                   *>SM#23002
                05  FILLER.                                              *>SM#23002
                   10  GRID-RH-FORE-COLOR       PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-RH-FORE-HEX-COLOR.                           *>SM#23002
                       15  GRID-RH-FORE-HEX-COLOR-R                      *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-RH-FORE-HEX-COLOR-G                      *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-RH-FORE-HEX-COLOR-B                      *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-RH-SEL-BACK-COLOR   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10 GRID-RH-SEL-BACK-HEX-COLOR.                        *>SM#23002
                       15  GRID-RH-SEL-BACK-HEX-COLOR-R                  *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-RH-SEL-BACK-HEX-COLOR-G                  *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-RH-SEL-BACK-HEX-COLOR-B                  *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-RH-SEL-FORE-COLOR   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-RH-SEL-FORE-HEX-COLOR.                       *>SM#23002
                       15  GRID-RH-SEL-FORE-HEX-COLOR-R                  *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-RH-SEL-FORE-HEX-COLOR-G                  *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-RH-SEL-FORE-HEX-COLOR-B                  *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-CELL-BACK-COLOR     PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-CELL-BACK-HEX-COLOR.                         *>SM#23002
                       15 GRID-CELL-BACK-HEX-COLOR-R                     *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CELL-BACK-HEX-COLOR-G                    *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CELL-BACK-HEX-COLOR-B                    *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-CELL-FORE-COLOR     PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-CELL-FORE-HEX-COLOR.                         *>SM#23002
                       15  GRID-CELL-FORE-HEX-COLOR-R                    *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CELL-FORE-HEX-COLOR-G                    *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CELL-FORE-HEX-COLOR-B                    *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-CELL-SEL-BACK-COLOR PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-CELL-SEL-BACK-HEX-COLOR.                     *>SM#23002
                       15  GRID-CELL-SEL-BACK-HEX-COLOR-R                *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CELL-SEL-BACK-HEX-COLOR-G                *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CELL-SEL-BACK-HEX-COLOR-B                *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  GRID-CELL-SEL-FORE-COLOR PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  GRID-CELL-SEL-FORE-HEX-COLOR.                     *>SM#23002
                       15  GRID-CELL-SEL-FORE-HEX-COLOR-R                *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CELL-SEL-FORE-HEX-COLOR-G                *>SM#23002
                                                PIC X.                   *>SM#23002
                       15  GRID-CELL-SEL-FORE-HEX-COLOR-B                *>SM#23002
                                                PIC X.                   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
               05  FILLER.                                               *>SM#23002
                   10  FILLER                   PIC X(6)  VALUE SPACE.   *>SM#23002
                   10  FILLER                   PIC X(3)  VALUE SPACE.   *>SM#23002
                                                                         *>SM#23002
      ****************************************************************   *>SM#23002
      **   Generic color row area for reading in rows.                   *>SM#23002
      ****************************************************************   *>SM#23002
           02  FILLER                       REDEFINES                    *>SM#23002
               DETAILED-ROW-AREA.                                        *>SM#23002
               05  COLOR-DATA               OCCURS 40 TIMES.             *>SM#23002
                   10  ALPHA-COLOR-ROW      PIC X(6).                    *>SM#23002
                   10  HEX-COLOR-ROW.                                    *>SM#23002
                       15  HEX-COLOR-R      PIC X.                       *>SM#23002
                       15  HEX-COLOR-G      PIC X.                       *>SM#23002
                       15  HEX-COLOR-B      PIC X.                       *>SM#23002
                                                                         *>SM#23002
      ****************************************************************   *>SM#23002
      **   Color fields for converting between hex and intger values.    *>SM#23002
      ****************************************************************   *>SM#23002
       01  WS-ALPHA-COLOR.                                               *>SM#23002
           10  WS-ALPHA-COLOR-R            PIC X(2).                     *>SM#23002
           10  WS-ALPHA-COLOR-G            PIC X(2).                     *>SM#23002
           10  WS-ALPHA-COLOR-B            PIC X(2).                     *>SM#23002
       01  WS-HEX-COLOR.                                                 *>SM#23002
           10  WS-HEX-COLOR-R              PIC X.                        *>SM#23002
           10  WS-HEX-COLOR-G              PIC X.                        *>SM#23002
           10  WS-HEX-COLOR-B              PIC X.                        *>SM#23002
      ****************************************************************   *>SM#23002
      **   RGB Color Codes for .NET fields.                              *>SM#23002
      ****************************************************************   *>SM#23002
       01  WS-FIELD-BORDER-COLOR.                                        *>SM#23002
           10  BORDER-INT-COLOR-R               PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  BORDER-INT-COLOR-G               PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  BORDER-INT-COLOR-B               PIC 9(3)  VALUE ZERO.    *>SM#23002
       01  WS-FIELD-BACK-COLOR.                                          *>SM#23002
           10  BACK-INT-COLOR-R                 PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  BACK-INT-COLOR-G                 PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  BACK-INT-COLOR-B                 PIC 9(3)  VALUE ZERO.    *>SM#23002
       01  WS-FIELD-FORE-COLOR.                                          *>SM#23002
           10  FORE-INT-COLOR-R                 PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  FORE-INT-COLOR-G                 PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  FORE-INT-COLOR-B                 PIC 9(3)  VALUE ZERO.    *>SM#23002
       01  WS-BUTTON-BACK-COLOR.                                         *>SM#23002
           10  BTN-BACK-INT-COLOR-R             PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  BTN-BACK-INT-COLOR-G             PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  BTN-BACK-INT-COLOR-B             PIC 9(3)  VALUE ZERO.    *>SM#23002
       01  WS-BUTTON-MO-COLOR.                                           *>SM#23002
           10  BTN-MO-INT-COLOR-R               PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  BTN-MO-INT-COLOR-G               PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  BTN-MO-INT-COLOR-B               PIC 9(3)  VALUE ZERO.    *>SM#23002
       01  WS-BUTTON-MD-COLOR.                                           *>SM#23002
           10  BTN-MD-INT-COLOR-R               PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  BTN-MD-INT-COLOR-G               PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  BTN-MD-INT-COLOR-B               PIC 9(3)  VALUE ZERO.    *>SM#23002
       01  WS-BTN-SEL-BORDER-COLOR.                                      *>SM#23002
           10  BTN-SEL-BORDER-INT-COLOR-R       PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  BTN-SEL-BORDER-INT-COLOR-G       PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  BTN-SEL-BORDER-INT-COLOR-B       PIC 9(3)  VALUE ZERO.    *>SM#23002
      ****************************************************************   *>SM#23002
      **   Hardcoded RGB Color Codes for Buttons.                        *>SM#23002
      **   These colors are generated dynamically in the menu and are
      **   not in the theme environment variable.
      ****************************************************************   *>SM#23002
       01  WS-OFFICE-13-BORDER.                                          *>SM#23002
           10  OFFICE-13-BORDER-R               PIC 9(3)  VALUE ZERO.    *>SM#23002
           10  OFFICE-13-BORDER-G               PIC 9(3)  VALUE 114.     *>SM#23002
           10  OFFICE-13-BORDER-B               PIC 9(3)  VALUE 198.     *>SM#23002
       01  WS-DX-BORDER.                                                 *>SM#23002
           10  DX-BORDER-R                      PIC 9(3)  VALUE 153.     *>SM#23002
           10  DX-BORDER-G                      PIC 9(3)  VALUE 167.     *>SM#23002
           10  DX-BORDER-B                      PIC 9(3)  VALUE 200.     *>SM#23002
       01  WS-OFFICE-10-BLACK-BORDER.                                    *>SM#23002
           10  OFFICE-10-BLACK-BORDER-R         PIC 9(3)  VALUE 224.     *>SM#23002
           10  OFFICE-10-BLACK-BORDER-G         PIC 9(3)  VALUE 186.     *>SM#23002
           10  OFFICE-10-BLACK-BORDER-B         PIC 9(3)  VALUE 33.      *>SM#23002
       01  WS-OFFICE-10-BLUE-BORDER.                                     *>SM#23002
           10  OFFICE-10-BLUE-BORDER-R          PIC 9(3)  VALUE 243.     *>SM#23002
           10  OFFICE-10-BLUE-BORDER-G          PIC 9(3)  VALUE 208.     *>SM#23002
           10  OFFICE-10-BLUE-BORDER-B          PIC 9(3)  VALUE 60.      *>SM#23002
       01  WS-SEVEN-BORDER.                                              *>SM#23002
           10  SEVEN-BORDER-R                   PIC 9(3)  VALUE 60.      *>SM#23002
           10  SEVEN-BORDER-G                   PIC 9(3)  VALUE 127.     *>SM#23002
           10  SEVEN-BORDER-B                   PIC 9(3)  VALUE 177.     *>SM#23002
       01  WS-VS-10-BORDER.                                              *>SM#23002
           10  VS-10-BORDER-R                   PIC 9(3)  VALUE 221.     *>SM#23002
           10  VS-10-BORDER-G                   PIC 9(3)  VALUE 190.     *>SM#23002
           10  VS-10-BORDER-B                   PIC 9(3)  VALUE 59.      *>SM#23002
       01  WS-OFFICE-7-BORDER.                                           *>SM#23002
           10  OFFICE-7-BORDER-R                PIC 9(3)  VALUE 193.     *>SM#23002
           10  OFFICE-7-BORDER-G                PIC 9(3)  VALUE 169.     *>SM#23002
           10  OFFICE-7-BORDER-B                PIC 9(3)  VALUE 101.     *>SM#23002
      ****************************************************************   *>SM#23002
      **   Custom theme SP2 color numbers (ID's).                        *>SM#23002
      ****************************************************************   *>SM#23002
       01  WS-WINDOW-COLOR-NUM                  PIC S9(4) VALUE ZERO.    *>SM#23002
       01  WS-BORDER-COLOR-NUM                  PIC S9(4) VALUE ZERO.    *>SM#23002
       01  WS-FIELD-COLOR-NUM                   PIC S9(4) VALUE ZERO.    *>SM#23002
       01  WS-BUTTON-COLOR-NUM                  PIC S9(4) VALUE ZERO.    *>SM#23002
      ****************************************************************   *>SM#23002
      **   Custom theme SP2 color char codes.                            *>SM#23002
      ****************************************************************   *>SM#23002
       01  WS-WINDOW-COLOR-CHAR                 PIC X     VALUE ZERO.    *>SM#23002
       01  WS-BORDER-COLOR-CHAR                 PIC X     VALUE ZERO.    *>SM#23002
       01  WS-FIELD-COLOR-CHAR                  PIC X     VALUE ZERO.    *>SM#23002
       01  WS-BUTTON-COLOR-CHAR                 PIC X     VALUE ZERO.    *>SM#23002
       01  CUSTOM-COLOR-NUM                     PIC S9(04) VALUE ZERO.   *>SM#23002
       01  CUSTOM-COLOR-CHAR                    PIC X(01) VALUE ZERO.    *>SM#23002
           
       01  WS-HOLD-EDIT-MASK                PIC X(100)  VALUE SPACE.     *>SM#23002
       01  WS-WORK-EDIT-MASK                PIC X(100)  VALUE SPACE.     *>SM#23002
       01  WS-HOLD-DISP-FMT                 PIC X(100)  VALUE SPACE.     *>SM#23002
       01  WS-HOLD-NEG-MASK                 PIC X(50)   VALUE SPACE.     *>SM#23002
       01  WS-HOLD-POS-MASK                 PIC X(50)   VALUE SPACE.     *>SM#23002
       01  WS-HOLD-CAPTION                  PIC X(300)  VALUE SPACE.     *>SM#23002
       01  WS-WORK-CAPTION                  PIC X(300)  VALUE SPACE.     *>SM#23002
       01  WS-HOLD-FORMAT-LEN               PIC S9(4)   COMP-5.          *>SM#23002
       01  WS-HOLD-USER-LEN                 PIC S9(4)   COMP-5.          *>SM#23002
       01  WS-OBJ-NAME-LEN                  PIC S9(09)  VALUE ZEROS.     *>SM#23002
       01  WS-HOLD-CAPTION-LEN              PIC S9(4)   COMP-5.          *>SM#23002
       01  WS-ALPHA-CAPTION-LEN             PIC X(8)    VALUE SPACE.     *>SM#23002
       01  WS-HOLD-FD-JUSTIFY               PIC X       VALUE SPACE.     *>SM#23002
           88  WS-HOLD-FD-JUSTIFY-RIGHT                 VALUE 'r'.       *>SM#23002
       01  WS-PROP-NAME                     PIC X(80)   VALUE SPACE.     *>SM#23002
       01  WS-NE-RET-CODE                   PIC X(4)    VALUE SPACE.     *>SM#23002
       01  WS-NE-OPTIONS                    PIC X(2)    VALUE SPACE.     *>SM#23002
       01  CUR-IND                          PIC 9(9)    VALUE ZERO.      *>SK#50890
       01  WS-MANUAL-PIC-FLAG  PIC X            VALUE SPACE.             *>SM#23002
           88  WS-MANUAL-PIC                    VALUE "Y".               *>SM#23002
       01  WS-APPLY-THEME      PIC X            VALUE SPACE.             *>SM#23002
           88  WS-APPLY-THEME-YES               VALUE "Y".               *>SM#23002
       01  WS-DATE-TAG-FLAG            PIC X      VALUE "N".             *>SM#23002
           88  WS-DATE-TAG-FOUND                  VALUE "Y".             *>SM#23002
           88  WS-DATENB-TAG-FOUND                VALUE "B".             *>SM#23002
           88  WS-DATE-TAG-FOUND-FALSE            VALUE "N".             *>SM#23002
       01  WS-GRID-TAG-FLAG            PIC X      VALUE "N".             *>SM#23002
           88  WS-GRID-TAG-FOUND                  VALUE "Y".             *>SM#23002
       01  WS-CREATE-LABEL-FLAG        PIC X      VALUE SPACE.           *>SM#23002
           88  WS-CREATE-LABEL                    VALUE "Y".
       01  WS-INTEGER-STRING     PIC X(20)  VALUE SPACE.                 *>SM#23002
       01  WS-DECIMAL-STRING     PIC X(20)  VALUE SPACE.                 *>SM#23002
       01  WS-MASK-SEPARATOR     PIC X      VALUE SPACE.                 *>SM#23002
       01  INT-CHAR              PIC X      VALUE SPACE.                 *>SM#23002
       01  DEC-CHAR              PIC X      VALUE SPACE.                 *>SM#23002
       01  ws-mask-ptr           pic x(80)  VALUE SPACE.                 *>SM#23002
       01  ws-properties-ptr     pic x(80)  VALUE SPACE.                 *>SM#23002
       01  WS-LAST-SP2-PTR       PIC X(80)  VALUE SPACE.                 *>SM#23002
       01  WS-SP2-DATE-PTR       pic x(80)  VALUE SPACE.                 *>SM#23002
       01  WS-RepositoryItemDateEdit-PTR                                 *>SM#23002
                                 pic x(80)  VALUE SPACE.                 *>SM#23002
       01  WS-RepositoryItemTextEdit-PTR                                 *>SM#23002
                                 pic x(80)  VALUE SPACE.                 *>SM#23002
       01  WS-RepositoryItemMemoEdit-PTR                                 *>SM#23002
                                 pic x(80)  VALUE SPACE.                 *>SM#23002
       01  WS-DisplayFormat-PTR  pic x(80)  VALUE SPACE.                 *>SM#23002
       01  WS-DEVX-FONT-PTR      PIC X(80)  VALUE SPACE.                 *>SM#23002
       01  WS-TEXTPROPS-APPEARANCE-PTR                                   *>SM#23002
                                 PIC X(80)  VALUE SPACE.                 *>SM#23002
       01  WS-TEXTPROPS-APRNC-OPT-PTR                                    *>SM#23002
                                 PIC X(80)  VALUE SPACE.                 *>SM#23002
       01  WS-TEXTPROPS-APRNC-TXTOPT-PTR                                 *>SM#23002
                                 PIC X(80)  VALUE SPACE.                 *>SM#23002
       01  WS-BORDER-COLOR-PTR   PIC X(80)  VALUE SPACE.                 *>SM#23002
       01  WS-FIELD-BACK-COLOR-PTR                                       *>SM#23002
                                 PIC X(80)  VALUE SPACE.                 *>SM#23002
       01  WS-FIELD-FORE-COLOR-PTR                                       *>SM#23002
                                 PIC X(80)  VALUE SPACE.                 *>SM#23002
       01  ws-label-ptr          PIC X(80)  VALUE SPACE.                 *>SM#23002
       01  ws-point-ptr          PIC X(80)  VALUE SPACE.                 *>SM#23002
       01  WS-LookAndFeel-PTR    PIC X(80)  VALUE SPACE.                 *>SM#23002
       01  ws-font-ptr           PIC X(80)  VALUE SPACE.                 *>SM#23002
       01  WS-RepoItemDateEdit-PTR                                       *>SM#23002
                                       PIC X(30)  VALUE SPACES.          *>SM#23002
       01  ws-button-collection-ptr    PIC X(30)  VALUE SPACE.           *>SM#23002
       01  ws-button-ptr               PIC X(30)  VALUE SPACE.           *>SM#23002
       01  ws-property-ptr             PIC X(30)  VALUE SPACE.           *>SM#23002
       
       01  FLD-ID                PIC 9(09).
       01  WS-FD-OTHER-VAR-DATA-LEN PIC 9(09) VALUE ZERO.
       01  WS-FD-OTHER-VAR-DATA     PIC X(100) VALUE ZERO.
       01  WS-SP2-KEY-SELECT                PIC X(24)    VALUE LOW-VALUES.  *>MJ#55459
           88 WS-SP2-KEY-SELECT-TRUE                    VALUE "/Click:-2/DoubleClick:-2".   *>MJ#55459
           88 WS-SP2-KEY-SELECT-FALSE                   VALUE LOW-VALUES.  *>MJ#54568
       
           
           
           
           
       01  CUR-FIELD PIC 9(09) VALUE ZERO.
       01  NEW-LENGTH PIC 9(09) VALUE ZERO.
       
       77  WS-PPSM001-SCR-ID        PIC 9(06) VALUE 480502.
       
       LINKAGE SECTION.
       01  BLANK-LINKS PIC X(2500).
ord0bg PROCEDURE DIVISION.
       MAINLINE.
       
           PERFORM INITIALIZATION
           IF SP2-FI-RET-CODE NOT = 0
               EXIT PROGRAM
           END-IF
           PERFORM PPSM001-PROCESSING
           PERFORM END-PROGRAM
           .
       
       END-PROGRAM.
           IF JBS0052-OPEN   
               PERFORM CLOSE-SCREEN              
           END-IF
           GOBACK
           .
           
       CLOSE-SCREEN.      
           MOVE SP2-CLOSE-WINDOW TO CP-SP2-P1                       
           SET CP-SP2-P2 TO ADDRESS OF SP2-NULL-PARM        
           PERFORM COMPROC-CALL-SP2
           INITIALIZE WS-JBS0052-OPEN
           .
           
       INITIALIZATION.
           INVOKE cEnvironment "SetEnvironmentVariable" USING "SP2VST", "1"
           INVOKE cEnvironment "SetEnvironmentVariable" USING "SP2EBC", "1"
           INVOKE cEnvironment "SetEnvironmentVariable" USING "SP2GRI", "1"
           INVOKE cEnvironment "SetEnvironmentVariable" USING "SP2CEN", "75"
           INVOKE cEnvironment "SetEnvironmentVariable" USING "SP2CHK", "20"
           
           INVOKE cEnvironment "SetEnvironmentVariable" USING "QPRMTH", "1"   
           INVOKE cEnvironment "SetEnvironmentVariable" USING "SP2HIN", "0"   
           INVOKE cEnvironment "SetEnvironmentVariable" USING "SP2EDT", "133" 
           INVOKE cEnvironment "SetEnvironmentVariable" USING "SP2SAV", "4"   
           INVOKE cEnvironment "SetEnvironmentVariable" USING "SP2OWN", "12"  
           
           INVOKE cEnvironment "SetEnvironmentVariable" USING "SP2DTE", "1"   *>THIS CAUSES THE BUG
           
           PERFORM GET-OPTIONS
      **   INVOKE cEnvironment "SetEnvironmentVariable" USING "SP2DIR", "C:\LocalCOBOLSearch\trunk\cobol\panels"
           .
           
                                                                            
      ******************************************************************     
       GET-OPTIONS.                                                                
      ******************************************************************   
      
      *    INITIALIZE SCR010-LINKS                                                 
      *    SET SCR010-GET-DISPLAY-FORMAT TO TRUE                                   
      *    CALL "GSSERP.SCR010" USING SCR010-LINKS
      *      ON OVERFLOW                                                           
      *        SET SCR010-FAILED TO TRUE                                           
      *    END-CALL                                                                
      *                                                                            
      *    IF SCR010-SUCCESSFUL                                                    
      *        MOVE SCR010-SD-MONTH-ORDER TO MONTH-ORDER                          
      *        MOVE SCR010-SD-DAY-ORDER TO DAY-ORDER                            
      *        MOVE SCR010-SD-YEAR-ORDER TO YEAR-ORDER                           
      *        MOVE SCR010-SD-DATE-SEP TO DATE-SEP                             
      *        MOVE SCR010-SD-SHOW-CENTURY TO WS-SHOW-CENTURY                      
      *    ELSE                                                                    
      *        MOVE DATE-FORMAT TO SCR010-SD-PIC                        
      *    END-IF.                                                                 
      *                                                                            
      *    MOVE FUNCTION UPPER-CASE(SCR010-SD-PIC) TO COMPROC-STRING
      *    MOVE COMPROC-STRING(1 : 20) TO SCR010-SD-PIC(1 : 20)
      *                                                                            
      *    MOVE ZERO TO DATE-DIGITS.                                 
      *    PERFORM VARYING WS-CTR1 FROM 1 BY 1                                     
      *        UNTIL WS-CTR1 > 20                                                  
      *        IF SCR010-SD-PIC(WS-CTR1 : 1) <= SPACES                             
      *            EXIT PERFORM                                                    
      *        END-IF                                                              
      *        IF SCR010-SD-PIC(WS-CTR1:1) = "M"                                   
      *            OR SCR010-SD-PIC(WS-CTR1:1) = "D"                               
      *            OR SCR010-SD-PIC(WS-CTR1:1) = "Y"                               
      *            MOVE "9" TO SCR010-SD-PIC(WS-CTR1:1)                         
      *            ADD 1 TO DATE-DIGITS                                      
      *        END-IF                                                              
      *    END-PERFORM.                                                            
      *    MOVE SCR010-SD-PIC TO DATE-FORMAT.                                 
      *    COMPUTE DATE-FORMAT-LEN = COMPROC-ZERO + WS-CTR1 - 1
                          . 

       PPSM001-PROCESSING.
           PERFORM DISPLAY-PPSM001
           
           PERFORM UNTIL JBS0052-EXIT
               PERFORM JBS0052-CONVERSE
               
               IF PPSM001-KEY = SP2-KEY-CTRL-FIELD
                   MOVE LOW-VALUES TO SP2-MS-DATA                                         
                   MOVE 2 TO SP2-MS-LINE-CNT                                          
                   MOVE "s"   TO SP2-MS-ICON                                              
                   MOVE "o"   TO SP2-MS-BUTTON                                            
                   MOVE LOW-VALUES TO SP2-MS-CANCEL                                       
                   MOVE "SP2 Key Received" TO SP2-MS-TITLE                       
                   MOVE "Field change event triggerred" TO SP2-MS-TEXT                                         
                   MOVE SP2-DISPLAY-MESSAGE TO CP-SP2-P1                         *>FUJITSU 
                   SET CP-SP2-P2 TO ADDRESS OF SP2-MESSAGE-DATA                  *>FUJITSU 
                   PERFORM COMPROC-CALL-SP2 
               END-IF
               
               IF PPSM001-OK-BTN-HIT
      ** Ugly but easier to follow. We grab all FD for PPSM001
                   PERFORM GET-NEXT-FIELD-DEF
      *  Then we set subpanel PPSMHLD1 to active and get FDs (Only contains subpanels)                   
                   MOVE "PPSMHLD1" TO SP2-ND-NAME
                   PERFORM SET-ACTIVE-SCREEN 
                   PERFORM GET-NEXT-FIELD-DEF
      *  Then we set subpanel PPSTOPT to active and get FDs.
      *  Browser lookup icons are returning 0s for the GUI-ID.
                   MOVE "PPSTOPT" TO SP2-ND-NAME
                   PERFORM SET-ACTIVE-SCREEN
                   PERFORM GET-NEXT-FIELD-DEF 
               END-IF
      
               PERFORM JBS0052-EVENT-HANDLER
           END-PERFORM
           PERFORM CLOSE-SCREEN
           .    
      
      ******************************************************************
       GET-NEXT-FIELD-DEF.
      ******************************************************************   
           MOVE -1 TO SP2-FD-RET-CODE
           PERFORM UNTIL SP2-FD-RET-CODE = 1
              MOVE LOW-VALUES TO SP2-FD-DATA
                                 SP2-FD-VAR-DATA 
              MOVE LOW-VALUES TO SP2-FD-VAR-LENS
              MOVE 4000 TO SP2-FD-VAR-LEN       
              MOVE SP2-GET-NEXT-FIELD-DEF TO CP-SP2-P1                     
              SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF  
              PERFORM COMPROC-CALL-SP2 
              IF SP2-FD-ID = PPSTOPT-CL-DN-ARROW-I 
              AND SP2-ND-NAME = "PPSTOPT"
              AND SP2-FD-GUI-ID = ZEROES                       
                 DISPLAY "Reached CL-DN-ARROW, GUI-ID = Zeroes"                               
              END-IF                                                                                       
           END-PERFORM.
      
      ******************************************************************
       GET-FIELD-DEF.
      ******************************************************************
           MOVE LOW-VALUES TO SP2-FD-DATA
                              SP2-FD-VAR-DATA
                              SP2-FD-VAR-LENS
           MOVE 4000 TO SP2-FD-VAR-LEN
           MOVE PPSTOPT-CL-DN-ARROW-I TO SP2-FD-ID
           MOVE SP2-GET-FIELD-DEF TO CP-SP2-P1
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF
           PERFORM COMPROC-CALL-SP2.
           
      ******************************************************************
       SET-ACTIVE-SCREEN.
      ******************************************************************   
           MOVE SP2-ACTIVATE-WINDOW TO CP-SP2-P1                     *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF SP2-NAME-DEF                  *>FUJITSU 
           PERFORM COMPROC-CALL-SP2.                                  *>FUJITSU            
                                                           
      ******************************************************************
       SET-PROGRAM-LENGTH.                                              
      ******************************************************************
      **   Call SP2 to set program length property using                
      **   SP2-PR-KEY = FN-000280002N                                   
      ******************************************************************
           EXIT PARAGRAPH
           MOVE LOW-VALUES TO SP2-PR-DATA                               
           MOVE FLD-ID TO SP2-PR-ID                                
           SET SP2-PR-FIELD TO TRUE                                     
           SET SP2-PR-NUM-T TO TRUE                                     
           MOVE 28 TO SP2-PR-OFFSET                                     
           MOVE 2 TO SP2-PR-LEN                                         
           SET SP2-PR-NUMBER TO TRUE                                    
           COMPUTE SP2-PR-NUM-VALUE                                   
               = NEW-LENGTH
           SET SP2-PR-RECREATE TO TRUE
           MOVE SP2-SET-PROPERTY TO CP-SP2-P1                           
           SET CP-SP2-P2 TO ADDRESS OF SP2-PROPERTY                     
           PERFORM COMPROC-CALL-SP2.     
                                                                            
      ******************************************************************
       SET-MAX-LENGTH.                                                  
      ******************************************************************
      **   Call SP2 to set maximum length property using                
      **   SP2-PR-KEY = FN-000260002N                                   
      ******************************************************************
           MOVE LOW-VALUES TO SP2-PR-DATA                               
           MOVE FLD-ID TO SP2-PR-ID                                
           SET SP2-PR-FIELD TO TRUE                                     
           SET SP2-PR-NUM-T TO TRUE                                     
           MOVE 26 TO SP2-PR-OFFSET                                     
           MOVE 2 TO SP2-PR-LEN                                         
           SET SP2-PR-NUMBER TO TRUE
           COMPUTE SP2-PR-NUM-VALUE 
               = NEW-LENGTH       
           SET SP2-PR-RECREATE TO TRUE
           MOVE SP2-SET-PROPERTY TO CP-SP2-P1                           
           SET CP-SP2-P2 TO ADDRESS OF SP2-PROPERTY                     
           PERFORM COMPROC-CALL-SP2.       
              
     
       DISPLAY-PPSM001. 
007951                                                                             
007960     MOVE SP2-SET-RECORD TO CP-SP2-P1                              *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF PPSM001-PANEL-RECORD              *>FUJITSU 
           PERFORM COMPROC-CALL-SP2                                      *>FUJITSU 
007960     MOVE SP2-SET-RECORD TO CP-SP2-P1                              *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF PPSMHLD1-PANEL-RECORD             *>FUJITSU 
           PERFORM COMPROC-CALL-SP2                                      *>FUJITSU 
007960     MOVE SP2-SET-RECORD TO CP-SP2-P1                              *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF PPSTOPT-PANEL-RECORD              *>FUJITSU 
           PERFORM COMPROC-CALL-SP2                                      *>FUJITSU 
007960     MOVE SP2-SET-RECORD TO CP-SP2-P1                              *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF PPSTCOM-PANEL-RECORD              *>FUJITSU 
           PERFORM COMPROC-CALL-SP2                                      *>FUJITSU 
007960     MOVE SP2-SET-RECORD TO CP-SP2-P1                              *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF PPSTVAT-PANEL-RECORD              *>FUJITSU 
           PERFORM COMPROC-CALL-SP2                                      *>FUJITSU        
                                                    
           MOVE LOW-VALUES TO SP2-WD-DATA.                                         
           MOVE "PPSM001" TO SP2-WD-PANEL-NAME.                              
           MOVE SP2-SET-RECORD TO CP-SP2-P1                       
           SET CP-SP2-P2 TO ADDRESS OF PPSM001-PANEL-RECORD        
           PERFORM COMPROC-CALL-SP2 
           MOVE SP2-OPEN-WINDOW TO CP-SP2-P1                      
           SET CP-SP2-P2 TO ADDRESS OF SP2-WINDOW-DEF             
           PERFORM COMPROC-CALL-SP2                       .       
008030     MOVE LOW-VALUES TO PPSM001-DATA.                                        
008050     MOVE "PPSM001"  TO PPSM001-NEXT-PANEL.                                  
008060     MOVE LOW-VALUES TO PPSM001-FIELDS.                                      
008070     MOVE LOW-VALUES TO PPSM001-COLRS.                                       
008080     MOVE LOW-VALUES TO PPSM001-TYPES.                                       
008080     MOVE "r"        TO PPSM001-MOUSE-SW.                                    
008090*    MOVE "N"        TO PPSM001-EXIT.                       
           INITIALIZE WS-JBS0052-EXIT
           SET JBS0052-OPEN TO TRUE
              
           
008540     MOVE "n" to PPSM001-WAIT-SW                                             
008550     MOVE SP2-CONVERSE-PANEL TO CP-SP2-P1                          *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF PPSM001-CONVERSE-DATA             *>FUJITSU 
           PERFORM COMPROC-CALL-SP2                                      *>FUJITSU 
008560     MOVE LOW-VALUES TO PPSM001-WAIT-SW.                  
           MOVE "PPSTOPT" TO SP2-ND-NAME
      *    MOVE HOOK01-SCREEN TO SP2-ND-NAME                     
           MOVE SP2-ACTIVATE-INTERNAL TO CP-SP2-P1                
           SET CP-SP2-P2 TO ADDRESS OF SP2-NAME-DEF               
           PERFORM COMPROC-CALL-SP2                                            
000110*    INITIALIZE WS-SCR001-FIELDS                                   *>CC110411
000120*    SET WS-SCR001-GET-WINDOW-CONFIG TO TRUE.                      *>CC110411
000130*    SET WS-SCR001-SUPPRESS-RESET TO TRUE.                         *>CC110411
000140*    MOVE CP-CMPNY                TO WS-SCR001-COMPANY             *>FJ131203
000150*    MOVE PROGRAM-NAME-77         TO WS-SCR001-PROGRAM             *>CC110411
000160*    MOVE "PPSM001"               TO WS-SCR001-SCREEN              *>CC110411
000170*    MOVE WS-PPSM001-SCR-ID       TO WS-SCR001-XLATE-ID            *>CC110411
000180*    EVALUATE TRUE                                                 *>CC110411
000190*       WHEN ADD-REQ                                               *>CC110411
000200*          MOVE WS-SCR-ID-ADD    TO WS-SCR001-PARAM-1              *>CC110411
000201*       WHEN UPDATE-REQ                                            *>CC110411
000202*          MOVE WS-SCR-ID-UPDATE TO WS-SCR001-PARAM-1              *>CC110411
000203*       WHEN DELETE-REQ                                            *>CC110411
000204*          MOVE WS-SCR-ID-DELETE TO WS-SCR001-PARAM-1              *>CC110411
000205*       WHEN OTHER                                                 *>CC110411
000206*          MOVE WS-SCR-ID-VIEW   TO WS-SCR001-PARAM-1              *>CC110411
000207*    END-EVALUATE.                                                 *>CC110411
000208*    PERFORM CALL-SCR001.                                          *>CC110411              
            
000310* SET FOCUS TO PPSTOPT                                                       
008560     MOVE "PPSTOPT" TO SP2-ND-NAME.                                          
008550     MOVE SP2-ACTIVATE-WINDOW TO CP-SP2-P1                         *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF SP2-NAME-DEF                      *>FUJITSU 
           PERFORM COMPROC-CALL-SP2                         .            *>FUJITSU            

           
              
           SET IS-NUMERIC TO TRUE

      *    SET IS-TEXT TO TRUE
           MOVE PPSTOPT-CL-DN-ARROW-I TO FLD-ID
           PERFORM FORMAT-TEXT
           
           SET IS-DATE TO TRUE
           PERFORM FORMAT-TEXT
           
           SET IS-TEXT TO TRUE
           PERFORM FORMAT-TEXT           
           .
           
       ADD-BOTTOM-LABEL.                               
           MOVE LOW-VALUES TO SP2-FD-DATA SP2-FD-VAR-LENS                            
           MOVE "FUJ-LABEL" TO SP2-FD-NAME                             
           COMPUTE SP2-FD-ROW = 387
           COMPUTE SP2-FD-COL = 0
           MOVE 120 TO SP2-FD-HEIGHT                           
           COMPUTE SP2-FD-WIDTH = 6000
           MOVE "e" TO SP2-FD-CTRL-TYPE    
           MOVE 9 TO SP2-FD-FONT-ID           
           MOVE "c" TO SP2-FD-JUSTIFY                          
           MOVE "y" TO SP2-FD-OUTPUT                             
           MOVE X"0A" TO SP2-FD-ANCHOR
           MOVE 5 TO SP2-FD-FORMAT-LEN
           MOVE "X(20).NET - Not Optimized"  
               TO SP2-FD-VAR-DATA            
           MOVE 20 TO SP2-FD-INITIAL-LEN     
                       SP2-FD-MAX-LEN        
           MOVE 33 TO SP2-FD-VAR-LEN 
           MOVE X"02" TO SP2-FD-PROG-SPEC                        
           MOVE 0 TO SP2-FD-PROG-LEN                         
           MOVE 8 TO SP2-FD-USER-LEN                         
           MOVE SP2-SET-FIELD-DEF TO CP-SP2-P1      
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF
           PERFORM COMPROC-CALL-SP2
           .
       
       JBS0052-CONVERSE.     
                                         
           MOVE SP2-CONVERSE-PANEL TO CP-SP2-P1                   
           SET CP-SP2-P2 TO ADDRESS OF PPSM001-CONVERSE-DATA       
           PERFORM COMPROC-CALL-SP2
           . 
       JBS0052-EVENT-HANDLER.
           IF PPSM001-KEY = SP2-KEY-ESCAPE OR                                       
              PPSM001-KEY = SP2-KEY-CLOSE
               SET JBS0052-EXIT TO TRUE
               EXIT PARAGRAPH
           END-IF 
           
                   
      *    MOVE LOW-VALUES TO SP2-MS-DATA.                                         
      *    MOVE 2 TO SP2-MS-LINE-CNT.                                              
      *    MOVE "s"   TO SP2-MS-ICON.                                              
      *    MOVE "o"   TO SP2-MS-BUTTON.                                            
      *    MOVE LOW-VALUES TO SP2-MS-CANCEL.                                       
      *    MOVE "SP2 Key Received" TO SP2-MS-TITLE.  
      *    MOVE JBS0052-KEY TO DISPLAY-NUM                                
      *    MOVE DISPLAY-NUM TO SP2-MS-TEXT.                                         
      *    MOVE SP2-DISPLAY-MESSAGE TO CP-SP2-P1                         *>FUJITSU 
      *    SET CP-SP2-P2 TO ADDRESS OF SP2-MESSAGE-DATA                  *>FUJITSU 
      *    PERFORM COMPROC-CALL-SP2                             .        *>FUJITSU 
           
      *    IF JBS0052-SAVE-BTN-HIT
      *        SET formatString to cString::"Concat"("Due Date: ", JBS0052-DUE-DATE)
      *        INVOKE cMsgBox "Show" USING formatString
      *    END-IF
           .    
       SCR110-DATE-FORMAT.
           PERFORM CREATE-NET-DATE 
           PERFORM SET-DATE-TIME-ADVANCING-CARET
           PERFORM SET-RIGHT-JUSTIFICATION
           PERFORM SET-FIELD-PROTECTION
           PERFORM SET-FIELD-TYPE
      *    PERFORM SET-SPECIAL-FORMAT                                    *>SK#53051
           
           
       FORMAT-TEXT.
           PERFORM COMMON-TEXT-FORMATTING
           .
         
       SCR010-FLIP-DAYS.
           SET FLIP-DAYS TO TRUE
      *    INITIALIZE FLIP-DAYS-FLAG
           .       
           
           
      ******************************************************************           
       SET-SIZE-WITH-SET-FIELD.
      ******************************************************************
           MOVE 4000 TO SP2-FD-VAR-LEN
           MOVE LOW-VALUES TO SP2-FD-DATA
                              SP2-FD-VAR-LENS
           MOVE FLD-ID TO SP2-FD-ID
           MOVE SP2-GET-FIELD-DEF TO CP-SP2-P1
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF
           PERFORM COMPROC-CALL-SP2
           MOVE NEW-LENGTH TO SP2-FD-MAX-LEN
           MOVE NEW-LENGTH TO SP2-FD-ITEM-LEN
           MOVE NEW-LENGTH TO SP2-FD-PROG-LEN
           MOVE SP2-SET-FIELD-DEF TO CP-SP2-P1       
      *    MOVE X"04" TO SP2-FD-MISC-OPTIONS                             *>EB#56255
      *    MOVE X"20" TO SP2-FD-OPTIONS-5                                *>MJ#56923
      *    MOVE X"04" TO SP2-FD-MORE-OPTIONS                             *>MJ#59406
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF 
           PERFORM COMPROC-CALL-SP2 
           .
      ******************************************************************           
       COMMON-TEXT-FORMATTING.
      ******************************************************************
           PERFORM SCR010-FLIP-DAYS.

      ******************************************************************
      **   Build "Initial" Var Data String.
      ******************************************************************
           INITIALIZE WS-EDIT-OBJ-NAME
           MOVE 1 TO WS-OBJ-NAME-LEN                                                    
           IF IS-NUMERIC
               STRING                                                                    
                   "        "                                                             
                   "TN  .net    "                                                       
      *****************************************************************
      **   Custom Subclassed TextEdit control with Basic AutoComplete.
      *****************************************************************
                   "GSSERP.Support.GSTextEdit"
                   "/assembly=GSSERP.Support/ForeColor=$colorf/BackColor=$colorb" 
                   DELIMITED BY SIZE                                                   
                   INTO WS-EDIT-OBJ-NAME                                           
                   WITH POINTER WS-OBJ-NAME-LEN                                             
               END-STRING                                                              
           ELSE
               STRING                                                                    
                   "        "                                                             
                   "TA  .net    "                                                       
      ******************************************************************
      **   Custom Subclassed TextEdit control with Basic AutoComplete.
      ******************************************************************
                   "GSSERP.Support.GSTextEdit"
                   "/assembly=GSSERP.Support/MaxLength=$propFN-0002600002"
                   "/ForeColor=$colorf/BackColor=$colorb"  *>MJ#59406
                   DELIMITED BY SIZE                                                   
                   INTO WS-EDIT-OBJ-NAME                                           
                   WITH POINTER WS-OBJ-NAME-LEN                                             
               END-STRING
           END-IF 
      ******************************************************************
      **  Get the field definition
      ******************************************************************
           MOVE LOW-VALUES TO SP2-FD-DATA
                              SP2-FD-VAR-DATA 
           MOVE LOW-VALUES TO SP2-FD-VAR-LENS
           MOVE 4000 TO SP2-FD-VAR-LEN      
           MOVE FLD-ID TO SP2-FD-ID           
           MOVE SP2-GET-FIELD-DEF TO CP-SP2-P1       
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF  
           PERFORM COMPROC-CALL-SP2
      *****************************************************************  *>MJ#55746
      **   If a non-numeric entry has Z or 9 instead of X, change it.    *>MJ#55746
      *****************************************************************  *>MJ#55746
           IF (SP2-FD-VAR-DATA(1:1) = 'Z' OR SP2-FD-VAR-DATA(1:1) = 'z'  *>MJ#55746
             OR SP2-FD-VAR-DATA(1:1) = '9')                              *>MJ#55746
             AND NOT IS-DATE AND NOT IS-NUMERIC
               MOVE 'X' TO SP2-FD-VAR-DATA(1:1)                          *>MJ#55746
           END-IF                                                        *>MJ#55746
      ******************************************************************
      **   Get needed data out of field def before deleting field.
      ******************************************************************
           MOVE SP2-FD-ID TO WS-HOLD-FD-ID
           MOVE SP2-FD-OUTPUT TO WS-HOLD-FD-PROTECTION
           MOVE SP2-FD-CURS-SKIP TO WS-HOLD-USAGE-OPTION
           MOVE SP2-FD-TYPE TO WS-HOLD-FD-TYPE
           MOVE SP2-FD-USER-LEN TO WS-HOLD-USER-LEN         
           MOVE SP2-FD-FORMAT-LEN TO WS-HOLD-FORMAT-LEN
           IF WS-HOLD-FORMAT-LEN > ZERO
               MOVE SP2-FD-VAR-DATA(1 : WS-HOLD-FORMAT-LEN) 
               TO WS-HOLD-EDIT-MASK  
           END-IF
      ******************************************************************
      **   Get user data.
      ******************************************************************
           PERFORM GET-USER-DATA
           MOVE SP2-PR-VALUE TO WS-FD-USER-DATA-ALL
           COMPUTE WS-FD-OTHER-VAR-DATA-LEN = SP2-FD-CLASS-LEN + SP2-FD-RANGE-LEN + SP2-FD-DISCRETE-LEN + SP2-FD-MSG-TEXT-LEN  *>EB#59123
           IF WS-FD-OTHER-VAR-DATA-LEN > 0                               *>EB#59123
               MOVE SP2-FD-VAR-DATA(SP2-FD-FORMAT-LEN + SP2-FD-CAPTION-LEN + SP2-FD-INITIAL-LEN + 1:  *>EB#59123
                    WS-FD-OTHER-VAR-DATA-LEN)                            *>EB#59123
                 TO WS-FD-OTHER-VAR-DATA                                 *>EB#59123
           END-IF                                                        *>EB#59123
      ******************************************************************
      **   If the word "DATE" is found in user data, don't do any more 
      **   processing on this field as it is an SCR110 date and it will
      **   get handled by SCR110 later on.
      ******************************************************************
      *    INITIALIZE WS-DATE-TAG-FLAG
      *    PERFORM VARYING WS-STR-CTR FROM 1 BY 1       
      *        UNTIL WS-STR-CTR >= 253                  
      *        IF WS-FD-USER-DATA-ALL(WS-STR-CTR:4) = "DATE" 
      *            SET WS-DATE-TAG-FOUND TO TRUE        
      *        END-IF                                   
      ***** SETS THE FLAG FOR THE AUTOTABBING PROPERTY ON GSTEXTEDIT **** *>MJ#56255
      *        IF NOT WS-STR-CTR >= 250                                      *>MJ#56255
      *            IF WS-FD-USER-DATA-ALL(WS-STR-CTR:7) = "AUTOTAB"          *>MJ#56255
      *                SET WS-AUTO-TAB-ENABLED TO TRUE                       *>MJ#56255
      *            END-IF                                                    *>MJ#56255
      *        END-IF                                                        *>MJ#56255
      ******************************************************************* *>MJ#56255
      *    END-PERFORM                                    
      *    IF WS-DATE-TAG-FOUND
      *        MOVE SP2-SET-FIELD-DEF TO CP-SP2-P1       
      *        SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF  
      *        PERFORM COMPROC-CALL-SP2
      *        EXIT PARAGRAPH
      *    END-IF
      ******************************************************************
      **   If this is a legacy date field, it does not use SCR110 nor 
      **   SCR003 for any date logic. Handle it here.
      ******************************************************************
           IF IS-DATE
               PERFORM UPDATE-LEGACY-DATE
               EXIT PARAGRAPH            
           END-IF                        
      ******************************************************************
      **   Check current value, then check for initial value and compare
      **     - If current = initial or low values, set initial
      **     - Otherwise, set current
      ******************************************************************
           MOVE SPACES TO WS-HOLD-INITIAL-VALUE      
           IF SP2-FD-INITIAL-LEN = 0                 
               IF IS-NUMERIC AND SP2-FD-PROG-LEN > 0 
               MOVE WS-WORK-ZEROES(1:SP2-FD-PROG-LEN)
                 TO WS-HOLD-INITIAL-VALUE            
                 MOVE LOW-VALUES TO WS-HOLD-INITIAL-VALUE(SP2-FD-PROG-LEN + 1:1)
               ELSE                                         
                   MOVE LOW-VALUES TO WS-HOLD-INITIAL-VALUE 
               END-IF                                       
           ELSE                                             
               PERFORM GET-INITIAL-VALUE                    
               MOVE SP2-PR-VALUE(1:SP2-FD-INITIAL-LEN)      
                 TO WS-HOLD-INITIAL-VALUE                   
               IF SP2-FD-INITIAL-LEN < 30                   
                   MOVE LOW-VALUES TO WS-HOLD-INITIAL-VALUE(SP2-FD-INITIAL-LEN + 1:1)
               END-IF                                                    
           END-IF                                                        
           STRING                                                        
               "/init-val="          DELIMITED BY SIZE                   
               WS-HOLD-INITIAL-VALUE DELIMITED BY LOW-VALUES             
               INTO WS-EDIT-OBJ-NAME                                     
               WITH POINTER WS-OBJ-NAME-LEN                              
           END-STRING                                                    
      ******************************************************************
      **   Delete original SP2 field
      ******************************************************************
           MOVE SP2-DELETE-FIELD TO CP-SP2-P1        
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF                     
           PERFORM COMPROC-CALL-SP2
      ******************************************************************
      **   Dev Express Phase 2. Multi Line Edit fields.
      ******************************************************************
      *    IF WS-HOLD-USAGE-MLE
      *        PERFORM CREATE-MLE-CONTROL                                     
      *        EXIT PARAGRAPH
      *    ELSE
      ******************************************************************
      **   Apparently field height can be zero and a default value will
      **   will be applied by SP2...but not .NET controls. So if height
      **   is zero, assign a default field height here.
      ******************************************************************
           IF SP2-FD-HEIGHT = ZERO                                  
               MOVE 170 TO SP2-FD-HEIGHT
           END-IF        
      ******************************************************************
      **   Set required properties to make new field work.
      ******************************************************************
           MOVE "i" TO SP2-FD-CTRL-TYPE
           MOVE X"05" TO SP2-FD-TYPE
           MOVE X"C0" TO SP2-FD-OPTIONS-4
           MOVE WS-HOLD-EDIT-MASK TO SP2-FD-VAR-DATA
           MOVE WS-HOLD-FORMAT-LEN TO SP2-FD-FORMAT-LEN
           IF IS-NUMERIC AND WS-HOLD-FORMAT-LEN > ZERO 
               MOVE SP2-FD-VAR-DATA(1 : WS-HOLD-FORMAT-LEN)              *>MJ#56255
                 TO WS-HOLD-EDIT-MASK                                    *>MJ#56255
               IF WS-HOLD-EDIT-MASK (WS-HOLD-FORMAT-LEN : 1) = "-"       *>MJ#56255
                   MOVE WS-HOLD-EDIT-MASK (1 : WS-HOLD-FORMAT-LEN - 1)   *>MJ#56255
                     TO SP2-FD-VAR-DATA (2 : WS-HOLD-FORMAT-LEN - 1)     *>MJ#56255
                   MOVE "-" TO SP2-FD-VAR-DATA (1 : 1)                   *>MJ#56255
               END-IF                                                    *>MJ#56255
           END-IF                                                        *>MJ#56255
           MOVE "Text" TO SP2-FD-VAR-DATA (SP2-FD-FORMAT-LEN + 1 :)
           MOVE 4 TO SP2-FD-CAPTION-LEN
           MOVE WS-OBJ-NAME-LEN TO SP2-FD-INITIAL-LEN
           MOVE WS-EDIT-OBJ-NAME TO SP2-FD-VAR-DATA (SP2-FD-FORMAT-LEN
                                                     + SP2-FD-CAPTION-LEN + 1:)
           MOVE WS-HOLD-USER-LEN TO SP2-FD-USER-LEN
           IF WS-FD-OTHER-VAR-DATA-LEN > 0                               *>EB#59123
               MOVE WS-FD-OTHER-VAR-DATA TO SP2-FD-VAR-DATA(SP2-FD-FORMAT-LEN  *>EB#59123
                   + SP2-FD-CAPTION-LEN + SP2-FD-INITIAL-LEN + 1 : WS-FD-OTHER-VAR-DATA-LEN)  *>EB#59123
           END-IF                                                        *>EB#59123
           IF SP2-FD-USER-LEN > 1                                        *>SK#50890
               MOVE WS-FD-USER-DATA-ALL(1:SP2-FD-USER-LEN)               *>SK#50890
                   TO sp2-fd-var-data (SP2-FD-FORMAT-LEN                 *>SK#50890
                   + SP2-FD-CAPTION-LEN + SP2-FD-INITIAL-LEN + WS-FD-OTHER-VAR-DATA-LEN + 1:)        *>EB#59123
           END-IF                                                        *>SK#50890
           COMPUTE SP2-FD-VAR-LEN
               = SP2-FD-FORMAT-LEN + SP2-FD-CAPTION-LEN
               + SP2-FD-INITIAL-LEN + SP2-FD-CLASS-LEN                   *>EB#59123
               + SP2-FD-RANGE-LEN + SP2-FD-DISCRETE-LEN                  *>EB#59123
               + SP2-FD-MSG-TEXT-LEN + SP2-FD-USER-LEN                   *>EB#59123
               + SP2-FD-HELP-LEN                                         *>EB#59123
           MOVE SP2-FD-MAX-LEN TO SP2-FD-ITEM-LEN
           MOVE X"04" TO SP2-FD-MISC-OPTIONS                             *>EB#56255
           MOVE X"20" TO SP2-FD-OPTIONS-5                                *>MJ#56923
           MOVE X"04" TO SP2-FD-MORE-OPTIONS                             *>MJ#59406
      *    MOVE WS-MENU-FONT-ID TO SP2-FD-FONT-ID                        *>themecode
      ******************************************************************
      **   Create new control!
      ******************************************************************
           MOVE SP2-SET-FIELD-DEF TO CP-SP2-P1        
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF  
           PERFORM COMPROC-CALL-SP2
      ****************************************************************** 
      **   Get Properties Property for TextEdit Object at FC-ID(FC-IND)      
      ****************************************************************** 
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE FLD-ID TO SP2-NE-ID
           MOVE "Properties" TO SP2-NE-PROP-NAME
           move
           "DevExpress.XtraEditors.Repository.RepositoryItemTextEdit"
               to sp2-ne-value-type
           move 32 to sp2-ne-options  
           MOVE SP2-GET-NET TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM             
           PERFORM COMPROC-CALL-SP2                                     
           
           IF SP2-NE-RET-CODE NOT = ZERO
               MOVE SP2-SET-FIELD-DEF TO CP-SP2-P1        
               SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF  
               PERFORM COMPROC-CALL-SP2
               
               MOVE "Properties" TO SP2-NE-PROP-NAME
               MOVE SP2-GET-NET TO CP-SP2-P1                        
               SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM             
               PERFORM COMPROC-CALL-SP2        
               IF SP2-NE-RET-CODE NOT = ZERO
                   EXIT PARAGRAPH
               END-IF
           END-IF
           MOVE SP2-NE-VALUE TO WS-RepositoryItemTextEdit-PTR
      ******************************************************************     
      **   Get appearance subproperty from Properties subproperty.
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE FLD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "Appearance" TO SP2-NE-PROP-NAME
           MOVE WS-RepositoryItemTextEdit-PTR TO SP2-NE-OBJECT-PTR
           MOVE SP2-GET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2                                 
           IF SP2-NE-RET-CODE = ZERO
               MOVE SP2-NE-VALUE TO WS-TEXTPROPS-APPEARANCE-PTR
           END-IF  
      ******************************************************************     
      **   Get Options subproperty from Appearance subproperty.
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE FLD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "Options" TO SP2-NE-PROP-NAME
           MOVE WS-TEXTPROPS-APPEARANCE-PTR TO SP2-NE-OBJECT-PTR
           MOVE SP2-GET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2                                 
           IF SP2-NE-RET-CODE = ZERO 
               MOVE SP2-NE-VALUE TO WS-TEXTPROPS-APRNC-OPT-PTR
           END-IF  
      ******************************************************************     
      **   Get TextOptions subproperty from Options subproperty.
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE FLD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "TextOptions" TO SP2-NE-PROP-NAME
           MOVE WS-TEXTPROPS-APPEARANCE-PTR TO SP2-NE-OBJECT-PTR
           MOVE SP2-GET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2                                 
           IF SP2-NE-RET-CODE = ZERO
               MOVE SP2-NE-VALUE TO WS-TEXTPROPS-APRNC-TXTOPT-PTR
           END-IF  
      ****************************************************************** 
      **   When field type is numeric, set a numeric mask based on 
      **   mask in user data. Otherwise, no mask is needed, but we need
      **   to set the MaxLength property.
      **   Here, MaxLength is a subproperty of the Properties Property.
      **   The Properties Property properties are referenced by the 
      **   RepositoryItemTextEdit object. Clear as mud.
      ****************************************************************** 
           IF IS-NUMERIC
                   PERFORM APPLY-NUMERIC-MASK
                   IF NOT WS-WORK-STRING(1:1) = "X" AND                  *>MJ#54568
                      NOT WS-WORK-STRING(1:1) = "x"                      *>MJ#54568
                       PERFORM SET-RIGHT-JUSTIFICATION
                   END-IF                                                *>MJ#54568
                   PERFORM SET-FIELD-TYPE                                *>SM#23002
      *    ELSE
      *            MOVE LOW-VALUES TO SP2-NE-DATA
      *            MOVE FLD-ID TO SP2-NE-ID
      *            MOVE 1 TO SP2-NE-OPTIONS
      *            MOVE WS-RepositoryItemTextEdit-PTR TO SP2-NE-OBJECT-PTR
      *            MOVE "MaxLength" TO SP2-NE-PROP-NAME
      *            move SP2-FD-MAX-LEN to sp2-ne-value
      *            MOVE SP2-SET-NET TO CP-SP2-P1                    
      *            SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
      *            PERFORM COMPROC-CALL-SP2                                 
           END-IF
           PERFORM SET-FIELD-PROTECTION                                  *>SK#53051
      ****************************************************************** *>MJ#55656
      **   Enforce upper/ lower casing on the textedit.                  *>MJ#55656
      ****************************************************************** *>MJ#55656
           IF SP2-FD-CASE = 'l' OR SP2-FD-CASE = 'u'                     *>MJ#55656
               MOVE LOW-VALUES TO SP2-NE-DATA                            *>MJ#55656
               MOVE SP2-FD-ID TO SP2-NE-ID                               *>MJ#55656
               MOVE 1 TO SP2-NE-OPTIONS                                  *>MJ#55656
               MOVE "CharacterCasing" TO SP2-NE-PROP-NAME                *>MJ#55656
               MOVE WS-RepositoryItemTextEdit-PTR TO SP2-NE-OBJECT-PTR   *>MJ#55656
               IF SP2-FD-CASE = 'l'                                      *>MJ#55656
                   MOVE "CharacterCasing.Lower" to SP2-NE-VALUE          *>MJ#55656
               END-IF                                                    *>MJ#55656
               IF SP2-FD-CASE = 'u'                                      *>MJ#55656
                   MOVE "CharacterCasing.Upper" to SP2-NE-VALUE          *>MJ#55656
               END-IF                                                    *>MJ#55656
               MOVE SP2-SET-NET TO CP-SP2-P1                             *>MJ#55656
               SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                  *>MJ#55656
               PERFORM COMPROC-CALL-SP2                                  *>MJ#55656
           END-IF                                                        *>MJ#55656
           
           .
           
           
           
           
      ****************************************************************** 
       APPLY-NUMERIC-MASK.
      ****************************************************************** 
      **   Get Format String for current field. We cannot rely on cached
      **   var data in FC table because it has likely been overwritten  
      **   in SCR003 when applying options and numeric format tags.
      ****************************************************************** 
      ******************************************************************           
      **   WS-HOLD-EDIT-MASK Replace all Z by # and all 9 by 0.
      **   Don't build sign in manually as it causes problems.  
      **   Need to set one mask for editing and one mask for display
      **   purposes.
      ******************************************************************           
           SET NEG-FALSE TO TRUE
           PERFORM EXPAND-PIC-CLAUSE
           MOVE WS-WORK-STRING TO WS-HOLD-EDIT-MASK 
                                  WS-HOLD-DISP-FMT
           
           PERFORM VARYING WS-LOOP-CTR FROM 1 BY 1               
               UNTIL WS-LOOP-CTR > 100
               OR WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) = SPACE                            
               IF WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) = 'Z'
                   OR WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) = 'z'           
                   MOVE '#' TO WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) 
               END-IF                                           
               IF WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) = '9' OR
                  WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) = 'X' OR              *>MJ#54568
                  WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) = 'x'                 *>MJ#54568
                   MOVE '0' TO WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) 
               END-IF        
               IF WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) = '-'  OR
                  WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) = 'S'
                   IF WS-LOOP-CTR = 1
                       SET LEADING-NEG TO TRUE
                   ELSE
                       SET NEG TO TRUE
                   END-IF
               END-IF                                           
           END-PERFORM     
             
      ******************************************************************           
      **   DevExpress TextEdit controls use a separate edit mask for 
      **   positive values and negative values. If a negative is found
      **   in the edit mask, we need to duplicate the edit mask, first
      **   positive then negative separated by semicolon.
      ******************************************************************           
           IF NEG OR LEADING-NEG
                MOVE WS-HOLD-EDIT-MASK TO WS-HOLD-POS-MASK
                                          WS-HOLD-NEG-MASK
                IF LEADING-NEG
                    MOVE WS-HOLD-POS-MASK(2:49) TO WS-HOLD-POS-MASK(1:49)
                    MOVE SPACE TO WS-HOLD-POS-MASK(50:1)
                ELSE
                    PERFORM VARYING WS-LOOP-CTR FROM 50 BY -1               
                        UNTIL WS-LOOP-CTR = ZERO
                        IF WS-HOLD-POS-MASK(WS-LOOP-CTR:1) = '-'          
                            MOVE SPACE TO WS-HOLD-POS-MASK(WS-LOOP-CTR:1)
                        END-IF                                           
                     END-PERFORM     
                END-IF                          
                PERFORM VARYING WS-POS-STR-CTR FROM 50 BY -1               
                    UNTIL WS-HOLD-POS-MASK(WS-POS-STR-CTR:1) NOT = SPACE                            
                    CONTINUE
                END-PERFORM     
                PERFORM VARYING WS-NEG-STR-CTR FROM 50 BY -1               
                    UNTIL WS-HOLD-NEG-MASK(WS-NEG-STR-CTR:1) NOT = SPACE                            
                    CONTINUE
                END-PERFORM
                STRING
                   WS-HOLD-POS-MASK(1:WS-POS-STR-CTR)
                   ';'
                   WS-HOLD-NEG-MASK(1:WS-NEG-STR-CTR)
                   DELIMITED BY SIZE
                   INTO WS-HOLD-EDIT-MASK
                END-STRING
           END-IF
      ****************************************************************** 
      **   Set AllowNullInput SubProperty of Properties Property.
      ****************************************************************** 
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE SP2-FD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE WS-RepositoryItemTextEdit-PTR TO SP2-NE-OBJECT-PTR
           MOVE "AllowNullInput" TO SP2-NE-PROP-NAME
           move "1" to SP2-NE-VALUE
           MOVE SP2-SET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2                                 
                                          
           IF SP2-NE-RET-CODE NOT = ZERO
               EXIT PARAGRAPH
           END-IF
      ****************************************************************** 
      **   Get Mask SubProperty from Properties Property.
      ****************************************************************** 
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE SP2-FD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "Mask" TO SP2-NE-PROP-NAME
           MOVE WS-RepositoryItemTextEdit-PTR TO SP2-NE-OBJECT-PTR
           MOVE SP2-GET-NET TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM             
           PERFORM COMPROC-CALL-SP2                                     

           IF SP2-NE-RET-CODE NOT = ZERO
               EXIT PARAGRAPH
           END-IF

           MOVE SP2-NE-VALUE TO WS-Mask-PTR
        
      ****************************************************************** 
      **   Set Mask Type and EditMask sub properties from Mask Property.
      ****************************************************************** 
           IF SP2-NE-RET-CODE = ZERO
      ****************************************************************** 
      **   Set MaskType to Numeric.
      ****************************************************************** 
               MOVE LOW-VALUES TO SP2-NE-DATA
               MOVE SP2-FD-ID TO SP2-NE-ID
               MOVE 1 TO SP2-NE-OPTIONS
               MOVE WS-Mask-PTR TO SP2-NE-OBJECT-PTR
               MOVE "MaskType" TO SP2-NE-PROP-NAME
               move
               "DevExpress.XtraEditors.TextEdit.Mask.MaskType.Numeric"
                   to sp2-ne-value
               MOVE SP2-SET-NET TO CP-SP2-P1                    
               SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
               PERFORM COMPROC-CALL-SP2                                 
      ****************************************************************** 
      **   Set EditMask to WS-HOLD-EDIT-MASK from above
      ****************************************************************** 
               MOVE LOW-VALUES TO SP2-NE-DATA
               MOVE SP2-FD-ID TO SP2-NE-ID
               MOVE 1 TO SP2-NE-OPTIONS
               MOVE WS-Mask-PTR TO SP2-NE-OBJECT-PTR
               MOVE "EditMask" TO SP2-NE-PROP-NAME
               move WS-HOLD-EDIT-MASK to sp2-ne-value
               MOVE SP2-SET-NET TO CP-SP2-P1                    
               SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
               PERFORM COMPROC-CALL-SP2                                 
           END-IF
           .
           
      ******************************************************************           
      **   The display format is very similar to the edit format,
      **   but with all 0's replaced by #
      ******************************************************************          
           MOVE WS-HOLD-EDIT-MASK TO WS-HOLD-DISP-FMT                    *>EB#54404
           PERFORM VARYING WS-LOOP-CTR FROM 1 BY 1                       *>EB#54404
               UNTIL WS-LOOP-CTR > 100                                   *>EB#54404
               OR WS-HOLD-DISP-FMT(WS-LOOP-CTR:1) = SPACE                *>EB#54404
               IF WS-HOLD-DISP-FMT(WS-LOOP-CTR:1) = '0'                  *>EB#54404
                   MOVE '#' TO WS-HOLD-DISP-FMT(WS-LOOP-CTR:1)           *>EB#54404
               END-IF                                                    *>EB#54404
           END-PERFORM                                                   *>EB#54404

      ****************************************************************** 
      **   Get DisplayFormat SubProperty from Properties Property.
      ****************************************************************** 
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE SP2-FD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "DisplayFormat" TO SP2-NE-PROP-NAME
           MOVE WS-RepositoryItemTextEdit-PTR TO SP2-NE-OBJECT-PTR
           MOVE SP2-GET-NET TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM             
           PERFORM COMPROC-CALL-SP2                                     
       
           IF SP2-NE-RET-CODE NOT = ZERO
               EXIT PARAGRAPH
           END-IF
       
           MOVE SP2-NE-VALUE TO WS-DisplayFormat-PTR
        
      ****************************************************************** 
      **   Set FormatType and FormatString sub properties from 
      **   DisplayFormat Property.
      ****************************************************************** 
           IF SP2-NE-RET-CODE = ZERO
      ****************************************************************** 
      **   Set MaskType to Numeric.
      ****************************************************************** 
               MOVE LOW-VALUES TO SP2-NE-DATA
               MOVE SP2-FD-ID TO SP2-NE-ID
               MOVE 1 TO SP2-NE-OPTIONS
               MOVE WS-DisplayFormat-PTR TO SP2-NE-OBJECT-PTR
               MOVE "FormatType" TO SP2-NE-PROP-NAME
               string
                   "DevExpress.XtraEditors.TextEdit." DELIMITED BY SIZE
                   "DisplayFormat.FormatType.Numeric" DELIMITED BY SIZE
                   INTO SP2-NE-VALUE
               end-string
               MOVE SP2-SET-NET TO CP-SP2-P1                    
               SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
               PERFORM COMPROC-CALL-SP2                                 
      ****************************************************************** 
      **   Set EditMask to WS-STRING(1) from above
      ****************************************************************** 
               MOVE LOW-VALUES TO SP2-NE-DATA
               MOVE SP2-FD-ID TO SP2-NE-ID
               MOVE 1 TO SP2-NE-OPTIONS
               MOVE WS-DisplayFormat-PTR TO SP2-NE-OBJECT-PTR  
               MOVE "FormatString" TO SP2-NE-PROP-NAME
               move WS-HOLD-DISP-FMT to sp2-ne-value
               MOVE SP2-SET-NET TO CP-SP2-P1             
               SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
               PERFORM COMPROC-CALL-SP2                                 
           END-IF
           .
      
                                                                             
      ******************************************************************           
       GET-INITIAL-VALUE.                                                          
      ******************************************************************           
           MOVE LOW-VALUES             TO SP2-PR-DATA                              
           MOVE SP2-FD-ID              TO SP2-PR-ID                                
           SET  SP2-PR-FIELD           TO TRUE                                     
           SET  SP2-PR-VAR-T           TO TRUE                                     
           SET  SP2-PR-VAR-3           TO TRUE                                     
           MOVE ZERO                   TO SP2-PR-OFFSET                            
           MOVE 100                     TO SP2-PR-LEN                               
           MOVE SP2-GET-PROPERTY TO CP-SP2-P1                            *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF SP2-PROPERTY                      *>FUJITSU 
           PERFORM COMPROC-CALL-SP2                      .               *>FUJITSU 
                                                                                                        
      ******************************************************************           
       GET-USER-DATA.                                                    *>EB100401
      ******************************************************************           
           MOVE LOW-VALUES             TO SP2-PR-DATA                              
           MOVE SP2-FD-ID              TO SP2-PR-ID                                
           SET SP2-PR-FIELD            TO TRUE                                     
           SET SP2-PR-VAR-T            TO TRUE                                     
           SET SP2-PR-VAR-8            TO TRUE                                     
           MOVE 2000                   TO SP2-PR-LEN                               
           MOVE SP2-GET-PROPERTY TO CP-SP2-P1                            *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF SP2-PROPERTY                      *>FUJITSU 
           PERFORM COMPROC-CALL-SP2
           MOVE SP2-PR-VALUE TO WS-FD-USER-DATA 
           .
            
      ****************************************************************** 
       UPDATE-CHECKBOX. 
      ******************************************************************
      **  Get the field definition
      ******************************************************************
           MOVE LOW-VALUES TO SP2-FD-DATA
                              SP2-FD-VAR-DATA 
           MOVE LOW-VALUES TO SP2-FD-VAR-LENS
           MOVE FLD-ID TO SP2-FD-ID
           MOVE 4000 TO SP2-FD-VAR-LEN          
           MOVE SP2-GET-FIELD-DEF TO CP-SP2-P1       
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF  
           PERFORM COMPROC-CALL-SP2   
      ******************************************************************
      **   Determine if checkbox returns to program when selected.
      ******************************************************************           
           IF SP2-FD-PROG-CTRL = 's' OR SP2-FD-PROG-CTRL = 'o'           *>MJ#55459
               SET WS-SP2-KEY-SELECT-TRUE TO TRUE                        *>MJ#54568
           ELSE                                                          *>MJ#54568
               SET WS-SP2-KEY-SELECT-FALSE TO TRUE                       *>MJ#54568
           END-IF                                                        *>MJ#54568
      ****************************************************************** *>SK#50890
      **   Build "Initial" Var Data String.
      ******************************************************************
           INITIALIZE WS-EDIT-OBJ-NAME
           MOVE 1 TO WS-OBJ-NAME-LEN                                                 
           STRING                                                                    
               "        "                                                             
               "CK  .net    "                                                       
               "GSSERP.Support.GSCheckEdit"                                  
               "/assembly=GSSERP.Support"
               "/ForeColor=$colorf/BackColor=$colorb"
               WS-SP2-KEY-SELECT                                         *>MJ#54568
               DELIMITED BY SIZE                                                   
               INTO WS-EDIT-OBJ-NAME                                           
               WITH POINTER WS-OBJ-NAME-LEN                                             
           END-STRING                                                          
      ******************************************************************
      **   Get needed data out of field def before deleting field.
      ******************************************************************
           MOVE SP2-FD-ID TO WS-HOLD-FD-ID
           MOVE SP2-FD-OUTPUT TO WS-HOLD-FD-PROTECTION
           MOVE SP2-FD-TYPE TO WS-HOLD-FD-TYPE
           COMPUTE WS-HOLD-FD-WIDTH = SP2-FD-WIDTH + 10
           IF WS-HOLD-FD-WIDTH < 180
               MOVE 180 TO WS-HOLD-FD-WIDTH
           END-IF
           IF SP2-FD-HEIGHT > 169                                        *>SK#50890
               COMPUTE WS-HOLD-FD-HEIGHT = SP2-FD-HEIGHT + 10
           ELSE
               MOVE WS-HOLD-FD-WIDTH  TO WS-HOLD-FD-HEIGHT
           END-IF
           IF SP2-FD-CAPTION-LEN > ZERO
               MOVE SPACES TO WS-HOLD-CAPTION
               SET WS-CREATE-LABEL TO TRUE
               MOVE 180 TO WS-LBL-FD-HEIGHT
               COMPUTE WS-LBL-FD-WIDTH = WS-HOLD-FD-WIDTH - 320
               MOVE SP2-FD-ROW TO WS-LBL-FD-ROW   
               COMPUTE WS-LBL-FD-COL = SP2-FD-COL + 20
               MOVE SP2-FD-CAPTION-LEN TO WS-HOLD-CAPTION-LEN
               MOVE WS-HOLD-CAPTION-LEN TO WS-ALPHA-CAPTION-LEN
      *        IF SP2-FD-FORMAT-LEN = ZERO
               STRING
                   "X("   DELIMITED BY SIZE
                   WS-ALPHA-CAPTION-LEN DELIMITED BY SPACE
                   ")"    DELIMITED BY SIZE
                   SP2-FD-VAR-DATA(1 + SP2-FD-FORMAT-LEN : SP2-FD-CAPTION-LEN) DELIMITED BY SIZE
                   INTO WS-HOLD-CAPTION
               END-STRING
               MOVE 7 TO WS-HOLD-FORMAT-LEN
      *        ELSE
      *            MOVE SP2-FD-VAR-DATA(1: SP2-FD-FORMAT-LEN + SP2-FD-CAPTION-LEN + 1) TO WS-HOLD-CAPTION
      *            MOVE SP2-FD-FORMAT-LEN TO WS-HOLD-FORMAT-LEN
      *        END-IF
               STRING
                   'LBL-'      DELIMITED BY SIZE
                   SP2-FD-NAME DELIMITED BY SIZE
                   INTO 
                   WS-HOLD-FD-NAME
               END-STRING
      *        MOVE 160 TO WS-HOLD-FD-WIDTH
               COMPUTE WS-HOLD-FD-WIDTH = 320 + WS-LBL-FD-WIDTH
               MOVE 180 TO WS-HOLD-FD-HEIGHT
      *        PERFORM SET-CHECKBOX-CAPTION
           END-IF
           COMPUTE SP2-FD-ROW = SP2-FD-ROW - 2
           MOVE SP2-FD-VAR-DATA(SP2-FD-FORMAT-LEN            
           + SP2-FD-CAPTION-LEN + SP2-FD-INITIAL-LEN + 1:)   
           TO WS-HOLD-DISCRETE-VALUES                        
           IF WS-HOLD-DISCRETE-VALUES = SPACE
               MOVE 'YN' TO WS-HOLD-DISCRETE-VALUES
           END-IF
           IF WS-HOLD-DISCRETE-VALUES = '00'
               MOVE '10' TO WS-HOLD-DISCRETE-VALUES
           END-IF
           PERFORM GET-USER-DATA
           MOVE SP2-PR-VALUE TO WS-FD-USER-DATA-ALL
      ******************************************************************
      **   Delete original SP2 field
      ******************************************************************
           MOVE SP2-DELETE-FIELD TO CP-SP2-P1        
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF                     
           PERFORM COMPROC-CALL-SP2
      ******************************************************************
      **   Create new field
      ******************************************************************
           MOVE "i" TO SP2-FD-CTRL-TYPE                                  
           MOVE X"05" TO SP2-FD-TYPE                                   
           MOVE X"C0" TO SP2-FD-OPTIONS-4     
           MOVE WS-HOLD-FD-WIDTH TO SP2-FD-WIDTH                      
           MOVE WS-HOLD-FD-HEIGHT TO SP2-FD-HEIGHT
           MOVE "Checked" TO SP2-FD-VAR-DATA (SP2-FD-FORMAT-LEN + 1 :) 
           MOVE 7 TO SP2-FD-CAPTION-LEN                                
           MOVE WS-EDIT-OBJ-NAME                                   
           TO SP2-FD-VAR-DATA (SP2-FD-FORMAT-LEN                       
           + SP2-FD-CAPTION-LEN + 1:)                                  
           MOVE WS-OBJ-NAME-LEN TO SP2-FD-INITIAL-LEN                  
           MOVE WS-HOLD-DISCRETE-VALUES                                
           TO SP2-FD-VAR-DATA (SP2-FD-FORMAT-LEN                       
           + SP2-FD-CAPTION-LEN + SP2-FD-INITIAL-LEN + 1:)             
           move 2 to SP2-FD-DISCRETE-LEN  
           IF SP2-FD-USER-LEN > 1
               MOVE WS-FD-USER-DATA-ALL(1:SP2-FD-USER-LEN)
                   TO sp2-fd-var-data (SP2-FD-FORMAT-LEN         
                   + SP2-FD-CAPTION-LEN + SP2-FD-INITIAL-LEN 
                   + SP2-FD-DISCRETE-LEN + 1:)
           END-IF
           MOVE WS-HOLD-USER-LEN TO SP2-FD-USER-LEN         
           COMPUTE SP2-FD-VAR-LEN                           
               = SP2-FD-FORMAT-LEN + SP2-FD-CAPTION-LEN     
               + SP2-FD-INITIAL-LEN + SP2-FD-DISCRETE-LEN
               + WS-HOLD-USER-LEN 
           MOVE SP2-FD-MAX-LEN TO SP2-FD-ITEM-LEN     
           MOVE X"04" TO SP2-FD-MISC-OPTIONS                             *>EB#56255
      ******************************************************************
      **   Dev Express Phase 2 - Menu themes in core screens.
      ******************************************************************
      *    MOVE WS-MENU-SMALL-FONT-ID TO SP2-FD-FONT-ID                 
      ******************************************************************
      **   Create new control!
      ******************************************************************
           MOVE SP2-SET-FIELD-DEF TO CP-SP2-P1      
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF
           PERFORM COMPROC-CALL-SP2   
      ****************************************************************** 
      **   Get Properties Property for CheckEdit Object at SP2-FD-ID     
      ****************************************************************** 
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE SP2-FD-ID TO SP2-NE-ID
           MOVE "Properties" TO SP2-NE-PROP-NAME
           move
           "DevExpress.XtraEditors.Repository.RepositoryItemCheckEdit"
               to sp2-ne-value-type
           move 32 to sp2-ne-options  
           MOVE SP2-GET-NET TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM             
           PERFORM COMPROC-CALL-SP2                                     
           
           IF SP2-NE-RET-CODE NOT = ZERO
               EXIT PARAGRAPH
           END-IF
           MOVE SP2-NE-VALUE TO WS-RepositoryItemTextEdit-PTR
      ******************************************************************     
      **   Get appearance subproperty from Properties subproperty.
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE SP2-FD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "Appearance" TO SP2-NE-PROP-NAME
           MOVE WS-RepositoryItemTextEdit-PTR TO SP2-NE-OBJECT-PTR
           MOVE SP2-GET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2                                 
           IF SP2-NE-RET-CODE = ZERO
               MOVE SP2-NE-VALUE TO WS-TEXTPROPS-APPEARANCE-PTR
           END-IF  
      ******************************************************************     
      **   Get TextOptions subproperty from Options subproperty.
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE SP2-FD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "TextOptions" TO SP2-NE-PROP-NAME
           MOVE WS-TEXTPROPS-APPEARANCE-PTR TO SP2-NE-OBJECT-PTR
           MOVE SP2-GET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2                                 
           IF SP2-NE-RET-CODE = ZERO
               MOVE SP2-NE-VALUE TO WS-TEXTPROPS-APRNC-TXTOPT-PTR
           END-IF  
      ******************************************************************     
      **   Set focus rectangle around the entire field instead of just
      **   the caption
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA                                *>EB#54404
           MOVE SP2-FD-ID TO SP2-NE-ID                                   *>EB#54404
           MOVE 1 TO SP2-NE-OPTIONS                                      *>EB#54404
           MOVE "FullFocusRect" TO SP2-NE-PROP-NAME                      *>EB#54404
           MOVE WS-RepositoryItemTextEdit-PTR TO SP2-NE-OBJECT-PTR       *>EB#54404
           move "1" to sp2-ne-value                                      *>EB#54404
           MOVE SP2-SET-NET TO CP-SP2-P1                                 *>EB#54404
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                      *>EB#54404
           PERFORM COMPROC-CALL-SP2                                      *>EB#54404
      ******************************************************************     
      **   Set Properties.Appearance.TextOptions.HAlignment to 
      **   HorzAlignment.Far
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE SP2-FD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "HAlignment" TO SP2-NE-PROP-NAME
           MOVE WS-TEXTPROPS-APRNC-TXTOPT-PTR TO SP2-NE-OBJECT-PTR
           move "HorzAlignment.Near" to sp2-ne-value                     *>SK#50890
           MOVE SP2-SET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2  
                             
           IF WS-CREATE-LABEL
      *        PERFORM CREATE-LABEL
               PERFORM SET-CHECKBOX-CAPTION
               INITIALIZE WS-CREATE-LABEL-FLAG
           END-IF             
      ******************************************************************
      **   Dev Express Phase 2 - Menu themes in core screens.
      **   Set CheckEdit.BackColor 
      ******************************************************************
      *    IF WS-APPLY-THEME-YES                                         *>themecode
      *        MOVE LOW-VALUES TO SP2-NE-DATA                            *>themecode
      *        MOVE FC-ID(FC-IND) TO SP2-NE-ID                           *>themecode
      *        MOVE "BackColor" TO SP2-NE-PROP-NAME                      *>themecode
      *        MOVE WS-FIELD-BACK-COLOR-PTR TO SP2-NE-VALUE              *>themecode
      *        MOVE SP2-SET-NET TO CP-SP2-P1                             *>themecode
      *        SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                  *>themecode
      *        PERFORM COMPROC-CALL-SP2                                  *>themecode
      ****************************************************************** *>themecode
      **   Set CheckEdit.ForeColor                                       *>themecode
      ****************************************************************** *>themecode
      *        MOVE LOW-VALUES TO SP2-NE-DATA                            *>themecode
      *        MOVE FC-ID(FC-IND) TO SP2-NE-ID                           *>themecode
      *        MOVE "ForeColor" TO SP2-NE-PROP-NAME                      *>themecode
      *        MOVE WS-FIELD-FORE-COLOR-PTR TO SP2-NE-VALUE              *>themecode
      *        MOVE SP2-SET-NET TO CP-SP2-P1                             *>themecode
      *        SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                  *>themecode
      *        PERFORM COMPROC-CALL-SP2                                  *>themecode
      *    END-IF                                                        *>themecode
           PERFORM SET-FIELD-PROTECTION
           PERFORM SET-UNIQUE-ID                                         *>EB#56718
           
      *    MOVE LOW-VALUES TO SP2-FD-DATA
      *                       SP2-FD-VAR-DATA 
      *    MOVE LOW-VALUES TO SP2-FD-VAR-LENS
      *    MOVE 4000 TO SP2-FD-VAR-LEN      
      *    MOVE SP2-NE-ID TO SP2-FD-ID           
      *    MOVE SP2-GET-FIELD-DEF TO CP-SP2-P1       
      *    SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF  
      *    PERFORM COMPROC-CALL-SP2
               
           .
           
      ****************************************************************** 
       SET-CHECKBOX-CAPTION. *> Set "Text" property of DevEx checkbox
      ****************************************************************** 
      * Previously the checkbox captions were separate controls, either
      * Label controls or transparent TextEdit controls. However, this
      * makes it harder for checkboxes that to be hidden, because it
      * requires two controls to be hidden.
      ******************************************************************
      * get into Properties.Text
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE SP2-FD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "Text" TO SP2-NE-PROP-NAME
      *    MOVE WS-RepositoryItemTextEdit-PTR TO SP2-NE-OBJECT-PTR
      * note to self: above line breaks things; appears no pointer is necessary here
           MOVE WS-HOLD-CAPTION(8:)
               TO sp2-ne-value
           MOVE SP2-SET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2
      *    IF SP2-NE-RET-CODE = ZERO
      *        MOVE SP2-NE-VALUE TO WS-TEXTPROPS-APPEARANCE-PTR
      *    END-IF
           .
            
           
      ******************************************************************           
       SET-UNIQUE-ID.                                                    *>EB#56718
      ******************************************************************  
      **  UniqueID is required for automation to work.  SP2-FD-ID should 
      **  be populated with the field to update.
      ******************************************************************      
           MOVE LOW-VALUES TO SP2-NE-DATA                            
           MOVE SP2-FD-ID  TO SP2-NE-ID                              
      *    MOVE 1 TO SP2-NE-OPTIONS                                  
           MOVE "UniqueID" TO SP2-NE-PROP-NAME 
           MOVE SP2-FD-ID TO WS-HOLD-INT 
           MOVE WS-HOLD-INT TO SP2-NE-VALUE                                  
           MOVE SP2-SET-NET TO CP-SP2-P1                             
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                  
           PERFORM COMPROC-CALL-SP2                                  
           .
      ******************************************************************
       EXPAND-PIC-CLAUSE.                                                *>SM#23002
      ******************************************************************
      **   If a pic clause was entered with parentheses to indicate a
      **   number of digits, we need to expand that into a COBOL edited
      **   numeric definition, which is a character for every byte
      **   position. This was needed as part of the project to replace
      **   SP2 text boxes with DevExpress TextEdit controls because the
      **   edit mask we assign to the DevExpress fields is effectivly in
      **   edited numeric format, so we were losing leading zeros and 
      **   commas in fields that were not tagged with a format tag.
      ******************************************************************
           
           MOVE WS-HOLD-EDIT-MASK TO WS-WORK-STRING
      **   TODO: remove these variables from working storage
      *          if they are not used elsewhere
      *    MOVE SPACE TO WS-INTEGER-STRING
      *                  WS-DECIMAL-STRING
      *                  INT-CHAR
      *                  DEC-CHAR
      *    MOVE ZERO TO WS-DEC-DIGITS-NUM              
      *                 WS-NON-DEC-DIGITS 
      *                 WS-START
      *                 WS-LEN 
      * The code below removes all "(1)" from WS-WORK-STRING.
      * It also removes leading zeroes if they end in a "1)". Example: "(0001)"
           MOVE 1 to CUR-IND                                               
           PERFORM UNTIL WS-WORK-STRING(CUR-IND:3) = SPACES                
               IF WS-WORK-STRING(CUR-IND:2)="(0"        
                   MOVE CUR-IND TO CUR-POS-IND
                   ADD 1 TO CUR-POS-IND
                   PERFORM UNTIL WS-WORK-STRING(CUR-POS-IND:1) NOT = "0"
                       ADD 1 TO CUR-POS-IND
                   END-PERFORM     
                   IF WS-WORK-STRING(CUR-POS-IND:2)="1)"
                       STRING                                                  
                           WS-WORK-STRING(1:CUR-IND - 1)                       
                           DELIMITED BY SIZE                               
                           WS-WORK-STRING(CUR-POS-IND + 2:98 - CUR-POS-IND)   
                           DELIMITED BY SIZE                               
                           "         "                                             
                           DELIMITED BY SIZE                               
                           INTO WS-WORK-STRING                             
                       END-STRING         
                   END-IF
               END-IF
               IF WS-WORK-STRING(CUR-IND:3)="(1)"                       
                   STRING                                               
                       WS-WORK-STRING(1:CUR-IND - 1)                
                       DELIMITED BY SIZE                            
                       WS-WORK-STRING(CUR-IND + 3:100 - CUR-IND - 3)
                       DELIMITED BY SIZE                            
                       "   "                                        
                       DELIMITED BY SIZE                            
                       INTO WS-WORK-STRING                          
                   END-STRING                                           
               END-IF    
               ADD 1 TO CUR-IND                                            
           END-PERFORM                                                  
      ********************************************************************
           MOVE 1 to CUR-IND
                        
           PERFORM UNTIL WS-WORK-STRING(CUR-IND:1) = SPACES
               INITIALIZE WS-START
               PERFORM UNTIL WS-WORK-STRING(CUR-IND:1) = SPACES OR WS-WORK-STRING(CUR-IND:1) = ")"
                   IF WS-WORK-STRING(CUR-IND:1) = "("
                       COMPUTE WS-START = CUR-IND + 1
                       IF CUR-IND > 1
                           MOVE WS-WORK-STRING(CUR-IND - 1:1) TO INT-CHAR
                       END-IF
                   END-IF
                   ADD 1 TO CUR-IND
               END-PERFORM
               IF WS-WORK-STRING(CUR-IND:1) = ")"
                   COMPUTE WS-LEN = CUR-IND - WS-START
                   PERFORM EXPAND-PARENTHESES
               END-IF
               ADD 1 TO CUR-IND
           END-PERFORM 
           .
       
       EXPAND-PARENTHESES.
           IF WS-START > ZERO
               AND WS-LEN > ZERO    
               MOVE WS-WORK-STRING(WS-START:WS-LEN) TO WS-NON-DEC-DIGITS-X(7 - WS-LEN:WS-LEN)
           ELSE
               EXIT PARAGRAPH                               
           END-IF
           INITIALIZE SETVAL-DATA
           MOVE 1 TO WS-STR-CTR
           IF INT-CHAR > SPACE
               AND WS-NON-DEC-DIGITS > ZERO
               PERFORM VARYING WS-LOOP-CTR FROM 1 BY 1                  
                   UNTIL WS-LOOP-CTR >= WS-NON-DEC-DIGITS
                       STRING                                           
                           INT-CHAR                                         
                           DELIMITED BY SIZE                            
                           INTO SETVAL-DATA                            
                           WITH POINTER WS-STR-CTR                        
                       END-STRING                                       
                       ADD 1 TO MAX-DISPLAY-LEN                                 
               END-PERFORM
           END-IF   
           IF WS-STR-CTR > 1
               MOVE WS-WORK-STRING(WS-START + WS-LEN + 1:) TO WS-STRING(1)
               MOVE SETVAL-DATA TO WS-WORK-STRING(WS-START - 1:WS-STR-CTR - 1)
               MOVE WS-STRING(1) TO WS-WORK-STRING(WS-START + WS-STR-CTR - 2:)
           END-IF
           .
      
      

      ******************************************************************           
       UPDATE-LEGACY-DATE.        
      ******************************************************************           
      **   Branch here from UPDATE-TEXTEDIT when we encounter a legacy
      **   date field. When we come here, we have already read the field
      **   definition and begun creating new field but we could not 
      **   continue using the textedit options, so finish off setting
      **   properties and creating legacy date field here.
      ******************************************************************           
      **   Build "Initial" Var Data String for a date field.
      ******************************************************************
           MOVE FLD-ID TO SP2-FD-ID
           INITIALIZE WS-EDIT-OBJ-NAME
           MOVE 1 TO WS-OBJ-NAME-LEN
               STRING
                   "        "                                                             
                   "LD  .net    "
                   "GSSERP.Support.GSDateEdit"
                   "/assembly=GSSERP.Support" 
                   DELIMITED BY SIZE                                                   
                   INTO WS-EDIT-OBJ-NAME                                           
                   WITH POINTER WS-OBJ-NAME-LEN                                             
               END-STRING
           MOVE SP2-DELETE-FIELD TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF                 
           PERFORM COMPROC-CALL-SP2                                  
      ******************************************************************
      **   Apparently field height can be zero and a default value will
      **   will be applied by SP2...but not .NET controls. So if height
      **   is zero, assign a default field height here.
      ******************************************************************
           IF SP2-FD-HEIGHT = ZERO                                  
               MOVE 170 TO SP2-FD-HEIGHT
           END-IF        
      ******************************************************************
      **   Customize the field before recreating it as a .NET control
      ******************************************************************
           MOVE "i" TO SP2-FD-CTRL-TYPE
           MOVE X"05" TO SP2-FD-TYPE       
           if sp2-fd-output = "p" or "y"
               move X"C0" to SP2-FD-OPTIONS-4
           else
               move X"80" to sp2-fd-options-4
           end-if
      ******************************************************************
      **   WS-HOLD-DISP-FMT Replace 'm' by 'M' and lowercase the rest.
      **   No regional flipping here.                                    *>SK#53051
      ******************************************************************     
      **   Required for JB0620 and maybe more programs. **               
           IF WS-HOLD-EDIT-MASK = "YY/YY/MMDD"                           *>EB#55092
               MOVE "MM/DD/YYYY" TO WS-HOLD-EDIT-MASK                    *>EB#55092
           END-IF                                                        *>EB#55092
      **   **   **  **  **  **  **  **  **  **  **  **  **
           IF WS-HOLD-EDIT-MASK(1:2) = "99"
               MOVE "mm/dd/yyyy" TO WS-HOLD-EDIT-MASK(1:SP2-FD-FORMAT-LEN)
           ELSE
               MOVE 1 TO WS-STR-CTR
               PERFORM VARYING WS-LOOP-CTR FROM 1 BY 1
                   UNTIL WS-LOOP-CTR > 100
                   OR WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) = SPACE
                   
                   IF WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) = 'M'
                       MOVE 'm' TO WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1)
                   END-IF
                   IF WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) = 'D'
                       MOVE 'd' TO WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1)
                   END-IF
                   IF WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1) = 'Y'
                       MOVE 'y' TO WS-HOLD-EDIT-MASK(WS-LOOP-CTR:1)
                   END-IF
               END-PERFORM
           END-IF
           IF SP2-FD-FORMAT-LEN >= 10
               MOVE 600 TO SP2-FD-WIDTH
               *>THIS IS NECESSARY FOR TAHOMA SIZE 8 FONT
           END-IF
      ****************************************************************** *>SK#53051
      **   Before modifying VAR-DATA, stash INITIAL-VAL                  *>SK#53051
      **   (may contain important "NullDate" configuration)              *>SK#53051
      ****************************************************************** *>SK#53051
           IF SP2-FD-INITIAL-LEN > 0                                     *>SK#53051
               MOVE SP2-FD-VAR-DATA (SP2-FD-FORMAT-LEN                   *>SK#53051
                 + SP2-FD-CAPTION-LEN + 1 : SP2-FD-INITIAL-LEN)          *>SK#53051
               TO WS-HOLD-INITIAL-VALUE                                    *>SK#53051
           END-IF                                                        *>SK#53051
      ******************************************************************
      **   Build new SP2-FD-VAR-DATA string, using a DateTime object for
      **   the SP2 "Fields Area" property and specifying the object name
      **   with WS-EDIT-OBJ-NAME.
      **   ------------------------------------------------------------  *>SK#53051
      **   Note: "WS-OBJ-NAME-LEN" is a pointer, whereas the other -LEN  *>SK#53051
      **   values are just integers. Pointer used on indexing will have  *>SK#53051
      **   value increased by 1, so "+1" not needed when index by ptr.   *>SK#53051
      **   Furthermore, using "ADD A TO B" (A being pointer and B being  *>SK#53051
      **   integer) makes B a pointer too. Use COMPUTE to avoid this.    *>SK#53051
      ******************************************************************
           MOVE WS-HOLD-EDIT-MASK TO SP2-FD-VAR-DATA                
           MOVE WS-HOLD-FORMAT-LEN TO SP2-FD-FORMAT-LEN                
                                      SP2-FD-MAX-LEN
           MOVE "DateTime"                                 
               TO SP2-FD-VAR-DATA (SP2-FD-FORMAT-LEN + 1:)
           MOVE 8 TO SP2-FD-CAPTION-LEN                
           MOVE WS-EDIT-OBJ-NAME TO SP2-FD-VAR-DATA (SP2-FD-FORMAT-LEN                       
               + SP2-FD-CAPTION-LEN + 1:)                                   
           IF SP2-FD-INITIAL-LEN > 0                                     *>SK#53051
               MOVE "/init-val=" TO SP2-FD-VAR-DATA (SP2-FD-FORMAT-LEN   *>SK#53051
               + SP2-FD-CAPTION-LEN + WS-OBJ-NAME-LEN:)                  *>SK#53051
               ADD 10 TO WS-OBJ-NAME-LEN                                 *>SK#53051
               MOVE WS-HOLD-INITIAL-VALUE                                *>TAGTXTSK
                   TO SP2-FD-VAR-DATA (SP2-FD-FORMAT-LEN                 *>SK#53051
                   + SP2-FD-CAPTION-LEN + WS-OBJ-NAME-LEN:)              *>SK#53051
               COMPUTE SP2-FD-INITIAL-LEN = SP2-FD-INITIAL-LEN           *>SK#53051
                 + WS-OBJ-NAME-LEN - 1                                   *>SK#53051
           ELSE                                                          *>SK#53051
               MOVE WS-OBJ-NAME-LEN TO SP2-FD-INITIAL-LEN                *>SK#53051
           END-IF                                                        *>SK#53051
           MOVE WS-FD-USER-DATA-ALL
               TO sp2-fd-var-data (SP2-FD-FORMAT-LEN        
               + SP2-FD-CAPTION-LEN + SP2-FD-INITIAL-LEN + 1:)  
           MOVE WS-HOLD-USER-LEN TO SP2-FD-USER-LEN         
           COMPUTE SP2-FD-VAR-LEN                           
               = SP2-FD-FORMAT-LEN + SP2-FD-CAPTION-LEN     
               + SP2-FD-INITIAL-LEN + WS-HOLD-USER-LEN      
           MOVE SP2-FD-MAX-LEN TO SP2-FD-ITEM-LEN    
           MOVE LOW-VALUES TO SP2-FD-MISC-OPTIONS
      ******************************************************************
      **   Dev Express Phase 2 - Menu Themes in Core Screens.
      ******************************************************************
      *    MOVE WS-MENU-FONT-ID TO SP2-FD-FONT-ID                        *>themecode
      ******************************************************************
      **   Create new control!
      ******************************************************************
           MOVE SP2-SET-FIELD-DEF TO CP-SP2-P1        
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF  
           PERFORM COMPROC-CALL-SP2
           
      ******************************************************************
      **   setting properties for new sp2net/devex controls
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA                                          
           MOVE SP2-FD-ID TO SP2-NE-ID                                             
           MOVE "Properties" TO SP2-NE-PROP-NAME                                   
           move                                                                    
           "DevExpress.XtraEditors.Repository.RepositoryItemDateEdit"              
               to sp2-ne-value-type                                                
           move 32 to sp2-ne-options                                               
           MOVE SP2-GET-NET TO CP-SP2-P1            
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM 
           PERFORM COMPROC-CALL-SP2                 
           MOVE SP2-NE-VALUE TO WS-RepositoryItemDateEdit-PTR                      
           PERFORM APPLY-DATE-MASK                                                 
           PERFORM HIDE-BROWSER-BUTTON
           .      
      
      ******************************************************************           
       APPLY-DATE-MASK.                                                            
      ******************************************************************           
      **   Try setting NullDate property to 00010101 to see if it allows           
      **   setting date to null. This sort of works, but is not                    
      **   sufficient for our needs as current programs are simply                 
      **   moving zero to fields area. Setting this now sets date to null          
      **   by moving zero to fields area, but we still aren't getting              
      **   zero back out when getting edit value.                                  
      ******************************************************************           
           MOVE LOW-VALUES TO SP2-NE-DATA                                          
           MOVE FLD-ID TO SP2-NE-ID                                         
           MOVE "DateTime" TO SP2-NE-VALUE                                         
           MOVE 7 TO SP2-NE-ACCESS-TYPE                                            
           MOVE 3 TO SP2-NE-PARM-CNT                                               
      ******no idea what was going on in next six lines / if necessary to keep  
      *    MOVE 0001 TO SP2-NE-PARM-VALUE(1)                                       
      *    MOVE "Int32" TO SP2-NE-PARM-TYPE(1)                                     
      *    MOVE 01 TO SP2-NE-PARM-VALUE(2)                                         
      *    MOVE "Int32" TO SP2-NE-PARM-TYPE(2)                                     
      *    MOVE 01 TO SP2-NE-PARM-VALUE(3)                                         
      *    MOVE "Int32" TO SP2-NE-PARM-TYPE(3)                                     
           
           MOVE 01 TO SP2-NE-PARM-VALUE(1)                                       
           MOVE "Int32" TO SP2-NE-PARM-TYPE(1)                                     
           MOVE 01 TO SP2-NE-PARM-VALUE(2)                                         
           MOVE "Int32" TO SP2-NE-PARM-TYPE(2)                                     
           MOVE 0001 TO SP2-NE-PARM-VALUE(3)                                         
           MOVE "Int32" TO SP2-NE-PARM-TYPE(3)                                     
           MOVE SP2-SET-NET TO CP-SP2-P1                                 *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                      *>FUJITSU 
           PERFORM COMPROC-CALL-SP2                                      *>FUJITSU 
           MOVE SP2-NE-VALUE TO WS-SP2-DATE-PTR                                    
           IF SP2-NE-RET-CODE = ZERO                                               
               MOVE LOW-VALUES TO SP2-NE-DATA                                      
               MOVE FLD-ID TO SP2-NE-ID                                     
               MOVE 1 TO SP2-NE-OPTIONS                                            
               MOVE WS-RepositoryItemDateEdit-PTR TO SP2-NE-OBJECT-PTR             
               MOVE "NullDate" TO SP2-NE-PROP-NAME                                 
               move WS-SP2-DATE-PTR to sp2-ne-value                                
               MOVE SP2-SET-NET TO CP-SP2-P1                             *>FUJITSU 
               SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                  *>FUJITSU 
               PERFORM COMPROC-CALL-SP2                                  *>FUJITSU 
           END-IF                                                                  
                                                                                   
      ******************************************************************           
      **   WS-HOLD-DISP-FMT Replace all 'm' by 'M'.
      ******************************************************************           
           INITIALIZE WS-HOLD-DISP-FMT                                             
                      WS-WORK-EDIT-MASK    
           MOVE 1 TO WS-STR-CTR                                                    
           MOVE WS-HOLD-EDIT-MASK TO WS-HOLD-DISP-FMT                              
           PERFORM VARYING WS-LOOP-CTR FROM 1 BY 1                                 
               UNTIL WS-LOOP-CTR > 100                                             
               OR WS-HOLD-DISP-FMT(WS-LOOP-CTR:1) = SPACE                          
                                                                                   
               IF WS-HOLD-DISP-FMT(WS-LOOP-CTR:1) = 'd'
                   IF FLIP-DAYS                        
                       MOVE 'M' TO WS-HOLD-DISP-FMT(WS-LOOP-CTR:1)
                   END-IF
               END-IF                                                              
               IF WS-HOLD-DISP-FMT(WS-LOOP-CTR:1) = 'm'   
                   IF FLIP-DAYS                        
                       MOVE 'd' TO WS-HOLD-DISP-FMT(WS-LOOP-CTR:1)                     
                   ELSE
                       MOVE 'M' TO WS-HOLD-DISP-FMT(WS-LOOP-CTR:1)   
                   END-IF                  
               END-IF                                                              
           END-PERFORM                                                             
      ******************************************************************           
      **   Get Mask SubProperty from Properties Property.                          
      ******************************************************************           
           MOVE LOW-VALUES TO SP2-NE-DATA                                          
           MOVE SP2-FD-ID TO SP2-NE-ID                                             
           MOVE 1 TO SP2-NE-OPTIONS                                                
           MOVE "Mask" TO SP2-NE-PROP-NAME                                         
           MOVE WS-RepositoryItemDateEdit-PTR TO SP2-NE-OBJECT-PTR                 
           MOVE SP2-GET-NET TO CP-SP2-P1                                 *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                      *>FUJITSU 
           PERFORM COMPROC-CALL-SP2                                      *>FUJITSU 
           IF SP2-NE-RET-CODE NOT = ZERO                                           
               EXIT PARAGRAPH                                                      
           END-IF                                                                  
           MOVE SP2-NE-VALUE TO WS-Mask-PTR                                        
      ******************************************************************           
      **   Set Mask Type and EditMask sub properties from Mask Property.           
      ******************************************************************           
      **   Set MaskType to DateTimeAdvancingCaret.                                 
      ******************************************************************           
      **   Set Mask.UseMaskAsDisplayFormat to True
      ******************************************************************           
           IF SP2-NE-RET-CODE = ZERO                                               
               MOVE LOW-VALUES TO SP2-NE-DATA                                      
               MOVE SP2-FD-ID TO SP2-NE-ID                                         
               MOVE 1 TO SP2-NE-OPTIONS                                            
               MOVE WS-Mask-PTR TO SP2-NE-OBJECT-PTR                               
               MOVE "MaskType" TO SP2-NE-PROP-NAME                                 
               move                                                                
               "DevExpress.XtraEditors.Mask.DateTimeAdvancingCaret"                
                   to sp2-ne-value                                                 
               MOVE SP2-SET-NET TO CP-SP2-P1                             *>FUJITSU 
               SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                  *>FUJITSU 
               PERFORM COMPROC-CALL-SP2                                  *>FUJITSU 
               MOVE LOW-VALUES TO SP2-NE-DATA                                      
               MOVE SP2-FD-ID TO SP2-NE-ID                                         
               MOVE 1 TO SP2-NE-OPTIONS                                            
               MOVE WS-Mask-PTR TO SP2-NE-OBJECT-PTR                               
               MOVE "EditMask" TO SP2-NE-PROP-NAME                                 
               move WS-HOLD-DISP-FMT to sp2-ne-value                               
               MOVE SP2-SET-NET TO CP-SP2-P1                             *>FUJITSU 
               SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                  *>FUJITSU 
               PERFORM COMPROC-CALL-SP2                                  *>FUJITSU 
               MOVE LOW-VALUES TO SP2-NE-DATA                                      
               MOVE SP2-FD-ID TO SP2-NE-ID                                         
               MOVE 1 TO SP2-NE-OPTIONS                                            
               MOVE WS-Mask-PTR TO SP2-NE-OBJECT-PTR                               
               MOVE "UseMaskAsDisplayFormat" TO SP2-NE-PROP-NAME                                 
               move "1" to sp2-ne-value                               
               MOVE SP2-SET-NET TO CP-SP2-P1                             *>FUJITSU 
               SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                  *>FUJITSU 
               PERFORM COMPROC-CALL-SP2                                  *>FUJITSU 
           END-IF                                                                  
           .                                                                       
      
      ******************************************************************
       HIDE-BROWSER-BUTTON.
      ******************************************************************
      **   settings.Properties.DropDownButton.Visible = false;
      ******************************************************************
      **   Get EditorButton collection from Buttons property.
      ******************************************************************           
           MOVE LOW-VALUES TO SP2-NE-DATA
           INITIALIZE WS-PROP-NAME
                      WS-NE-OPTIONS
           MOVE FLD-ID TO SP2-NE-ID
           MOVE "Buttons" TO SP2-NE-PROP-NAME
                             WS-PROP-NAME
           MOVE 1 TO SP2-NE-OPTIONS
                     WS-NE-OPTIONS
           MOVE WS-RepositoryItemDateEdit-PTR TO SP2-NE-OBJECT-PTR
           MOVE SP2-GET-NET TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM             
           PERFORM COMPROC-CALL-SP2                                     
       
           IF SP2-NE-RET-CODE = ZERO 
              MOVE SP2-NE-VALUE TO ws-button-collection-ptr
           END-IF
      ******************************************************************           
      **   Get the first instance of an EditorButton object from EditorButton collection.            
      ******************************************************************           
           MOVE LOW-VALUES TO SP2-NE-DATA
           INITIALIZE WS-PROP-NAME
                      WS-NE-OPTIONS
           MOVE FLD-ID TO SP2-NE-ID
           MOVE "Item" TO SP2-NE-PROP-NAME
                          WS-PROP-NAME
           MOVE 1 TO SP2-NE-OPTIONS
                     WS-NE-OPTIONS
           MOVE ws-button-collection-ptr TO SP2-NE-OBJECT-PTR
           move 1 to sp2-ne-parm-cnt
           move "0" to sp2-ne-parm-value (1)
           MOVE SP2-GET-NET TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM             
           PERFORM COMPROC-CALL-SP2                                     
      
           IF SP2-NE-RET-CODE = ZERO
               MOVE SP2-NE-VALUE TO ws-button-ptr
           END-IF
      ******************************************************************           
      **   Set the Visible property of the zeroth EditorButton object.      
      ******************************************************************           
           MOVE LOW-VALUES TO SP2-NE-DATA
           INITIALIZE WS-PROP-NAME
                      WS-NE-OPTIONS
           MOVE FLD-ID TO SP2-NE-ID
           MOVE "Visible" TO SP2-NE-PROP-NAME
                             WS-PROP-NAME
           MOVE 1 TO SP2-NE-OPTIONS
                     WS-NE-OPTIONS
           MOVE ws-button-ptr TO SP2-NE-OBJECT-PTR
           move "0" to sp2-ne-value
           MOVE SP2-SET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2                           
      
           IF SP2-NE-RET-CODE = ZERO
               MOVE SP2-NE-VALUE TO ws-property-ptr
           END-IF
           .
           
           
      ******************************************************************
       CREATE-NET-DATE.                                                  *>SM#23002
      ******************************************************************
      **  Perform SP2 call to create a .NET date time picker
      ******************************************************************
           MOVE 1 TO WS-OBJ-NAME-LEN          
           STRING    
               "        "
               "DT  .net    "
      ******************************************************************
      **   Custom Subclassed TextEdit control with Basic AutoComplete.
      ******************************************************************
               "GSSERP.Support.GSDateEdit"
               "/assembly=GSSERP.Support"
      *        "DevExpress.XtraEditors.DateEdit"
      *        "/assembly=DevExpress.XtraEditors.v16.1"                  *>SM#23002
               DELIMITED BY SIZE
               INTO WS-EDIT-OBJ-NAME
               WITH POINTER WS-OBJ-NAME-LEN
           END-STRING      
      
      ******************************************************************
      **   Get the field definition and then delete the field
      ******************************************************************
           MOVE LOW-VALUES TO SP2-FD-DATA 
                              SP2-FD-VAR-LENS                         
           MOVE 4000 TO SP2-FD-VAR-LEN    
           MOVE FLD-ID TO SP2-FD-ID                              
      ******************************************************************
      **   Get Field Protection value of date field.
      ******************************************************************
      *    MOVE 200 TO SP2-FD-INITIAL-LEN                   
           MOVE SP2-GET-FIELD-DATA TO CP-SP2-P1              
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF        
           PERFORM COMPROC-CALL-SP2                         
           MOVE SP2-FD-OUTPUT TO WS-HOLD-FD-PROTECTION                   *>SM#23002
           MOVE SP2-FD-TYPE TO WS-HOLD-FD-TYPE                           *>SM#23002
           MOVE LOW-VALUES TO SP2-FD-VAR-LENS                            *>SM#23002
           
           MOVE SP2-GET-FIELD-DEF TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF                  
           PERFORM COMPROC-CALL-SP2                                         
           PERFORM GET-USER-DATA                                         *>SM#23002
           MOVE SP2-FD-NAME TO WS-HOLD-FD-NAME                          
           MOVE SP2-FD-WIDTH TO WS-HOLD-FD-WIDTH                         *>SM#23002
           MOVE sp2-fd-format-len TO WS-HOLD-FORMAT-LEN
           MOVE SP2-FD-VAR-DATA(1:SP2-FD-FORMAT-LEN) TO WS-HOLD-EDIT-MASK
           MOVE SP2-DELETE-FIELD TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF                   
           PERFORM COMPROC-CALL-SP2
      
      ******************************************************************
      **   Customize the field before recreating it as a .NET control
      ******************************************************************
           COMPUTE SP2-FD-ID = FLD-ID 
      ******************************************************************
      **   Apparently field height can be zero and a default value will
      **   will be applied by SP2...but not .NET controls. So if height
      **   is zero, assign a default field height here.
      ******************************************************************
           IF SP2-FD-HEIGHT = ZERO                                  
               MOVE 170 TO SP2-FD-HEIGHT
           END-IF           
           MOVE "i" TO SP2-FD-CTRL-TYPE
           MOVE X"05" TO SP2-FD-TYPE       
           move X"C0" to SP2-FD-OPTIONS-4
           IF WS-HOLD-FORMAT-LEN = 5
               AND DATE-DIGITS = 8                                       *>SM#23002
               move "mm/dd/yyyy" to sp2-fd-var-data
               move 10 to sp2-fd-format-len sp2-fd-max-len 
               MOVE 600 TO SP2-FD-WIDTH
               *>THIS IS NECESSARY FOR TESTING TAHOMA SIZE 8 FONT
           ELSE
               move WS-HOLD-EDIT-MASK to sp2-fd-var-data                 
               move WS-HOLD-FORMAT-LEN to sp2-fd-format-len 
           END-IF
           IF NOT WS-DATENB-TAG-FOUND
               ADD 180 TO SP2-FD-WIDTH  
           END-IF
      ******************************************************************           
      **   Caption - DateTime - 8 bytes - returns spaces.
      ******************************************************************           
           move "DateTime" to sp2-fd-var-data (sp2-fd-format-len + 1 :)            
           move 8 to sp2-fd-caption-len                                            
      
           MOVE WS-EDIT-OBJ-NAME 
           TO sp2-fd-var-data (sp2-fd-format-len 
           + sp2-fd-caption-len + 1 :)
        
           move WS-OBJ-NAME-LEN to sp2-fd-initial-len 
      
           IF WS-HOLD-USER-LEN > ZERO
               MOVE WS-FD-USER-DATA(1:WS-HOLD-USER-LEN)                  *>SM#23002
               TO sp2-fd-var-data (SP2-FD-FORMAT-LEN                     *>SM#23002
               + SP2-FD-CAPTION-LEN + SP2-FD-INITIAL-LEN                 *>SM#23002
               + 1:WS-HOLD-USER-LEN)                                     *>SM#23002
               MOVE WS-HOLD-USER-LEN TO SP2-FD-USER-LEN                  *>SM#23002
           END-IF
      
           COMPUTE SP2-FD-VAR-LEN                                        *>SM#23002
               = SP2-FD-FORMAT-LEN + SP2-FD-CAPTION-LEN                  *>SM#23002
               + SP2-FD-INITIAL-LEN + WS-HOLD-USER-LEN                   *>SM#23002
           move sp2-fd-max-len to sp2-fd-item-len
           MOVE LOW-VALUES TO SP2-FD-MISC-OPTIONS
      *******************************************************************
      ***   Resize control for DevEx font.
      ***   NOTE: Font has not yet been determined; waiting on management
      ***         to make the decision, so this resizing is test code.
      *******************************************************************
      *    IF SP2-FD-REPEAT-ID = ZERO
      *        COMPUTE WS-HOLD-FD-WIDTH = SP2-FD-WIDTH * 1.03
      *        MOVE WS-HOLD-FD-WIDTH TO SP2-FD-WIDTH
      *    END-IF
      ******************************************************************
      **   Set field definition for the new control.
      ******************************************************************
           MOVE SP2-SET-FIELD-DEF TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-FIELD-DEF                 
           PERFORM COMPROC-CALL-SP2     
           .
           
      ******************************************************************
       SET-DATE-TIME-ADVANCING-CARET.                                    *>SM#23002
      ******************************************************************
      **  set the MaskType to DateTimeAdvancingCaret, like so:
      **  dateEdit1.Properties.Mask.MaskType = DevExpress.XtraEditors.Mask.MaskType.DateTimeAdvancingCaret
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE FLD-ID TO SP2-NE-ID
           MOVE "Properties" TO SP2-NE-PROP-NAME
           move
           "DevExpress.XtraEditors.Repository.RepositoryItemDateEdit"
               to sp2-ne-value-type
           move 32 to sp2-ne-options
           MOVE SP2-GET-NET TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                 
           PERFORM COMPROC-CALL-SP2                                        

           IF SP2-NE-RET-CODE NOT = ZERO
               EXIT PARAGRAPH
           END-IF
           MOVE SP2-NE-VALUE TO WS-RepoItemDateEdit-PTR
          
      ******************************************************************
      **   Try setting NullDate property to 00010101 to see if it allows
      **   setting date to null.       
      ******************************************************************
           MOVE SP2-NE-VALUE TO WS-LAST-SP2-PTR
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE FLD-ID TO SP2-NE-ID 
           MOVE "DateTime" TO SP2-NE-VALUE                             
           MOVE 7 TO SP2-NE-ACCESS-TYPE
           MOVE 3 TO SP2-NE-PARM-CNT
           MOVE 0001 TO SP2-NE-PARM-VALUE(1)
           MOVE "Int32" TO SP2-NE-PARM-TYPE(1)
           MOVE 01 TO SP2-NE-PARM-VALUE(2)
           MOVE "Int32" TO SP2-NE-PARM-TYPE(2)
           MOVE 01 TO SP2-NE-PARM-VALUE(3)
           MOVE "Int32" TO SP2-NE-PARM-TYPE(3)
           MOVE SP2-SET-NET TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                 
           PERFORM COMPROC-CALL-SP2                                        
           MOVE SP2-NE-VALUE TO WS-SP2-DATE-PTR
           IF SP2-NE-RET-CODE = ZERO
               MOVE LOW-VALUES TO SP2-NE-DATA
               MOVE FLD-ID TO SP2-NE-ID
               MOVE 1 TO SP2-NE-OPTIONS
               MOVE WS-RepoItemDateEdit-PTR TO SP2-NE-OBJECT-PTR
               MOVE "NullDate" TO SP2-NE-PROP-NAME
               move WS-SP2-DATE-PTR to sp2-ne-value
               MOVE SP2-SET-NET TO CP-SP2-P1                        
               SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                 
               PERFORM COMPROC-CALL-SP2                                        
           END-IF
           .

           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE FLD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "Mask" TO SP2-NE-PROP-NAME
           MOVE WS-RepoItemDateEdit-PTR TO SP2-NE-OBJECT-PTR
           MOVE SP2-GET-NET TO CP-SP2-P1                        
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                 
           PERFORM COMPROC-CALL-SP2                                        

           IF SP2-NE-RET-CODE NOT = ZERO
               EXIT PARAGRAPH
           END-IF

           MOVE SP2-NE-VALUE TO WS-LAST-SP2-PTR
        
           IF SP2-NE-RET-CODE = ZERO
               MOVE LOW-VALUES TO SP2-NE-DATA
               MOVE FLD-ID TO SP2-NE-ID
               MOVE 1 TO SP2-NE-OPTIONS
               MOVE WS-LAST-SP2-PTR TO SP2-NE-OBJECT-PTR
               MOVE "MaskType" TO SP2-NE-PROP-NAME
               move
               "DevExpress.XtraEditors.Mask.DateTimeAdvancingCaret"
                   to sp2-ne-value
               MOVE SP2-SET-NET TO CP-SP2-P1                        
               SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM                 
               PERFORM COMPROC-CALL-SP2                                        
           END-IF       
           .
      
            
      ******************************************************************
       SET-FIELD-PROTECTION.      
      ******************************************************************
      **   Set field enabled to false when display only or greyed out.
      ******************************************************************
      *    IF WS-HOLD-FD-DISP-ONLY
      *        OR WS-HOLD-FD-GREYED
      *        move low-values to SP2-NE-DATA
      *        move SCR110-DATE-ID to SP2-NE-ID
      *        move "Enabled" to SP2-NE-PROP-NAME
      *        move B"0" to SP2-NE-VALUE
      *        MOVE sp2-set-net TO CP-SP2-P1                    
      *        SET CP-SP2-P2 TO ADDRESS OF sp2-net-parm         
      *        PERFORM COMPROC-CALL-SP2                         
      *    END-IF
      ******************************************************************
      **   Set ReadOnly to True when protected.
      ******************************************************************
           IF WS-HOLD-FD-DISP-ONLY
               OR WS-HOLD-FD-GREYED
               OR WS-HOLD-FD-PROTECTED
               move low-values to SP2-NE-DATA
               move FLD-ID to SP2-NE-ID
               move "ReadOnly" to SP2-NE-PROP-NAME
               move B"1" to SP2-NE-VALUE
               MOVE sp2-set-net TO CP-SP2-P1                    
               SET CP-SP2-P2 TO ADDRESS OF sp2-net-parm         
               PERFORM COMPROC-CALL-SP2                         
           END-IF     
           .
           
      ******************************************************************
       SET-RIGHT-JUSTIFICATION.
      ******************************************************************
      **   Get appearance subproperty from Properties subproperty.
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE FLD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "Appearance" TO SP2-NE-PROP-NAME
           MOVE WS-RepoItemDateEdit-PTR TO SP2-NE-OBJECT-PTR
           MOVE SP2-GET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2                                 
           IF SP2-NE-RET-CODE = ZERO
               MOVE SP2-NE-VALUE 
               TO WS-DATEPROPS-APPEARANCE-PTR
           END-IF  
      ******************************************************************     
      **   Get Options subproperty from Appearance subproperty.
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE FLD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "Options" TO SP2-NE-PROP-NAME
           MOVE WS-DATEPROPS-APPEARANCE-PTR TO SP2-NE-OBJECT-PTR
           MOVE SP2-GET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2                                 
           IF SP2-NE-RET-CODE = ZERO
               MOVE SP2-NE-VALUE 
               TO WS-DATEPROPS-APRNC-OPT-PTR
           END-IF  
      ******************************************************************     
      **   Get TextOptions subproperty from Options subproperty.
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE FLD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "TextOptions" TO SP2-NE-PROP-NAME
           MOVE WS-DATEPROPS-APPEARANCE-PTR TO SP2-NE-OBJECT-PTR
           MOVE SP2-GET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2                                 
           IF SP2-NE-RET-CODE = ZERO
               MOVE SP2-NE-VALUE 
               TO WS-DATEPROPS-APRNC-TXTOPT-PTR
           END-IF  
      ******************************************************************     
      **   Set Properties.Appearance.Options.UseTextOptions = true
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE FLD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "UseTextOptions" TO SP2-NE-PROP-NAME
           MOVE WS-DATEPROPS-APRNC-OPT-PTR TO SP2-NE-OBJECT-PTR
           move B"1" to sp2-ne-value
           MOVE SP2-SET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2                                 
      ******************************************************************     
      **   Set Properties.Appearance.TextOptions.HAlignment to 
      **   HorzAlignment.Far
      ******************************************************************
           MOVE LOW-VALUES TO SP2-NE-DATA
           MOVE FLD-ID TO SP2-NE-ID
           MOVE 1 TO SP2-NE-OPTIONS
           MOVE "HAlignment" TO SP2-NE-PROP-NAME
           MOVE WS-DATEPROPS-APRNC-TXTOPT-PTR TO SP2-NE-OBJECT-PTR
           move "HorzAlignment.Far" to sp2-ne-value
           MOVE SP2-SET-NET TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF SP2-NET-PARM         
           PERFORM COMPROC-CALL-SP2                                 
           .  
           
      ******************************************************************
       SET-FIELD-TYPE.                                                   *>SM#23002
      ******************************************************************
      **   Set custom field type property for controls that have 
      **   SP2-FD-TYPE = "n" or "d" so that HOOK01 can retrieve this 
      **   property when exporting hook data. This is necessary so that 
      **   dates and numeric fields get formatted correctly on hook
      **   processing.
      ******************************************************************
           move low-values to SP2-NE-DATA
           move FLD-ID to SP2-NE-ID
           move "FieldType" to SP2-NE-PROP-NAME
           move WS-HOLD-FD-TYPE to SP2-NE-VALUE
           MOVE sp2-set-net TO CP-SP2-P1                    
           SET CP-SP2-P2 TO ADDRESS OF sp2-net-parm         
           PERFORM COMPROC-CALL-SP2
           .
                                                                                    
      ******************************************************************     
       SET-SPECIAL-FORMAT.                                                         
      ******************************************************************     
           MOVE LOW-VALUES TO SP2-PR-DATA                              
           MOVE FLD-ID TO SP2-PR-ID                                
           SET SP2-PR-FIELD TO TRUE                                     
           SET SP2-PR-CHAR-T TO TRUE                                     
           MOVE 61 TO SP2-PR-OFFSET                            
           MOVE 1 TO SP2-PR-LEN                               
           MOVE "y" TO SP2-PR-VALUE                             
           MOVE SP2-SET-PROPERTY TO CP-SP2-P1                            *>FUJITSU 
           SET CP-SP2-P2 TO ADDRESS OF SP2-PROPERTY                      *>FUJITSU 
           PERFORM COMPROC-CALL-SP2
           
           IF SP2-PR-RET-CODE NOT = ZERO                                 *>FUJITSU 
               PERFORM END-PROGRAM                                                 
           END-IF.                                                                 
             
           
      ******************************************************************                                                                          
       COMPROC-CALL-SP2.
      ******************************************************************  
           CALL "GSSERP.SP2" USING CP-SP2-P1 CP-SP2-P2-R
               ON OVERFLOW
                   CONTINUE
           END-CALL.
     
           
       END PROGRAM MAIN.
