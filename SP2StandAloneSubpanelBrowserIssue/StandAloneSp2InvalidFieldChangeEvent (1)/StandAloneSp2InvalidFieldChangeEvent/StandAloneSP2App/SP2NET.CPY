      **********************
      * NET function codes *
      **********************

       01  SP2-NET-FUNCTION-CODES.
           05  SP2-SET-NET             PIC S9(4) COMP-5 VALUE +80.
           05  SP2-GET-NET             PIC S9(4) COMP-5 VALUE +81.

       01  SP2-KEY-NET                 PIC S9(4) COMP-5 VALUE -16.

      ********************************
      * NET property data            *
      * parameter for SET/GET-NET    *
      ********************************

       01  SP2-NET-PARM.
           05  SP2-NE-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-NE-LENS.
               10  SP2-NE-LEN-LEN      PIC S9(4) COMP-5 VALUE +12.
               10  SP2-NE-NUM-LEN      PIC S9(4) COMP-5 VALUE +12.
               10  SP2-NE-CHAR-LEN     PIC S9(4) COMP-5 VALUE +110.
               10  SP2-NE-VAR-LEN      PIC S9(4) COMP-5 VALUE +3200.
               10  SP2-NE-VALUE-LEN    PIC S9(4) COMP-5 VALUE +80.
               10  SP2-NE-TYPE-LEN     PIC S9(4) COMP-5 VALUE +80.
           05  SP2-NE-DATA.
             06  SP2-NE-NUM-DATA.
               10  SP2-NE-ID           PIC S9(4) COMP-5.
               10  SP2-NE-OPTIONS      PIC S9(4) COMP-5.
               10  SP2-NE-INDEX        PIC S9(4) COMP-5.
               10  SP2-NE-GET-TYPE     PIC S9(4) COMP-5.
               10  SP2-NE-ACCESS-TYPE  PIC S9(4) COMP-5.
               10  SP2-NE-PARM-CNT     PIC S9(4) COMP-5.
             06  SP2-NE-CHAR-DATA.
               10  SP2-NE-PROP-NAME    PIC X(80).
               10  SP2-NE-OBJECT-PTR   PIC X(30).
             06  SP2-NE-VAR-DATA.
               10  SP2-NE-VALUE        PIC X(80).
               10  SP2-NE-VALUE-TYPE   PIC X(80).
               10  SP2-NE-PARMS.
                 11  SP2-NE-PARM OCCURS 20.
                   15  SP2-NE-PARM-VALUE
                                       PIC X(80).
                   15  SP2-NE-PARM-TYPE 
                                       PIC X(80).

