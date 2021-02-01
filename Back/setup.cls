 
 /*------------------------------------------------------------------------
    File        : Setup
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Hannes
    Created     : Wed Jan 29 12:16:29 EET 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Back.setup INHERITS Back.raamatukogu: 

    DEFINE PRIVATE TEMP-TABLE tsetup LIKE raamat.setup.
    
    METHOD PUBLIC VOID GetSetup(INPUT-OUTPUT TABLE tsetup):
        EMPTY TEMP-TABLE tsetup. 
        FIND FIRST bsetup NO-LOCK NO-ERROR.
        IF AVAILABLE bsetup THEN DO:
            CREATE tsetup.
            BUFFER-COPY bsetup TO tsetup.   
        END.               
    END.

    METHOD PUBLIC LOGICAL PutSetup(INPUT-OUTPUT TABLE tsetup):
        FIND FIRST tsetup NO-LOCK NO-ERROR.
        FIND FIRST bsetup EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bsetup AND AVAILABLE tsetup THEN DO:            
            BUFFER-COPY tsetup TO bsetup.  
            RELEASE bsetup.  
            FIND FIRST bsetup NO-LOCK NO-ERROR.
            RETURN TRUE.
        END.            
        RETURN FALSE.
    END.
               
END CLASS.