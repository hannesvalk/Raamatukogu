 
 /*------------------------------------------------------------------------
    File        : kasutaja
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Hannes
    Created     : Wed Jan 27 14:41:07 EET 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Back.kasutaja INHERITS Back.raamatukogu: 

    DEFINE PRIVATE TEMP-TABLE tkasutajad LIKE raamat.kasutajad.

    METHOD PUBLIC VOID GetKasutajad(INPUT-OUTPUT TABLE tkasutajad):
        EMPTY TEMP-TABLE tkasutajad. 
        FOR EACH bkasutajad NO-LOCK:
            CREATE tkasutajad.
            BUFFER-COPY bkasutajad TO tkasutajad.    
        END.               
    END.

    METHOD PUBLIC VOID GetKasutajad(INPUT nimi AS CHARACTER, 
                                    INPUT username AS CHARACTER,
                                    INPUT staatus AS CHARACTER,
                                    INPUT-OUTPUT TABLE tkasutajad):
                                          
        EMPTY TEMP-TABLE tkasutajad. 
        FOR EACH bkasutajad WHERE (bkasutajad.nimi MATCHES('*' + nimi + '*')) AND
                                  (bkasutajad.username MATCHES('*' + username + '*')) AND
                                  (staatus=bkasutajad.staatus OR staatus='') NO-LOCK:
            CREATE tkasutajad.
            BUFFER-COPY bkasutajad TO tkasutajad.
        END.               
    END.
        
    METHOD PUBLIC VOID AddKasutaja(INPUT nimi AS CHARACTER, 
                                   INPUT username AS CHARACTER,
                                   INPUT password AS CHARACTER,
                                   INPUT staatus AS CHARACTER):
        IF TRIM(nimi)='' THEN RETURN.
        CREATE bkasutajad.
        ASSIGN bkasutajad.nimi = nimi
               bkasutajad.username = username    
               bkasutajad.password = password    
               bkasutajad.staatus = staatus.    
    END.
        
    METHOD PUBLIC LOGICAL DelKasutaja(INPUT id AS INTEGER):
        FIND FIRST bkasutajad WHERE bkasutajad.id = id EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bkasutajad THEN
            IF (NEW back.laenutus()):CheckDelKasutajaLaenutus(INPUT id) THEN DO:            
                DELETE bkasutajad.
                RETURN TRUE.            
            END.
        RETURN FALSE.
    END.
               
END CLASS.