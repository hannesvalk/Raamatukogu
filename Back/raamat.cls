 
 /*------------------------------------------------------------------------
    File        : raamat
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Hannes
    Created     : Wed Jan 27 14:42:02 EET 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Back.raamat INHERITS Back.raamatukogu : 

    METHOD PUBLIC VOID GetRaamatud(INPUT-OUTPUT TABLE traamatud):
        EMPTY TEMP-TABLE traamatud. 
        FOR EACH braamatud NO-LOCK:
            CREATE traamatud.
            BUFFER-COPY braamatud TO traamatud.    
        END.               
    END.

    METHOD PUBLIC VOID GetRaamatud(INPUT pealkiri AS CHARACTER, 
                                   INPUT autor AS CHARACTER,
                                   INPUT ISBN AS CHARACTER,
                                   INPUT asukoht AS CHARACTER,
                                   INPUT-OUTPUT TABLE traamatud):
                                          
        EMPTY TEMP-TABLE traamatud. 
        FOR EACH braamatud WHERE (braamatud.pealkiri MATCHES('*' + pealkiri + '*')) AND
                                 (braamatud.autor MATCHES('*' + autor + '*')) AND 
                                 (braamatud.asukoht MATCHES('*' + asukoht + '*')) AND 
                                 (braamatud.ISBN MATCHES('*' + ISBN + '*')) 
                                 NO-LOCK:
            CREATE traamatud.
            BUFFER-COPY braamatud TO traamatud.    
        END.               
    END.
        
    METHOD PUBLIC INTEGER GetTahtaeg(INPUT id AS INTEGER):
        DEFINE VARIABLE tahtaeg AS INTEGER INITIAL 28 NO-UNDO.
        DEFINE VARIABLE vaba    AS INTEGER INITIAL 0  NO-UNDO.
        
        FIND FIRST bsetup NO-LOCK NO-ERROR.
        IF AVAILABLE bsetup THEN DO:        
            tahtaeg = bsetup.raamat_tahtaeg.
            FIND FIRST braamatud WHERE braamatud.id = id NO-LOCK NO-ERROR.
            IF AVAILABLE braamatud THEN 
                IF braamatud.kuupaev > TODAY - bsetup.uus_raamat THEN 
                    tahtaeg = bsetup.uusraamat_tahtaeg.
                    
            FOR EACH blaenutused WHERE blaenutused.raamatu_id = braamatud.id AND blaenutused.laenutaja_id = -1 NO-LOCK:
                ACCUMULATE blaenutused.ID (COUNT).
                vaba = (ACCUM COUNT blaenutused.ID).
            END.
            IF vaba<bsetup.vahe_raamat AND vaba>0 THEN 
                tahtaeg = min(tahtaeg,bsetup.vaheraamat_tahtaeg).
        END. 
        RETURN tahtaeg.
    END.
        
    METHOD PUBLIC VOID AddRaamat(INPUT pealkiri AS CHARACTER, 
                                 INPUT autor AS CHARACTER,
                                 INPUT asukoht AS CHARACTER,
                                 INPUT kuupaev AS DATE,                                 
                                 INPUT ISBN AS CHARACTER,
                                 INPUT arv AS INTEGER):
        DEFINE VARIABLE i AS INTEGER.
        
        CREATE braamatud.
        ASSIGN braamatud.pealkiri = pealkiri
               braamatud.autor = autor    
               braamatud.asukoht = asukoht   
               braamatud.kuupaev  = kuupaev
               braamatud.ISBN = ISBN.
        clLaenutus = NEW back.laenutus().    
        DO i = 1 TO arv:
            clLaenutus:AddUusRaamat(INPUT braamatud.id).
        END.                   
        DELETE OBJECT clLaenutus.
    END.

    METHOD PUBLIC LOGICAL DelRaamat(INPUT id AS INTEGER):
        FIND FIRST braamatud WHERE braamatud.id = id EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE braamatud THEN DO:
            clLaenutus = NEW back.laenutus().    
            IF clLaenutus:CheckDelRaamatLaenutus(INPUT id) THEN DO:
                clLaenutus:DelLaenutus(INPUT id). 
                DELETE braamatud.
                DELETE OBJECT clLaenutus.
                RETURN TRUE.
            END.
            ELSE
                DELETE OBJECT clLaenutus.
        END.
        
        RETURN FALSE.
    END.
               
END CLASS.