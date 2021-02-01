 
 /*------------------------------------------------------------------------
    File        : laenutus
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Hannes
    Created     : Wed Jan 27 14:41:07 EET 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Back.laenutus INHERITS Back.raamatukogu: 

    {back/raamaturapStru.i}
            
    METHOD PUBLIC VOID GetLaenutused(INPUT-OUTPUT TABLE tlaenutused):
        DEFINE VARIABLE tahtaeg AS INTEGER INITIAL 28 NO-UNDO.

        EMPTY TEMP-TABLE tlaenutused. 
        FOR EACH blaenutused NO-LOCK:
            CREATE tlaenutused.
            BUFFER-COPY blaenutused TO tlaenutused.    
            IF blaenutused.laenutaja_id>-1 THEN DO: 
                FIND FIRST bkasutajad WHERE bkasutajad.id=blaenutused.laenutaja_id NO-LOCK.
                IF AVAILABLE bkasutajad THEN 
                     tlaenutused.kasutaja=bkasutajad.nimi.
            END.
            FIND FIRST braamatud WHERE braamatud.id=blaenutused.raamatu_id NO-LOCK.
            IF AVAILABLE braamatud THEN DO: 
                 tlaenutused.pealkiri=braamatud.pealkiri.
                 tlaenutused.autor=braamatud.autor.
                 tlaenutused.ISBN =braamatud.ISBN.
            END.
            tahtaeg = (NEW back.raamat()):GetTahtaeg(INPUT tlaenutused.raamatu_id).
            tlaenutused.tahtaeg = TODAY + tahtaeg. 
            
        END.               
    END.

    METHOD PUBLIC VOID GetLaenutused(INPUT pealkiri AS CHARACTER, 
                                     INPUT autor    AS CHARACTER,
                                     INPUT ISBN     AS CHARACTER,
                                     INPUT-OUTPUT TABLE tlaenutused):
        DEFINE VARIABLE tahtaeg AS INTEGER INITIAL 28 NO-UNDO.
        
        EMPTY TEMP-TABLE tlaenutused.
        FOR EACH braamatud WHERE (braamatud.pealkiri MATCHES('*' + pealkiri + '*')) AND
                                 (braamatud.autor MATCHES('*' + autor + '*')) AND 
                                 (braamatud.ISBN MATCHES('*' + ISBN + '*')) 
                                 NO-LOCK:
            FOR EACH blaenutused WHERE blaenutused.raamatu_id = braamatud.id NO-LOCK:
                CREATE tlaenutused.
                BUFFER-COPY blaenutused TO tlaenutused.    
                IF blaenutused.laenutaja_id>-1 THEN DO: 
                    FIND FIRST bkasutajad WHERE bkasutajad.id=blaenutused.laenutaja_id NO-LOCK.
                    IF AVAILABLE bkasutajad THEN 
                         tlaenutused.kasutaja=bkasutajad.nimi.
                END.
                tlaenutused.pealkiri=braamatud.pealkiri.
                tlaenutused.autor=braamatud.autor.
                tlaenutused.ISBN =braamatud.ISBN.                
                tahtaeg = (NEW back.raamat()):GetTahtaeg(INPUT tlaenutused.raamatu_id).
                tlaenutused.tahtaeg = TODAY + tahtaeg. 
                
            END.               
        END.               
    END.


    METHOD PUBLIC VOID GetRaportRaamatud(INPUT raamatu_id   AS INTEGER, 
                                         INPUT-OUTPUT TABLE traamaturaport):

        DEFINE VARIABLE arv     AS INTEGER NO-UNDO.
        DEFINE VARIABLE vaba    AS INTEGER NO-UNDO.
        DEFINE VARIABLE tagasi  AS DATE  NO-UNDO.
        DEFINE VARIABLE tahtaeg AS INTEGER INITIAL 28 NO-UNDO.

        EMPTY TEMP-TABLE traamaturaport.
        FOR EACH braamatud WHERE (raamatu_id=braamatud.id OR raamatu_id<0) NO-LOCK:
            {back/RaamatuRaport.i}
        END.                                          
    END.

    METHOD PUBLIC VOID GetRaportRaamatud(INPUT pealkiri AS CHARACTER, 
                                         INPUT autor AS CHARACTER,
                                         INPUT ISBN AS CHARACTER,
                                         INPUT-OUTPUT TABLE traamaturaport):

        DEFINE VARIABLE arv     AS INTEGER NO-UNDO.
        DEFINE VARIABLE vaba    AS INTEGER NO-UNDO.
        DEFINE VARIABLE tagasi  AS DATE  NO-UNDO.
        DEFINE VARIABLE tahtaeg AS INTEGER INITIAL 28 NO-UNDO.

        EMPTY TEMP-TABLE traamaturaport.
        FOR EACH braamatud WHERE (braamatud.pealkiri MATCHES('*' + pealkiri + '*')) AND
                                 (braamatud.autor MATCHES('*' + autor + '*')) AND 
                                 (braamatud.ISBN MATCHES('*' + ISBN + '*')) 
                                 NO-LOCK:
            {back/RaamatuRaport.i}
        END.                                          
    END.

    METHOD PUBLIC VOID GetRaportVolglased(INPUT-OUTPUT TABLE traamaturaport):

        DEFINE VARIABLE arv     AS INTEGER NO-UNDO.
        DEFINE VARIABLE vaba    AS INTEGER NO-UNDO.
        DEFINE VARIABLE tagasi  AS DATE  NO-UNDO.
        DEFINE VARIABLE tahtaeg AS INTEGER INITIAL 28 NO-UNDO.

        EMPTY TEMP-TABLE traamaturaport.
        FOR EACH blaenutused WHERE blaenutused.tahtaeg < TODAY NO-LOCK,
            EACH braamatud   WHERE braamatud.id  = blaenutused.raamatu_id NO-LOCK,
            EACH bkasutajad  WHERE bkasutajad.id = blaenutused.laenutaja_id NO-LOCK:
            arv=TODAY - blaenutused.tahtaeg.
            CREATE traamaturaport.
            ASSIGN traamaturaport.arv      = arv
                   traamaturaport.tahtaeg  = blaenutused.tahtaeg
                   traamaturaport.pealkiri = braamatud.pealkiri
                   traamaturaport.kasutaja = bkasutajad.nimi
                   .
        END.                                          
    END.
    
    METHOD PUBLIC VOID AddUusRaamat(INPUT raamatu_id   AS INTEGER):
        CREATE blaenutused.
        ASSIGN blaenutused.raamatu_id   = raamatu_id
               blaenutused.laenutaja_id = -1.
    END.
        
    METHOD PUBLIC LOGICAL CheckStartLaenutus(INPUT raamatu_id AS INTEGER):
        FIND FIRST blaenutused WHERE blaenutused.raamatu_id   = raamatu_id AND 
                                     blaenutused.laenutaja_id = -1 NO-LOCK NO-ERROR.
        RETURN AVAILABLE blaenutused.
    END.
        
    METHOD PUBLIC LOGICAL StartLaenutus(INPUT id   AS INTEGER, 
                                        INPUT laenutaja_id AS INTEGER):
                                        
        DEFINE VARIABLE tahtaeg AS INTEGER INITIAL 28 NO-UNDO.
        
        FIND FIRST blaenutused WHERE blaenutused.id = id AND
                                     blaenutused.laenutaja_id = -1 EXCLUSIVE-LOCK NO-ERROR.
                                     
        IF AVAILABLE blaenutused THEN DO:
            clRaamat = NEW back.raamat().    
            tahtaeg = clRaamat:GetTahtaeg(INPUT raamatu_id).
            ASSIGN  blaenutused.laenutaja_id = laenutaja_id
                    blaenutused.laenutatud   = TODAY
                    blaenutused.tahtaeg      = TODAY + tahtaeg.
            DELETE OBJECT clRaamat.
            RETURN TRUE.
        END.    
        ELSE 
            RETURN FALSE.
    END.
        
    METHOD PUBLIC VOID EndLaenutus(INPUT id AS INTEGER):
                                         
        FIND FIRST blaenutused WHERE blaenutused.id = id EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE blaenutused THEN
            ASSIGN blaenutused.laenutaja_id = -1  
                   blaenutused.laenutatud   = ?
                   blaenutused.tahtaeg      = ?.
    END.
        
    METHOD PUBLIC VOID EndLaenutus(INPUT raamatu_id   AS INTEGER, 
                                   INPUT laenutaja_id AS INTEGER):
                                         
        FIND FIRST blaenutused WHERE blaenutused.raamatu_id   = raamatu_id AND
                                     blaenutused.laenutaja_id = laenutaja_id EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE blaenutused THEN
            ASSIGN blaenutused.laenutaja_id = -1  
                   blaenutused.laenutatud   = DATE(12,31,2999)
                   blaenutused.tahtaeg      = DATE(12,31,2999).    
    END.
        
    METHOD PUBLIC LOGICAL CheckDelRaamatLaenutus(INPUT raamatu_id AS INTEGER):
        FIND FIRST blaenutused WHERE blaenutused.raamatu_id   = raamatu_id AND 
                                     blaenutused.laenutaja_id <> -1 
                                     NO-LOCK NO-ERROR.
        RETURN NOT AVAILABLE blaenutused.
    END.
        
    METHOD PUBLIC VOID DelLaenutus(INPUT raamatu_id AS INTEGER):
        FIND FIRST blaenutused WHERE blaenutused.raamatu_id = raamatu_id EXCLUSIVE-LOCK NO-ERROR.
        DO WHILE AVAILABLE blaenutused:
            DELETE blaenutused.
            FIND FIRST blaenutused WHERE blaenutused.raamatu_id = raamatu_id EXCLUSIVE-LOCK NO-ERROR.
        END.    
    END.

    METHOD PUBLIC LOGICAL CheckDelKasutajaLaenutus(INPUT laenutaja_id AS INTEGER):
        FIND FIRST blaenutused WHERE blaenutused.laenutaja_id = laenutaja_id NO-LOCK NO-ERROR.
        RETURN NOT AVAILABLE blaenutused.
    END.
        
    METHOD PUBLIC VOID ClearLaenutus(INPUT laenutaja_id AS INTEGER):
        FIND FIRST blaenutused WHERE blaenutused.laenutaja_id = laenutaja_id EXCLUSIVE-LOCK NO-ERROR.
        DO WHILE AVAILABLE blaenutused:
            laenutaja_id = -1.
            FIND FIRST blaenutused WHERE blaenutused.laenutaja_id = laenutaja_id EXCLUSIVE-LOCK NO-ERROR.
        END.    
    END.
                
END CLASS.