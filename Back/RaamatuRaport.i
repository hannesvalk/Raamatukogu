
/*------------------------------------------------------------------------
    File        : RaamatuRaport.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Hannes
    Created     : Mon Feb 01 13:07:08 EET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

            arv=0.
            vaba=0.
            tagasi=DATE(12,31,2999).
            FOR EACH blaenutused WHERE blaenutused.raamatu_id = braamatud.id NO-LOCK:
                ACCUMULATE blaenutused.ID (COUNT).
                arv = (ACCUM COUNT blaenutused.ID).
            END.
            FOR EACH blaenutused WHERE blaenutused.raamatu_id = braamatud.id AND blaenutused.laenutaja_id = -1 NO-LOCK:
                ACCUMULATE blaenutused.ID (COUNT).
                vaba = (ACCUM COUNT blaenutused.ID).
            END.
            FOR EACH blaenutused WHERE blaenutused.raamatu_id = braamatud.id AND blaenutused.laenutaja_id > -1 NO-LOCK:
                ACCUMULATE blaenutused.tahtaeg (MIN).
                tagasi = (ACCUM MIN blaenutused.tahtaeg).
            END.
            CREATE traamaturaport.
            ASSIGN traamaturaport.arv      = arv
                   traamaturaport.vaba     = vaba
                   traamaturaport.pealkiri = braamatud.pealkiri
                   traamaturaport.autor    = braamatud.autor
                   traamaturaport.ISBN     = braamatud.ISBN
                   traamaturaport.asukoht  = braamatud.asukoht
                   traamaturaport.kuupaev  = braamatud.kuupaev
                    .
            tahtaeg = (NEW back.raamat()):GetTahtaeg(INPUT braamatud.id).
            traamaturaport.tahtaeg = TODAY + tahtaeg. 
                    
            IF tagasi < DATE(12,31,2999) THEN 
                   traamaturaport.tagasi   = tagasi.         
            