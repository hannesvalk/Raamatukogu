
/*------------------------------------------------------------------------
    File        : raamaturapStru.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Hannes
    Created     : Fri Jan 29 13:44:00 EET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

    DEFINE TEMP-TABLE traamaturaport {1}
        FIELD arv      AS INTEGER
        FIELD vaba     AS INTEGER
        FIELD pealkiri AS CHARACTER 
        FIELD autor    AS CHARACTER 
        FIELD ISBN     AS CHARACTER 
        FIELD asukoht  AS CHARACTER 
        FIELD kuupaev  AS DATE  
        FIELD tagasi   AS DATE  FORMAT '99/99/9999' 
        FIELD tahtaeg  AS DATE  FORMAT '99/99/9999' 
        FIELD kasutaja AS CHARACTER 
        .
