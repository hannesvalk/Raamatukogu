
/*------------------------------------------------------------------------
    File        : laenutusedStru.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Hannes
    Created     : Sat Jan 30 21:20:19 EET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE {1} TEMP-TABLE tlaenutused {2} 
    LIKE raamat.laenutused
    FIELD kasutaja AS CHARACTER
    FIELD pealkiri AS CHARACTER
    FIELD autor    AS CHARACTER
    FIELD ISBN     AS CHARACTER.
