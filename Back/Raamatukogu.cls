 
 /*------------------------------------------------------------------------
    File        : Raamatukogu
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Hannes
    Created     : Thu Jan 28 20:31:26 EET 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Back.Raamatukogu: 
    
    DEFINE PROTECTED TEMP-TABLE traamatud   LIKE raamat.raamatud.
    {back/laenutusedStru.i PROTECTED}

    DEFINE PROTECTED BUFFER braamatud   FOR raamat.raamatud.
    DEFINE PROTECTED BUFFER blaenutused FOR raamat.laenutused.
    DEFINE PROTECTED BUFFER bkasutajad  FOR raamat.kasutajad.
    DEFINE PROTECTED BUFFER bsetup      FOR raamat.setup.

    DEFINE PROTECTED VARIABLE clLaenutus AS back.laenutus.
    DEFINE PROTECTED VARIABLE clRaamat   AS back.raamat.

    CONSTRUCTOR Raamatukogu():
        
    END CONSTRUCTOR.

END CLASS.