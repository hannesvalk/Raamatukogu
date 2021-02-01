TRIGGER PROCEDURE FOR CREATE OF laenutused.

DEFINE BUFFER blaenutused FOR laenutused.

FIND LAST blaenutused NO-LOCK NO-ERROR.
IF AVAILABLE blaenutused THEN
    raamat.laenutused.id = blaenutused.id + 1.
ELSE    
    raamat.laenutused.id = 1.
