TRIGGER PROCEDURE FOR CREATE OF kasutajad.

DEFINE BUFFER bkasutajad FOR kasutajad.

FIND LAST bkasutajad NO-LOCK NO-ERROR.
IF AVAILABLE bkasutajad THEN 
    raamat.kasutajad.id = bkasutajad.id + 1.
ELSE 
    raamat.kasutajad.id = 1.
