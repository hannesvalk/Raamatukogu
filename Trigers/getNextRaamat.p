TRIGGER PROCEDURE FOR CREATE OF raamat.raamatud.

DEFINE BUFFER braamatud FOR raamatud.

FIND LAST braamatud NO-LOCK NO-ERROR.
IF AVAILABLE braamatud THEN 
    raamat.raamatud.id = braamatud.id + 1.
ELSE 
    raamat.raamatud.id = 1.
