


type ttype = TTINT | TTPTR of ttype | TTNULL

type vdeclt = VART of string * ttype * int | FUNT of string * (ttype list) * ttype
