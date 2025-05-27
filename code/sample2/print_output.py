from pyswip import Prolog

prolog = Prolog()
prolog.consult("logic.pl") 

print(list(prolog.query("prompt.")))
