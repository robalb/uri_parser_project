
TODO
capire come utilizzare in maniera pratica:
sbcl sembra restituire sempre le informazioni legali in stdout, prima di write legittimi.
Inoltre restituisce iversi warning, anche se non è un problema perchè sono diretti su stderr

    sbcl --load uri_parse.lisp --eval '(uri-display (uri-parse "asd://asd@as.it:60"))' --non-interactive 2>/dev/null
