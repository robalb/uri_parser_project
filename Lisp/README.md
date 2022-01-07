
TODO
capire come utilizzare in maniera pratica:
sbcl sembra restituire sempre le informazioni legali in stdout, prima di write legittimi.
Inoltre restituisce iversi warning, anche se non è un problema perchè sono diretti su stderr

    sbcl --load uri-parse.lisp --eval '(uri-display (uri-parse "asd://asd@as.it:60"))' --non-interactive 2>/dev/null

    cat uri-parse.lisp | cut -c \1-80 > uri-parse.cut.lisp

https://softwareengineering.stackexchange.com/questions/314898/in-which-order-should-lisp-functions-be-defined

https://lisp-lang.org/style-guide/
