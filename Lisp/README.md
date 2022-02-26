
# URI-parser - lisp

## Componenti del gruppo di gennaio

- 865993 Christian Dotti
- 866359 Adriano Colombo
- 866135 Alberto Ventafridda

Questo progetto è open source e disponibile su 
[github](https://github.com/robalb/uri_parser_project)
I test automatici con gli  URI su cui è stato testato il parser sono
consultabili al seguente link
https://github.com/robalb/uri_parser_project/blob/master/tests/tests_uri.py

Questo codice LISP implementa un Recursive-Descent-Parser per riconoscere la
grammatica URI descritta nella consegna.

La funzione uri-parse 
Se chiamata con un uri valido restituisce una struttura implementata tramite
liste.
Nel caso in cui un uri non sia valido viene sollevata un eccezione, con -
dove possibile - una descrizione dettagliata della causa.

Ai fini dell'implementazione, un carattere è considerato come
qualsiasi carattere stampabile dello standard ascii, ovvero qualsiasi
carattere nel range 0x20 - 0x7E
