/* -*- Mode: Prolog -*- */
%  begin of file mini-parser.pl

%  In questo programma viene eseguito il parsing di
%  stringhe che rispettano la sintassi dettata
%  dall'espressione regolare: a*bb*
%  Questo esercizio l'ho fatto per analizzare il
%  problema del parsing su un' E.R. molto semplice,
%  in modo da capirlo meglio.
%  Per risolverlo ho usato la teoria sul PPT che
%  vi ho mandato su Discord.


%  Metodi di Prolog utilizzati:
%
%
%  string_chars/2 (stringa, lista_di_caratteri)
%  Permette di convertire una stringa in una
%  lista di caratteri e viceversa

%  char_code/2 (carattere(atomo di lunghezza 1), intero)
%  Permette di convertire un carattere nell'intero
%  corrispondente e viceversa

%  reverse/2 (lista1, lista2)
%  Permette di invertire l'ordine degli elementi di
%  lista1 in lista2 e viceversa


mini_parse(String, A, B) :-
    string_chars(String, StringL),
    parse(StringL, A, B).
%  Qui vengono definiti e messo l'ordine nel quale gli stati dell'automa
%  devono susseguirsi.
%  In questo caso lo stato a è iniziale, b è finale e b viene dopo a.
%  Notare come parse_b ha in input Rest, che sarebbe la stringa che
%  rimane una volta usciti dallo stato a
%  A e B sono i termini che contengono le sotto-espressioni regolari
%  riconosiute, in questo caso: a* e bb*

parse(StringL, A, B) :-
    parse_a(StringL, [], A, _As, Rest),
    parse_b(Rest, [], B, _Bs, _RestB).


parse_a([Head | Tail], AsSoFar, A, As, Rest) :-
    'a' = Head,
    !,
    parse_a(Tail, [Head | AsSoFar], A, As, Rest).
parse_a([Head | Tail], AsSoFar, A, As, [Head | Tail]) :-
    'b' = Head,
    !,
    reverse(AsSoFar, As),
    string_chars(A, As).
parse_a([Head | _Tail], _BsSoFar, _A, _As, _Rest) :-
    !,
    write(Head),
    writeln(" Is not 'a' or 'b', therefore it is not a valid string"),
    fail.


parse_b([Head | Tail], BsSoFar, B, Bs, Rest) :-
    'b' = Head,
    !,
    parse_b(Tail, [Head | BsSoFar], B, Bs, Rest).
parse_b([], BsSoFar, B, Bs, []) :-
    !,
    reverse(BsSoFar, Bs),
    string_chars(B, Bs).

parse_b([Head | _Tail], _BsSoFar, _B, _Bs, _Rest) :-
    !,
    write(Head),
    writeln(" Is not 'b', therefore it is not a valid string"),
    fail.

%  % end of file -- mini-parser.pl
