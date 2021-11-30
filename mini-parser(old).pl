%  Questa è una vecchia versione del programma, che
%  invece di considerare StringL come una lista di caratteri la
%  considera come una lista di codici, che corrispondono ai caratteri
%  utilizzando il seguente predicato:
%  string_codes/2 (stringa, list)
%  Converte una stringa in una lista di numeri, corrispondenti ai
%  caratteri e viceversa


mini_parse(String, A, B) :-
    string_codes(String, StringL),
    parse(StringL, A, B).

parse(StringL, A, B) :-
    parse_a(StringL, [], A, _As, Rest),
    parse_b(Rest, [], B, _Bs, _RestB).


parse_a([Head | Tail], AsSoFar, A, As, Rest) :-
    char_code('a', Head),
    !,
    parse_a(Tail, [Head | AsSoFar], A, As, Rest).
parse_a([Head | Tail], AsSoFar, A, As, [Head | Tail]) :-
    char_code('b', Head),
    !,
    reverse(AsSoFar, As),
    string_chars(A, As).

parse_a([Head | _Tail], _BsSoFar, _A, _As, _Rest) :-
    !,
    char_code(Err, Head),
    write(Err),
    writeln(" Is not 'a' or 'b', therefore it is not a valid string"),
    fail.


parse_b([Head | Tail], BsSoFar, B, Bs, Rest) :-
    char_code('b', Head),
    !,
    parse_b(Tail, [Head | BsSoFar], B, Bs, Rest).
parse_b([], BsSoFar, B, Bs, []) :-
    !,
    reverse(BsSoFar, Bs),
    string_chars(B, Bs).
parse_b([Head | _Tail], _BsSoFar, _B, _Bs, _Rest) :-
    !,
    char_code(Err, Head),
    write(Err),
    writeln(" Is not 'b', therefore it is not a valid string"),
    fail.
