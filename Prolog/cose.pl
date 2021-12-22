/* -*- Mode: Prolog -*- */
%  begin of file: cose.pl
%  file usato per capire come si potrebbe semplificare l'analisi
%  lessicale

base([H | []], Pred) --> [H],  {call(Pred, H)}.
base([H | T], Pred) --> [H], {call(Pred, H)}, base(T, Pred).

satisfy(Pred, C) -->
    char(C),
    { call(Pred, C) }.
not_a(Thing) :-
    Thing \= 'a'.

try(Not_a) --> base(Not_a, always).
always(_).
%  end of file -- cose.pl

