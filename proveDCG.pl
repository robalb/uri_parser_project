/* -*- Mode: Prolog -*- */
% begin of file proveDCG.pl
%
%
sentence1(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).

 noun_phrase(np(D,N)) --> det(D), noun(N).

 verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).

 det(d(the)) --> [the].
 det(d(a)) --> [a].
 noun(n(bat)) --> [bat].
 noun(n(cat)) --> [cat].
 verb(v(eats)) --> [eats].



mini_parse(String, uri(A2, B2)) :-
    string_chars(String, StringL),
    parse(A, B, StringL, []),
    string_chars(A2, A),
    string_chars(B2, B).

parse(A, B) --> symbols_a(A), symbols_b(B), !.
symbols_a([a | A]) --> ['a'], symbols_a(A).
symbols_a([]) --> [].
symbols_b([b | B]) --> ['b'], symbols_b(B).
symbols_b([]) --> [].


% sentence2(A, Z) :-
%    symbols_a2(A, B), symbols_b2(B, Z).


/*symbols(end, S) --> [S].
symbols(sentence(Sim), S) --> [S], symbols(Sim, S).*/


% end of file -- proveDCG.pl
