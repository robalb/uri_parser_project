/* -*- Mode: Prolog -*- */
%!  %begin file: progetto.pl

uri_parse(URIString,
          uri(Scheme,
              Userinfo,
              Host,
              Port,
              Path,
              Query,
              Fragment)) :-


uri_display(URIString).
uri_display(URIString, Text).

%!  definizione del lessico di una URI



%!  %Automa che riconosce le URI

initial(vuoto).
final(uri).

delta(vuoto, scheme, authority).
delta(authority, author, authority).
delta(authority, rest, uri).

accept([I | Is], S) :-
    delta(S, I, N),
    accept(Is, N).
accept([], Q) :- final(Q).

%!  %end of file -- progetto.pl
