/* -*- Mode: Prolog -*- */
%  begin of file: uri-parse.pl
%
% 865993 Christian Dotti
% 866359 Adriano Colombo
% 866135 Alberto Ventafridda

%%% Definizione delle interfacce, come richiesto nella consegna

uri_parse(URIString,
          uri(Scheme,
              Userinfo,
              Host,
              Port,
              Path,
              Query,
              Fragment)) :-
    string_chars(URIString, URIList),
    uri_parse_start(SchemeL,
                    UserinfoL,
                    HostL,
                    PortL,
                    PathL,
                    QueryL,
                    FragmentL,
                    URIList,
                    []),
    atom_chars_wrapper(Scheme, SchemeL),
    atom_chars_wrapper(Userinfo, UserinfoL),
    atom_chars_wrapper(Host, HostL),
    number_codes_wrapper(Port, PortL),
    atom_chars_wrapper(Path, PathL),
    atom_chars_wrapper(Query, QueryL),
    atom_chars_wrapper(Fragment, FragmentL),
    !.


uri_display(Uri) :-
    uri_display(Uri, user_output).
uri_display(uri(Scheme,
                Userinfo,
                Host,
                Port,
                Path,
                Query,
                Fragment), Stream) :-
    set_output(Stream),
    write('Scheme: '),
    writeln(Scheme),
    write('Userinfo: '),
    writeln(Userinfo),
    write('Host: '),
    writeln(Host),
    write('Port: '),
    writeln(Port),
    write('Path: '),
    writeln(Path),
    write('Query: '),
    writeln(Query),
    write('Fragment: '),
    writeln(Fragment).


% number_codes_wrapper/2
%
% Permette di avere come porta un numero e non un atomo
%
number_codes_wrapper(A, B) :-
    length(B, C),
    C is 0,
    A = [].
number_codes_wrapper(A, B) :-
    number_codes(A,B).


% atom_chars_wrapper/2
%
% permette di avere una lista vuota al posto di un atomo vuoto
%
atom_chars_wrapper(A, B) :-
    length(B, C),
    C is 0,
    A = [].
atom_chars_wrapper(A, B) :-
    atom_chars(A, B).


% matches_scheme/2
%
% Permette una comparazione case-insensitive tra una stringa e una lista
% di caratteri.
% Esempio:
% > matches_scheme("zos", ['z', 'O', 's']).
%   T
%
matches_scheme(String, List) :-
    string_codes(String, List1),
    string_chars(String1, List),
    string_codes(String1, List2),
    matches_scheme_ric(List1, List2).

matches_scheme_ric([], []).
matches_scheme_ric([H1 | T1], [H2 | T2]) :-
    H1 = H2,
    !,
    matches_scheme_ric(T1, T2).
matches_scheme_ric([H1 | T1], [H2 | T2]) :-
    H1 is H2 - 32,
    !,
    matches_scheme_ric(T1, T2).
matches_scheme_ric([H1 | T1], [H2 | T2]) :-
    H1 is H2 + 32,
    !,
    matches_scheme_ric(T1, T2).


%%% Dichiarazione delle DCG per il riconoscimento della grammatica descritta
%%% nella consegna.

uri_parse_start(Scheme, Userinfo, Host, ['8', '0'], [], [], []) -->
    scheme(Scheme),
    [':'],
    { matches_scheme("mailto", Scheme), ! },
    mailto(Userinfo, Host).

uri_parse_start(Scheme, [], Host, ['8', '0'], [], [], []) -->
    scheme(Scheme),
    [':'],
    { matches_scheme("news", Scheme), ! },
    news_host(Host).

uri_parse_start(Scheme, [], Host, ['8', '0'], [], [], []) -->
    scheme(Scheme),
    [':'],
    { matches_scheme("iptest", Scheme), ! },
    indirizzo_ip(Host).

uri_parse_start(Scheme, Userinfo, [], ['8', '0'], [], [], []) -->
    scheme(Scheme),
    [':'],
    { tel_fax(Tel_Fax),
      matches_scheme(Tel_Fax, Scheme), ! },
    tel_fax_userinfo(Userinfo).

uri_parse_start(Scheme, Userinfo, Host, Port, Path, Query, Fragment) -->
    scheme(Scheme),
    [':'],
    { matches_scheme("zos", Scheme) },
    authorithy(Userinfo, Host, Port),
    slash_zos_path(Path),
    query(Query),
    fragment(Fragment).

uri_parse_start(Scheme, [], [], ['8', '0'], Path, Query, Fragment) -->
    scheme(Scheme),
    [':'],
    { matches_scheme("zos", Scheme), ! },
    slash(_),
    zos_path_opt(Path),
    query(Query),
    fragment(Fragment).

uri_parse_start(Scheme, Userinfo, Host, Port, Path, Query, Fragment) -->
    scheme(Scheme),
    [':'],
    authorithy(Userinfo, Host, Port),
    slash_path(Path),
    query(Query),
    fragment(Fragment).

uri_parse_start(Scheme, [], [], ['8', '0'], Path, Query, Fragment) -->
    scheme(Scheme),
    [':'],
    slash(_),
    path(Path),
    query(Query),
    fragment(Fragment).


tel_fax("tel").
tel_fax("fax").


tel_fax_userinfo(A) -->
    identificatore(A).
tel_fax_userinfo([]) -->
    [].


mailto(Userinfo, []) -->
    identificatore(Userinfo).
mailto(Userinfo, Host) -->
    identificatore(Userinfo), ['@'], host(Host).
mailto([], []) -->
    [].


news_host(Host) -->
    host(Host).
news_host([]) -->
    [].


scheme(Scheme) -->
    identificatore(Scheme).


authorithy(Userinfo, Host, Port) -->
    ['/', '/'],
    userinfo(Userinfo),
    host(Host),
    port(Port).


userinfo(Userinfo) -->
    identificatore(Userinfo),
    ['@'].
userinfo([]) -->
    [].


host(Host) -->
    identificatore_host(H),
    host_opt(T),
    { append(H, T, Host) }.
host(Host) -->
    indirizzo_ip(Host).


host_opt(['.' | Host_opt]) -->
    ['.'],
    identificatore_host(H),
    host_opt(T),
    { append(H, T, Host_opt) }.
host_opt([]) -->
    [].


port(Port) -->
    [':'],
    one_or_more(Port, single_digit).
port(['8', '0']) -->
    [].


slash(['/']) -->
    ['/'].
slash([]) -->
    [].


slash_path(Path) -->
    ['/'],
    path(Path).
slash_path([]) -->
    ['/'].
slash_path([]) -->
    [].


path(Path) -->
    identificatore(X),
    path_opt(Y),
    slash(Z),
    {append(X, Y, XY)},
    {append(XY, Z, Path)}.
path([])  -->
    [].


path_opt([ '/' | Path_opt]) -->
    ['/'],
    identificatore(H),
    path_opt(T),
    { append(H, T, Path_opt) }.
path_opt([]) -->
    [].


slash_zos_path(Path) -->
    ['/'],
    zos_path(Path).
slash_zos_path([]) -->
    [].


zos_path_opt(Path) -->
    zos_path(Path).
zos_path_opt([]) -->
    [].


zos_path(Path) -->
    id44(Path),
    { last(Path, C), C \= '.', length(Path, L), L =< 44 }.
zos_path(Path) -->
    id44(H),
    { last(H, C), C \= '.',length(H, L44), L44 =< 44 },
    ['('],
    id8(T),
    [')'],
    {
        length(T, L8), L8 =< 8,
        append(['('], T, T1),
        append(T1, [')'], T2),
        append(H, T2, Path)
    }.


id44([H | T]) -->
    [H],
    { single_alphabet_letter(H) },
    one_or_more(T, single_id44_character).


id8([H | T]) -->
    [H],
    {single_alphabet_letter(H)},
    one_or_more(T, single_alphanum_letter).


query(Query) -->
    ['?'],
    one_or_more(Query, single_query_character).
query([]) -->
    [].


fragment(Fragment) -->
    ['#'],
    one_or_more(Fragment, single_character).
fragment([]) -->
    [].


identificatore(Chars) -->
    one_or_more(Chars, single_identifier_character).

identificatore_host(Chars) -->
    one_or_more(Chars, single_host_character).


%%% Definizione di predicati di supporto

one_or_more([H | T], Pred) -->
    [H],
    { call(Pred, H) },
    one_or_more(T, Pred).
one_or_more([H | []], Pred) -->
    [H],
    { call(Pred, H) }.


single_digit(Digit) :-
    char_code(Digit, D),
    D >= 48,
    D =< 57.


single_alphabet_letter(Char) :-
    char_code(Char, C),
    C >=65,
    C =< 90.
single_alphabet_letter(Char) :-
    char_code(Char, C),
    C >= 97,
    C =< 122.


single_alphanum_letter(Char) :-
    single_alphabet_letter(Char).
single_alphanum_letter(Char) :-
    single_digit(Char).


single_id44_character(Char) :-
    Char = '.'.
single_id44_character(Char) :-
    single_alphanum_letter(Char).


%%% Definizione di carattere, progressivamente restrittiva.
%%% Per semplicita, a differenza dell'rfc, definiamo come base senza restrizioni
%%% qualsiasi carattere stampabile dello standard ascii, ovvero qualsiasi
%%% carattere nel range 0x20 - 0x7E

single_character(Char) :-
    char_code(Char, C),
    C >= 32,
    C =< 126.

single_query_character(Char) :-
    Char \= '#',
    single_character(Char).

single_identifier_character(Char) :-
    Char \= '/',
    Char \= '?',
    Char \= '@',
    Char \= ':',
    single_query_character(Char).

single_host_character(Char) :-
    Char \= '.',
    single_identifier_character(Char).

%%% Le regole per il riconoscimento di un IPv4 sono ridondanti e non
%%% contribuiscono al funzionamento del programma, dal momento che non
%%% e' richiesto di differenziare in alcun modo tra un host e un IPv4.
%%% Sono tuttavia presenti nella grammatica della consegna, e per questo motivo
%%% Le abbiamo implementate e integrate nel programma.

indirizzo_ip(Ip) -->
    terzina(NNN1), {length(NNN1, 3)}, ['.'],
    terzina(NNN2), {length(NNN2, 3)}, ['.'],
    terzina(NNN3), {length(NNN3, 3)}, ['.'],
    terzina(NNN4), {length(NNN4, 3)},
    { append(NNN1 , ['.'], N1),
      append(NNN2, ['.'], N2),
      append(NNN3, ['.'], N3),
      append(N1,  N2, N12),
      append(N3, NNN4, N34),
      append(N12, N34, Ip) }.

terzina(Num) -->
    [N1], [N2], [N3],
    { single_digit(N1),
      single_digit(N2),
      single_digit(N3),
      append([N1], [N2], N1N2),
      append(N1N2, [N3], Num),
      check_range(Num) }.

check_range(Numstring) :-
    number_string(Num, Numstring),
    Num >= 0,
    Num =< 255.

%%% end of file -- uri-parse.pl
