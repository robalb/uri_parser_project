/* -*- Mode: Prolog -*- */
%  begin of file: progetto.pl


%%% uri_parse/2
%%% uri_parse(URIString, uri)
%%%

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


%%% number_codes_wrapper
%%%
%%% Serve per avere come porta un numero e non un atomo

number_codes_wrapper(A, B) :-
    length(B, C),
    C is 0,
    A = [].
number_codes_wrapper(A, B) :-
    number_codes(A,B).


%%%
%%%

atom_chars_wrapper(A, B) :-
    length(B, C),
    C is 0,
    A = [].
atom_chars_wrapper(A, B) :-
    atom_chars(A, B).


%%%
%%%

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
    writeln(Fragment),
    close(Stream).


%%%
%%%

uri_parse_start(Scheme, Userinfo, Host, [], [], [], []) -->
    { string_chars("mailto", Scheme) },
    Scheme,
    { ! },
    [':'],
    mailto(Userinfo, Host).

uri_parse_start(Scheme, [], Host, [], [], [], []) -->
    { string_chars("news", Scheme) },
    Scheme,
    { ! },
    [':'],
    news_host(Host).

uri_parse_start(Scheme, Userinfo, [], [], [], [], []) -->
    { tel_fax(Tel_Fax), string_chars(Tel_Fax, Scheme) },
    Scheme,
    { ! },
    [':'],
    tel_fax_userinfo(Userinfo).

uri_parse_start(Scheme, Userinfo, Host, Port, Path, Query, Fragment) -->
    { string_chars("zos", Scheme) },
    Scheme,
    { ! },
    [':'],
    authorithy(Userinfo, Host, Port),
    zos_path_query_frag(Path, Query, Fragment).

uri_parse_start(Scheme, Userinfo, Host, Port, Path, Query, Fragment) -->
    scheme(Scheme),
    [':'],
    authorithy(Userinfo, Host, Port),
    path_query_frag(Path, Query, Fragment).


%%%
%%% regole tel/fax

tel_fax(Tel_Fax) :- Tel_Fax = "tel".
tel_fax(Tel_Fax) :- Tel_Fax = "fax".

tel_fax_userinfo(A) -->
    identificatore2(A).
tel_fax_userinfo([]) -->
    [].


%%%
%%% regole mailto

mailto(Userinfo, []) -->
    identificatore2(Userinfo).
mailto(Userinfo, Host) -->
    identificatore2(Userinfo), ['@'], host(Host).
mailto([], []) -->
    [].


%%%
%%% regole news

news_host(Host) -->
    host(Host).
news_host([]) -->
    [].


%%%
%%% regole generali e zos

scheme(Scheme) -->
    identificatore2(Scheme).


authorithy(Userinfo, Host, Port) --> 
    ['/', '/'],
    userinfo(Userinfo),
    host(Host),
    port(Port).
authorithy([], [], ['8', '0']) -->
    [].


userinfo(Userinfo) -->
    identificatore2(Userinfo),
    ['@'].
userinfo([]) -->
    [].


host(Host) -->
    identificatore_host2(H),
    host_opt(T),
    {append(H, T, Host)}.
host(Host) -->
    indirizzo_ip(Host).

host_opt(['.' | Host_opt]) -->
    ['.'],
    identificatore_host2(H),
    host_opt(T),
    {append(H, T, Host_opt)}.
host_opt([]) -->
    [].


port(Port) -->
    [':'],
    digit2(Port).
port(['8', '0']) -->
    [].


path_query_frag(Path, Query, Fragment) -->
    ['/'],
    path(Path),
    query(Query),
    fragment(Fragment).
path_query_frag([], [], []) -->
    [].


zos_path_query_frag(Path, Query, Fragment) -->
    ['/'],
    zos_path(Path),
    query(Query),
    fragment(Fragment).
zos_path_query_frag([], [], []) -->
    [].


path(Path) -->
    identificatore2(H),
    path_opt(T),
    {append(H, T, Path)}.
path([])  -->
    [].

path_opt([ '/' | Path_opt]) -->
    ['/'],
    identificatore2(H),
    path_opt(T),
    {append(H, T, Path_opt)}.
path_opt([]) -->
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
    {controllo_alfa(H)},
    base2(T, controllo_alfanum2).

id8([H | T]) -->
    [H],
    {controllo_alfa(H)},
    base2(T, controllo_alfanum).


query(Query) -->
    ['?'],
    caratteri_query(Query).
query([]) -->
    [].


fragment(Fragment) -->
    ['#'],
    caratteri(Fragment).
fragment([]) -->
    [].


% definizione di predicati di supporto
% TODO pulire e riordinare

base2([H | T], Pred) --> [H], {call(Pred, H)}, base2(T, Pred).
base2([H | []], Pred) --> [H], {call(Pred, H)}.

always(_).

digit2(Digit) --> base2(Digit, controllo_digit).

caratteri(Caratteri) --> base2(Caratteri, always).

caratteri_query(No_hashtag) --> base2(No_hashtag, controllo_no_hashtag).

identificatore2(Identificatore) --> base2(Identificatore, controllo_carattere).

identificatore_host2(Host) --> base2(Host, controllo_ident_host).

indirizzo_ip(Ip) --> terzina(NNN1), {length(NNN1, 3)}, ['.'],
terzina(NNN2), {length(NNN2, 3)}, ['.'],
terzina(NNN3), {length(NNN3, 3)}, ['.'],
terzina(NNN4), {length(NNN4, 3)},
{append(NNN1 , ['.'], N1),
 append(NNN2, ['.'], N2),
 append(NNN3, ['.'], N3),
 append(N1,  N2, N12),
 append(N3, NNN4, N34),
 append(N12, N34, Ip)}.

terzina(NNN) --> [N1], [N2], [N3],
{controllo_digit(N1),
 controllo_digit(N2),
 controllo_digit(N3),
 controllo_terzina(N1, N2, N3),
 append([N1], [N2], N12),
 append(N12, [N3], NNN)}.

controllo_terzina(N1, N2, N3) :-
N is N1 * N2 * N3,
N >= 0,
N =< 255.

controllo_digit(Digit) :-
char_code(Digit, D),
D >= 48,
D =< 57.

controllo_alfa(Char) :-
char_code(Char, C),
C >=65,
C =< 90.
controllo_alfa(Char) :-
char_code(Char, C),
C >= 97,
C =< 122.

controllo_no_hashtag(C) :- C \= '#'.

controllo_alfanum(Alfanum) :-
controllo_alfa(Alfanum).
controllo_alfanum(Alfanum) :-
controllo_digit(Alfanum).

controllo_alfanum2(Alfanum) :-
Alfanum = '.'.
controllo_alfanum2(Alfanum) :-
controllo_alfanum(Alfanum).

controllo_carattere(Carattere) :-
Carattere \= '/',
Carattere \= '?',
Carattere \= '#',
Carattere \= '@',
Carattere \= ':'.

controllo_ident_host(C) :-
C \= '.',
controllo_carattere(C).

% fine definizione di predicati di supporto
%!  %end of file -- progetto.pl
