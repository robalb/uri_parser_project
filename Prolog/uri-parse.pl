/* -*- Mode: Prolog -*- */
%  begin of file: progetto.pl

uri_parse(URIString,
          uri(Scheme,
              Userinfo,
              Host,
              Port,
              Path,
              Query,
              Fragment)) :-
string_chars(URIString, URIList),
the_uri_parse(SchemeL,
              UserinfoL,
              HostL,
              PortL,
              PathL,
              QueryL,
              FragmentL,
              URIList,
              []),
atom_chars_wrapper(Scheme, SchemeL),
/*analisi_uri(Scheme,
            UserinfoL,
            HostL,
            PortL,
            PathL,
            QueryL,
            FragmentL),*/
atom_chars_wrapper(Userinfo, UserinfoL),
atom_chars_wrapper(Host, HostL),
number_codes(Port, PortL), %serve per avere come porta un numero e non un atomo
atom_chars_wrapper(Path, PathL),
atom_chars_wrapper(Query, QueryL),
atom_chars_wrapper(Fragment, FragmentL),
!.

atom_chars_wrapper(A, B) :-
length(B, C),
C is 0,
A = [].

atom_chars_wrapper(A, B) :-
atom_chars(A, B).

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

%regola uri_analisis/7 che controlla che i "token" della grammatica
% delle uri riconosciuti dal parser corrispondano alla sintassi
% corrispondente a seconda del contenuto del campo "Scheme"
/*
analisi_uri(Scheme,
            _Userinfo,
            _Host,
            [],
            [],
            [],
            []) :-
!,
Scheme = 'mailto'.

analisi_uri(Scheme,
            _Userinfo,
            [],
            [],
            [],
            [],
            []) :-
!,
Scheme = 'tel'.

analisi_uri(Scheme,
            _Userinfo,
            [],
            [],
            [],
            [],
            []) :-
!,
Scheme = 'fax'.

analisi_uri(Scheme,
            _Userinfo,
            _Host,
            _Port,
            Path,
            _Query,
            _Fragment) :-
!,
Scheme = 'zos',
check_path(Path, []).

analisi_uri(_, _, _, _, _, _, _) :- !.
*/
/*implementazione sbagliata, che si appoggia ad analisi_uri
check_path --> [C], {controllo_alfanum(C)}, id44(1).
check_path --> [C], {controllo_alfanum(C)}, id44(1),
['(', B], {controllo_alfanum(B)}, id8(1), [')'].

id44(L) --> [C], {controllo_alfanum(C), L =< 43}.
id44(L) --> [C], {controllo_alfanum(C), L2 is L + 1}, id44(L2).
id44(L) --> ['.'], {L2 is L + 1}, id44(L2).

id8(L) --> [C], {controllo_alfanum(C), L =< 7}.
id8(L) --> [C], {controllo_alfanum(C), L2 is L + 1}, id8(L2).
*/
/* old implementation
id44_recursive([H | []]) --> [H], {controllo_alfanum(H)}.
id44_recursive([H | T]) --> [H], {controllo_alfanum(H)}, id44_recursive(T).
id44_recursive(['.' | T]) --> ['.'], id44_recursive(T).
*/
/* old implementation of id8
id8([H | T]) --> [H], {controllo_alfa(H)},
id8_recursive(T).

id8_recursive([H | []]) --> [H], {controllo_alfanum(H)}.
id8_recursive([H | T]) --> [H], {controllo_alfanum(H)}, id8_recursive(T).
*/

% Regole per il parsing

the_uri_zos_parse(Userinfo,
                  Host,
                  Port,
                  Path,
                  Query,
                  Fragment) -->
authorithy(Userinfo, Host, Port),
zos_path_query_frag(Path, Query, Fragment).

the_uri_parse(Scheme,
              Userinfo,
              Host,
              Port,
              Path,
              Query,
              Fragment) -->
{string_chars("zos", Scheme)}, Scheme,{!}, [':'], the_uri_zos_parse(Userinfo,
                                                    Host,
                                                    Port,
                                                    Path,
                                                    Query,
                                                    Fragment).

the_uri_parse(Scheme,
              Userinfo,
              Host,
              [],
              [],
              [],
              []) -->
{string_chars("mailto", Scheme)}, Scheme, {!}, [':'], mailto(Userinfo, Host).

the_uri_parse(Scheme,
              [],
              Host,
              [],
              [],
              [],
              []) -->
{string_chars("news", Scheme)}, Scheme, {!}, [':'], news(Host).

the_uri_parse(Scheme,
              Userinfo,
              [],
              [],
              [],
              [],
              []) -->
{tel_fax(Tel_Fax), string_chars(Tel_Fax, Scheme)},
Scheme, {!}, [':'], identificatore2(Userinfo).

the_uri_parse(Scheme,
              Userinfo,
              Host,
              Port,
              Path,
              Query,
              Fragment) -->
scheme(Scheme),
[':'],
authorithy(Userinfo, Host, Port),
path_query_frag(Path, Query, Fragment).

%regole generali


/* sembra non servire più
the_uri_parse(Scheme,
              [],
              [],
              [],
              Path,
              Query,
              Fragment) -->
scheme(Scheme),
[':'],
opt_slash_path_quer_frag(Path, Query, Fragment).
*/
% da implementare: le specifiche aggiuntive del pdf

% sintassi_speciali(Userinfo,
%             Host,
%             Port,
%             Path,
%             Query,
%             Fragment).
% sintassi speciali

% sintassi_speciali(Userinfo,
%             Host,
%             [],
%             [],
%             [],
%             []) -->
% mailto(Userinfo, Host).

% sintassi_speciali([],
%             Host,
%             [],
%             [],
%             [],
%             []) -->
% news(Host).

% sintassi_speciali(Userinfo,
%             Host,
%             Port,
%             Path,
%             Query,
%             Fragment) -->
% authorithy(Userinfo, Host, Port),
% path_quer_frag(Path, Query, Fragment).

% implementazione delle sotto-grammatiche
scheme(Scheme) --> identificatore2(Scheme).

authorithy(Userinfo, Host, Port) --> ['/'], ['/'],
userinfo(Userinfo), host(Host), port(Port).
authorithy([], [], []) --> [].

userinfo([]) --> [].
userinfo(Userinfo) --> identificatore2(Userinfo), ['@'].

% host da implementare
host(Host) --> identificatore_host2(H), host_opt(T),
{append(H, T, Host)}.
host(Host) --> indirizzo_ip(Host).

host_opt([]) --> [].
host_opt(['.' | Host_opt]) --> ['.'], identificatore_host2(H), host_opt(T),
{append(H, T, Host_opt)}.

port(['8', '0']) --> [].
port(Port) --> [':'], digit2(Port).

/* capire se vogliamo usarlo ancora
slash_path_quer_frag([],
                     [],
                     []) -->
[].
slash_path_quer_frag(Path,
                     Query,
                     Fragment) -->
['/'],
path_quer_frag(Path, Query, Fragment).
*/
/* sembra non sia più necessario
opt_slash_path_quer_frag(Path,
                         Query,
                         Fragment) -->
slash,
path_quer_frag(Path, Query, Fragment).

slash --> [].
slash --> ['/'].
*/
path_query_frag(Path, Query, Fragment) -->
['/'], path(Path), query(Query), fragment(Fragment).
path_query_frag([], [], []) --> [].


% sintassi speciale zos

zos_path_query_frag(Path, Query, Fragment) -->
['/'], zos_path(Path), query(Query), fragment(Fragment).
zos_path_query_frag([], [], []) --> [].


% path_quer_frag(Path, Query, Fragment) -->
% zos_path(Path), query(Query), fragment(Fragment).

% magari da riguardare un attimo la ricorsione
% ci ho riguardato, a me sembra corretta - A. C.

path(Path) --> identificatore2(H), path_opt(T),
{append(H, T, Path)}.
path([])  --> [].

path_opt([ '/' | Path_opt]) --> ['/'], identificatore2(H), path_opt(T),
{append(H, T, Path_opt)}.
path_opt([]) --> [].


zos_path(Path) --> id44_2(Path), {length(Path, L), L =< 44}.
zos_path(Path) --> id44_2(H), {length(H, L44), L44 =< 44},
['('], id8_2(T), [')'],
{length(T, L8), L8 =< 8,
 append(['('], T, T1),
 append(T1, [')'], T2),
 append(H, T2, Path)}.
%nel caso in cui si voglia accettare un zospath vuoto:
% zos_path([]) --> [].
% ANCHE SE DA QUELLO CHE C'é SCRITTO SUL FORUM NONSI DEVE FARE!

query(Query) --> ['?'], caratteri_no_hashtag2(Query).
query([]) --> [].

fragment(Fragment) --> ['#'], caratteri2(Fragment).
fragment([]) --> [].

mailto(Userinfo, []) --> identificatore2(Userinfo).
mailto(Userinfo, Host) --> identificatore2(Userinfo), ['@'], host(Host).
mailto([], []) --> [].

news(Host) --> host(Host).
news([]) --> [].

% definizione di predicati di supporto
tel_fax(Tel_Fax) :- Tel_Fax = "tel".
tel_fax(Tel_Fax) :- Tel_Fax = "fax".

% inizio implementazione semplificazione

base2([H | T], Pred) --> [H], {call(Pred, H)}, base2(T, Pred).
base2([H | []], Pred) --> [H], {call(Pred, H)}.

always(_).

digit2(Digit) --> base2(Digit, controllo_digit).

caratteri2(Caratteri) --> base2(Caratteri, always).

caratteri_no_hashtag2(No_hashtag) --> base2(No_hashtag, controllo_no_hashtag).

identificatore2(Identificatore) --> base2(Identificatore, controllo_carattere).

identificatore_host2(Host) --> base2(Host, controllo_ident_host).

id44_2([H | T]) --> [H], {controllo_alfa(H)}, base2(T, controllo_alfanum2).

id8_2([H | T]) --> [H], {controllo_alfa(H)}, base2(T, controllo_alfanum).

%fine implementazione semplificata!

% VECCHIA IMPLEMENTAZIONE!!!
/*
digit([H | []]) --> [H], {controllo_digit(H)}.
digit([H | T]) --> [H], {controllo_digit(H)}, digit(T).

caratteri([H | []]) --> [H].
caratteri([H | T]) --> [H], caratteri(T).

caratteri_no_hashtag([H | []]) --> [H], {H \= '#' }.
caratteri_no_hashtag([H | T]) --> [H], {H \= '#'}, caratteri_no_hashtag(T).

identificatore([H | []]) --> [H],{ controllo_carattere(H)}.
identificatore([H | T]) --> [H], { controllo_carattere(H)}, identificatore(T).

identificatore_host([H | []]) --> [H], {H \= '.', controllo_carattere(H)}.
identificatore_host([H | T]) --> [H], {H \= '.', controllo_carattere(H)},
identificatore_host(T).

id44([H | T]) --> [H], {controllo_alfa(H)},
id44_recursive(T).

id44_recursive([H | []]) --> [H], {controllo_alfanum(H)}.
id44_recursive([H | T]) --> [H], {controllo_alfanum(H)}, id44_recursive(T).
id44_recursive(['.' | T]) --> ['.'], id44_recursive(T).

id8([H | T]) --> [H], {controllo_alfa(H)},
id8_recursive(T).

id8_recursive([H | []]) --> [H], {controllo_alfanum(H)}.
id8_recursive([H | T]) --> [H], {controllo_alfanum(H)}, id8_recursive(T).
*/
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
