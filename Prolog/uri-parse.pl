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
atom_chars_wrapper(Userinfo, UserinfoL),
atom_chars_wrapper(Host, HostL),
atom_chars_wrapper(Port, PortL),
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



%!  definizione del lessico di una URI

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
slash_path_quer_frag(Path, Query, Fragment).

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

% da implementare: le specifiche aggiuntive del pdf
% the_uri_parse(Scheme,
%               Userinfo,
%               Host,
%               Port,
%               Path,
%               Query,
%               Fragment) -->
% scheme(Scheme),
% [':'],
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
scheme(Scheme) --> identificatore(Scheme).

authorithy(Userinfo, Host, Port) --> ['/'], ['/'],
userinfo(Userinfo), host(Host), port(Port).

userinfo([]) --> [].
userinfo(Userinfo) --> identificatore(Userinfo), ['@'].

% host da implementare
host(Host) --> identificatore_host(H), host_opt(T),
{append(H, T, Host)}.
host(Host) --> indirizzo_ip(Host).

host_opt([]) --> [].
host_opt(['.' | Host_opt]) --> ['.'], identificatore_host(H), host_opt(T),
{append(H, T, Host_opt)}.

port(['8', '0']) --> [].
port(Port) --> [':'], digit(Port).

slash_path_quer_frag(Path,
                     Query,
                     Fragment) -->
['/'],
path_quer_frag(Path, Query, Fragment).

slash_path_quer_frag([],
                     [],
                     []) -->
[].

opt_slash_path_quer_frag(Path,
                         Query,
                         Fragment) -->
slash,
path_quer_frag(Path, Query, Fragment).

slash --> [].
slash --> ['/'].

path_quer_frag(Path, Query, Fragment) -->
path(Path), query(Query), fragment(Fragment).

% sintassi speciale zos
% path_quer_frag(Path, Query, Fragment) -->
% zos_path(Path), query(Query), fragment(Fragment).

% magari da riguardare un attimo la ricorsione

path([])  --> [].
path(Path) --> identificatore(H), path_opt(T),
{append(H, T, Path)}.

path_opt([]) --> [].
path_opt([ '/' | Path_opt]) --> ['/'], identificatore(H), path_opt(T),
{append(H, T, Path_opt)}.

zos_path(Path) --> id44(Path), {length(Path, L), L =< 44}.
zos_path(Path) --> id44(H),
{length(H, L44),
 L44 =< 44},
['('], id8(T), [')'],
{length(T, L8),
 L8 =< 8,
 append(H, T, Path)}.

query([]) --> [].
query(Query) --> ['?'], caratteri_no_hashtag(Query).

fragment([]) --> [].
fragment(Fragment) --> ['#'], caratteri(Fragment).

mailto(Userinfo, []) --> userinfo(Userinfo).
mailto(Userinfo, Host) --> userinfo(Userinfo), ['@'], host(Host).

news(Host) --> host(Host).

% definizione di predicati di supporto

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
 append([N1], [N2], N12),
 append(N12, [N3], NNN)}.

id44([H | T]) --> [H], {controllo_alfa(H)},
id44_recursive(T).

id44_recursive([H | []]) --> [H], {controllo_alfanum(H)}.
id44_recursive([H | T]) --> [H], {controllo_alfanum(H)}, id44_recursive(T).
id44_recursive(['.' | T]) --> ['.'], id44_recursive(T).

id8([H | T]) --> [H], {controllo_alfa(H)},
id8_recursive(T).

id8_recursive([H | []]) --> [H], {controllo_alfanum(H)}.
id8_recursive([H | T]) --> [H], {controllo_alfanum(H)}, id8_recursive(T).

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

controllo_alfanum(Alfanum) :-
controllo_alfa(Alfanum).
controllo_alfanum(Alfanum) :-
controllo_digit(Alfanum).

controllo_carattere(Carattere) :-
Carattere \= '/',
Carattere \= '?',
Carattere \= '#',
Carattere \= '@',
Carattere \= ':'.

/*carattere(Char) :-
char_code(Char, C),
C < 48.

carattere(Char) :-
char_code(Char, C),
C > 57.
*/


% fine definizione di predicati di supporto
%!  %end of file -- progetto.pl
