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
string_chars(Scheme, SchemeL),
string_chars(Userinfo, UserinfoL),
string_chars(Host, HostL),
string_chars(Port, PortL),
string_chars(Path, PathL),
string_chars(Query, QueryL),
string_chars(Fragment, FragmentL).


uri_display(URIString).
uri_display(URIString, Text).

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
authority(Userinfo, Host, Port),
slash_path_quer_frag(Path, Query, Fragment).

the_uri_parse(Scheme,
              [],
              [],
              [],
              Path,
              Query,
              Fragment) -->
schema(Scheme),
[':'],
opt_slash_path_quer_frag(Path, Query, Fragment).

% da implementare: le specifiche aggiuntive del pdf
/*the_uri_parse(Scheme,
              Userinfo,
              Host,
              Port,
              Path,
              Query,
              Fragment) -->
schema(Scheme),
[':'],
cose_strane(Userinfo,
            Host,
            Port,
            Path,
            Query,
            Fragment).*/

% implementazione delle sotto-grammatiche
scheme(Scheme) --> identificatore(Scheme).

authorithy(Userinfo, Host, Port) --> ['/'], ['/'],
userinfo(Userinfo), host(Host), port(Port).

userinfo([]) --> [].
userinfo(Userinfo) --> identificatore(Userinfo), ['@'].

% host da implementare
host(['H']) --> ['H'].

port([]) --> [].
port(Port) --> digit(Port).

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

% magari da riguardare un attimo la ricorsione

path(Path) --> identificatore(H), path_opt(T),
{append(H, T, Path)}.

path_opt([]) --> [].
path_opt([/| Path_opt]) --> ['/'], identificatore(H), path_opt(T),
{append(H, T, Path_opt)}.

% definizione di predicati di supporto

digit([H | []]) --> [H], {controllo_digit(H)}.
digit([H | T]) --> [H], {controllo_digit(H)}, digit(T).

identificatore([H | []]) --> [H],{ controllo_carattere(H)}.
identificatore([H | T]) --> [H], { controllo_carattere(H)}, identificatore(T).

controllo_digit(Digit) :-
char_code(Digit, D),
D >= 48,
D =< 57.

controllo_carattere(Carattere) :-
Carattere \= '.',
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
