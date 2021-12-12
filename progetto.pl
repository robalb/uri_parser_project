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
schema(Scheme),
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


%!  %end of file -- progetto.pl
