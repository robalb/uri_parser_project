import unittest
from .interfaces import PrologParser, UrllibParser, MalformedException

__unittest = True

class Tests(unittest.TestCase):
    """Basic tests for the subset of rfc3986 described in the assignment
    https://elearning.unimib.it/pluginfile.php/1141330/mod_resource/content/13/20220115%20LP%20E1P%20URI%20v2.pdf
    https://datatracker.ietf.org/doc/html/rfc3986
    most of the tests have been ported from:
    https://github.com/sporkmonger/addressable/blob/master/spec/addressable/uri_spec.rb
    """
    @classmethod
    def setUpClass(self):
        #TODO: make this parametizable
        self.parser = PrologParser()

    def test_fail_parse(self):
        tests = [
            #https://elearning.unimib.it/mod/forum/discuss.php?d=189837
            'mailto://foo/bar?q',
            #https://elearning.unimib.it/mod/forum/discuss.php?d=189837#p306737
            'zos:/',
            #https://elearning.unimib.it/mod/forum/discuss.php?d=189837#p307681
            'zos://you@themainframe.bigiron.fe/hlq.source.fortran(svd)/some/more/path',
            #Telegram:
            #SCHEME
            's@:', #invalid characters
            's/:',
            's?:',
            's#:',
            's::',
            's c:',
            '1s:', #scheme must start with a letter
            '§s:',
            '+s:',
            ':',   #scheme can't be empty
            #HOST
            's://', #empty host
            's://host.', #invalid dot position
            's://.host',
            's://ho..st',
            's://ho?st', #invalid characters ( ?,# must be preceeded by /)
            's://ho#st',
            's://ho st',
            #USERINFO
            's://@host', #empty userinfo
            's://userinfo@', #empty host
            's://u@i@host', #invalid characters
            's://u/i@host',
            's://u?i@host',
            's://u#i@host',
            's://u i@host',
            's://u:i@host', #also an invalid character, although allowed by the rfc
            #PORT
            's://host:a', #port must be a number
            's://:42', #host missing
            's://host:', #value missing
            #PATH
            's://host//', #path can't start with slash
            's://host/path//',
            's://host/path/', #path can't end with slash
            's://host/path/?asd',
            's://host/path/#asd',
            's://host/path/path/',
            's:path/path', #path must start with slash, even without authority
            's:/path/',
            's:/path//path',
            's:host/pa@th', #invalid characters
            's:host/pa:th',
            's:host/pa?th',
            's:host/pa#th',
            's:host/pa th',
            #QUERY
            's:?query', #a slash must preceed the query
            's:/?quer y', #invalid characters
            's://host/path?', #query cant be empty
            #FRAGMENT
            's:#fragment', # a slash must preceed the fragment
            's://host/path#', #fragment cant be empty
            's://host/path#fragm ent', #invalid characters

            #MAILTO
            'mailto:userinfo@host?query',
            'mailto:/path',
            'mailto:/?query',
            'mailto:/#fragm',

            #NEWS
            'news:host.com?query',
            'news://host.com',
            'news:user@host.com',
            'news:@host.com',
            'news:/path',
            'news:/?query',
            'news:/#query',

            #TEL/FAX
            'fax://host.com',
            'tel://host.com',
            'tel:/path',
            'tel:/?query',
            'tel:/#fragm',
            'tel:us/er', #invalid characters
            'tel:us?er',
            'tel:us#er',
            'tel:us@er',
            'tel:us:er',

            #ZOS
            'zos:/',
            'zos:/.abc', #id44 must start with a letter
            'zos:/1abc',
            'zos:/abc.', #id44 can't end wit a dot
            ('zos:/' + 'a' * 46), #id4 must be max 44 chars long
            #'zos:/ab..c', #technically not allowed according to the real IBM specs
            'zos:/id44(8)', #id8 must start with a letter
            'zos:/id44(asd.asd)' #id8 can only contain letters and numbers
            'zos:/id44(aaaabbbbc)' #id8 must be max 8 characters long
            'zos:/id44(',
            'zos:/id44)',
            'zos:/id44()',
            'zos://host:43/id44()?query',
            'zos://host:43/id44(?query',
            'zos://host:43/id44)?query',

        ]
        for uri in tests:
            with self.subTest(uri=uri):
                with self.assertRaises(MalformedException):
                    self.parser.parse(uri)

    def test_parsed_values(self):
        tests = {
            #assignment pdf, page 4/6
            'http://disco.unimib.it': {
                'scheme': 'http',
                'userinfo': None,
                'host': 'disco.unimib.it',
                'port': 80,
                'path': None,
                'query': None,
                'fragment': None
            },
            #simple uri, common use case
            'https://disco.unimib.it:42': {
                'scheme': 'https',
                'userinfo': None,
                'host': 'disco.unimib.it',
                'port': 42,
                'path': None,
                'query': None,
                'fragment': None
            },
            #complete uri, common use case
            'http://user@disco.unimib.it/moodle/login?auth=asd&q=1#12': {
                'scheme': 'http',
                'userinfo': 'user',
                'host': 'disco.unimib.it',
                'port': 80,
                'path': 'moodle/login',
                'query': 'auth=asd&q=1',
                'fragment': '12'
            },
            #SCHEME
            's://host': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path': None,
                'query': None,
                'fragment': None
            },
            's+://host': {
                'scheme': 's+',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path': None,
                'query': None,
                'fragment': None
            },
            's.://host': {
                'scheme': 's.',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path': None,
                'query': None,
                'fragment': None
            },
            's-://host': {
                'scheme': 's-',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path': None,
                'query': None,
                'fragment': None
            },
            's1://host': {
                'scheme': 's1',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path': None,
                'query': None,
                'fragment': None
            },
            #HOST
            's://ho.st.asd': {
                'scheme': 's',
                'userinfo': None,
                'host': 'ho.st.asd',
                'port': 80,
                'path': None,
                'query': None,
                'fragment': None
            },
            's://127.0.0.1': {
                'scheme': 's',
                'userinfo': None,
                'host': '127.0.0.1',
                'port': 80,
                'path': None,
                'query': None,
                'fragment': None
            },
            # the assignment states that ip numbers must be between 0 and 255.
            # however, there are no restrictions on numeric values in hostnames.
            # therefore this should parse correctly.
            # Note: this is also the behavior of real URLs. EX: this https://9292.nl/ is
            # a working url
            's://127.0.0.999': {
                'scheme': 's',
                'userinfo': None,
                'host': '127.0.0.999',
                'port': 80,
                'path': None,
                'query': None,
                'fragment': None
            },
            #USERINFO
            's://user@host': {
                'scheme': 's',
                'userinfo': 'user',
                'host': 'host',
                'port': 80,
                'path': None,
                'query': None,
                'fragment': None
            },
            's://us+-_%20er@host': {
                'scheme': 's',
                'userinfo': 'us+-_%20er',
                'host': 'host',
                'port': 80,
                'path': None,
                'query': None,
                'fragment': None
            },
            #PORT
            's://host:42': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 42,
                'path': None,
                'query': None,
                'fragment': None
            },
            #PATH
            's:/pa.th': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path': 'pa.th',
                'query': None,
                'fragment': None
            },
            's://host/path': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path': 'path',
                'query': None,
                'fragment': None
            },
            's://host/p.ath%20path+-': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path': 'p.ath%20path+-',
                'query': None,
                'fragment': None
            },
            #QUERY
            #common use case
            's:/?q=12&rr=[]': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': 'q=12&rr=[]',
                'fragment': None
            },
            's:/?quer%20+-_y': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': 'quer%20+-_y',
                'fragment': None
            },
            's:/?query/?@:.': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': 'query/?@:.',
                'fragment': None
            },
            's://host/?query?@:.': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path':None,
                'query': 'query?@:.',
                'fragment': None
            },
            's://host/?query?#fragment': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path':None,
                'query': 'query?',
                'fragment': 'fragment'
            },
            #QUERY-FRAGMENT edge cases
            's://host/?quer#y#fragment': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path':None,
                'query': 'quer',
                'fragment': 'y#fragment'
            },
            #FRAGMENT
            #common use case
            's://host/?query#12': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path':None,
                'query': 'query',
                'fragment': '12'
            },
            's://host/?query#fragment': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path':None,
                'query': 'query',
                'fragment': 'fragment'
            },
            's://host/?query#fragme?#@:nt': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path':None,
                'query': 'query',
                'fragment': 'fragme?#@:nt'
            },
            's:/#fragment': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': 'fragment'
            },
            's:/#fragme?#@:nt': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': 'fragme?#@:nt'
            },

            #MAILTO
            'mailto:': {
                'scheme': 'mailto',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'mailto:asdasd': {
                'scheme': 'mailto',
                'userinfo': 'asdasd',
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'mailto:user@host': {
                'scheme': 'mailto',
                'userinfo': 'user',
                'host': 'host',
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },

            #NEWS
            'news:': {
                'scheme': 'news',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'news:host': {
                'scheme': 'news',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'news:127.0.0.1': {
                'scheme': 'news',
                'userinfo': None,
                'host': '127.0.0.1',
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },

            #TEL/FAX
            'tel:': {
                'scheme': 'tel',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'fax:': {
                'scheme': 'fax',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'fax:+39-327-9856-123': {
                'scheme': 'fax',
                'userinfo': '+39-327-9856-123',
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'tel:+393279856123': {
                'scheme': 'tel',
                'userinfo': '+393279856123',
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },

            #ZOS
            'zos:': {
                'scheme': 'zos',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },
            #TODO

        }
        for uri, expected in tests.items():
            with self.subTest(uri=uri, expected=expected):
                self.assertDictEqual(
                    self.parser.parse(uri),
                    expected,
                    f"parsed INCORRECTLY: {uri}"
                )
