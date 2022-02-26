"""Language independent Unit tests for the simplified uri-parser

"""
import unittest
from .interfaces import PrologParser, LispParser, MalformedException

__unittest = True

class Tests(unittest.TestCase):
    @classmethod
    def setUpClass(self):
        self.lisp = LispParser()
        self.prolog = PrologParser()

    def test_LISP_failing_uris(self):
      self.failing_uris(self.lisp)

    def test_PROLOG_failing_uris(self):
      self.failing_uris(self.prolog)

    def test_LISP_valid_uris(self):
      self.valid_uris(self.lisp)

    def test_PROLOG_valid_uris(self):
      self.valid_uris(self.prolog)

    def failing_uris(self, parser):
      """ Invalid uris, that the parser should fail to parse"""
      tests = [
          #https://elearning.unimib.it/mod/forum/discuss.php?d=189837
          'mailto://foo/bar?q',
          #https://elearning.unimib.it/mod/forum/discuss.php?d=189837#p307681
          'zos://you@themainframe.bigiron.fe/hlq.source.fortran(svd)/some/more/path',
          #SCHEME
          's@:', #invalid characters
          's/:',
          's?:',
          's#:',
          's::',
          ':',   #scheme can't be empty
          #HOST
          's://', #empty host
          's://host.', #invalid dot position
          's://.host',
          's://ho..st',
          #USERINFO
          's://@host', #empty userinfo
          's://userinfo@', #empty host
          's://u@i@host', #invalid characters
          's://u/i@host',
          's://u:i@host', #also an invalid character, although allowed by rfc
          #PORT
          's://host:a', #port must be a number
          's://:42', #host missing
          's://host:', #value missing
          #PATH
          's://host:99path',
          's://host//', #path can't have empty identifier between / /
          's://host//path',
          's://host/path//',
          's:/path//path',
          's:host/pa@th', #invalid characters
          's:host/pa:th',
          #QUERY
          's://host/path?', #query cant be empty
          's:/?',
          's:?',
          #FRAGMENT
          's://host/path#', #fragment cant be empty
          's:/#',
          's:#',
          #'s://host/path#fragm ent', #invalid characters

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
          'zos:/.abc', #id44 must start with a letter
          'zos:/1abc',
          'zos:/abc.', #id44 can't end wit a dot
          ('zos:/' + 'a' * 46), #id4 must be max 44 chars long
          #'zos:/ab..c', #technically not allowed according to the IBM specs
          'zos:/id44(8)', #id8 must start with a letter
          'zos:/id44(asd.asd)' #id8 can only contain letters and numbers
          'zos:/id44(aaaabbbbc)' #id8 must be max 8 characters long
          'zos:/id44(',
          'zos:/id44)',
          'zos:/id44()',
          'zos://host:43/id44()?query',
          'zos://host:43/id44(?query',
          'zos://host:43/id44)?query',
          'zos://host/', #aperto a interpretazione - modifiche febbraio
          'zos://host/?query'
      ]
      for uri in tests:
          with self.subTest(uri=uri):
              with self.assertRaises(MalformedException):
                  parser.parse(uri)

    def valid_uris(self, parser):
        """ Valid uris, that the parser should parse correctly"""
        default_80 = 80
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
            's 1://host': {
                'scheme': 's 1',
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
            's://ho.st.a sd': {
                'scheme': 's',
                'userinfo': None,
                'host': 'ho.st.a sd',
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
            's://host/path/': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path': 'path/',
                'query': None,
                'fragment': None
            },
            's://host/p/ath/': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path': 'p/ath/',
                'query': None,
                'fragment': None
            },
            's:path/p/ath/': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path': 'path/p/ath/',
                'query': None,
                'fragment': None
            },
            's:/path/p/ath/': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path': 'path/p/ath/',
                'query': None,
                'fragment': None
            },
            's:/path ath/': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path': 'path ath/',
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
            's:?quer%20+-_y': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': 'quer%20+-_y',
                'fragment': None
            },
            's:path?quer%20+-_y': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path': 'path',
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
            's://host?query': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path':None,
                'query': 'query',
                'fragment': None
            },
            's://host/path/?query': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path': 'path/',
                'query': 'query',
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
            's://host/?quEry#12': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path':None,
                'query': 'quEry',
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
            's://host/?query#fragme?#@: nt': {
                'scheme': 's',
                'userinfo': None,
                'host': 'host',
                'port': 80,
                'path':None,
                'query': 'query',
                'fragment': 'fragme?#@: nt'
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
            's:#fragment': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': 'fragment'
            },
            's:path#fragment': {
                'scheme': 's',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path': 'path',
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
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'mAilTO:': {
                'scheme': 'mAilTO',
                'userinfo': None,
                'host': None,
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'mailto:asdasd': {
                'scheme': 'mailto',
                'userinfo': 'asdasd',
                'host': None,
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'mailto:user@host': {
                'scheme': 'mailto',
                'userinfo': 'user',
                'host': 'host',
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },

            #NEWS
            'news:': {
                'scheme': 'news',
                'userinfo': None,
                'host': None,
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'NEwS:': {
                'scheme': 'NEwS',
                'userinfo': None,
                'host': None,
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'news:host': {
                'scheme': 'news',
                'userinfo': None,
                'host': 'host',
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'news:127.0.0.1': {
                'scheme': 'news',
                'userinfo': None,
                'host': '127.0.0.1',
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },

            #TEL/FAX
            'tel:': {
                'scheme': 'tel',
                'userinfo': None,
                'host': None,
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'TeL:': {
                'scheme': 'TeL',
                'userinfo': None,
                'host': None,
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'fax:': {
                'scheme': 'fax',
                'userinfo': None,
                'host': None,
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'FAX:+39-327-9856-123': {
                'scheme': 'FAX',
                'userinfo': '+39-327-9856-123',
                'host': None,
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'tel:+393279856123': {
                'scheme': 'tel',
                'userinfo': '+393279856123',
                'host': None,
                'port': default_80,
                'path':None,
                'query': None,
                'fragment': None
            },

            #ZOS
            #https://elearning.unimib.it/mod/forum/discuss.php?d=196181
            'zos:/': {
                'scheme': 'zos',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'zos:/#abc': {
                'scheme': 'zos',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': 'abc'
            },
            'zos:?abc': {
                'scheme': 'zos',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path': None,
                'query': 'abc',
                'fragment': None
            },
            'zos:': {
                'scheme': 'zos',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'ZOS:/': {
                'scheme': 'ZOS',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path':None,
                'query': None,
                'fragment': None
            },
            'zos:/zos.path(z0s)': {
                'scheme': 'zos',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path': 'zos.path(z0s)',
                'query': None,
                'fragment': None
            },
            'zos:zos.path(z0s)': {
                'scheme': 'zos',
                'userinfo': None,
                'host': None,
                'port': 80,
                'path': 'zos.path(z0s)',
                'query': None,
                'fragment': None
            },
            'zos://user@host:99/zos.path(z0s)?query': {
                'scheme': 'zos',
                'userinfo': 'user',
                'host': 'host',
                'port': 99,
                'path': 'zos.path(z0s)',
                'query': 'query',
                'fragment': None
            },
            'zos://user@host:99': {
                'scheme': 'zos',
                'userinfo': 'user',
                'host': 'host',
                'port': 99,
                'path': None,
                'query': None,
                'fragment': None
            },
            #TODO add more tests for ZOS

        }
        for uri, expected in tests.items():
            with self.subTest(uri=uri, expected=expected):
                self.assertDictEqual(
                    parser.parse(uri),
                    expected,
                    f"parsed INCORRECTLY: {uri}"
                )
