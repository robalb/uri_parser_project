import unittest
from .interfaces import PrologParser, UrllibParser, MalformedException


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

    def test_malformed_uris(self):
        tests = [
            #https://elearning.unimib.it/mod/forum/discuss.php?d=189837
            'mailto://foo/bar?q',
            #https://elearning.unimib.it/mod/forum/discuss.php?d=189837#p306737
            'zos:/',
            #https://elearning.unimib.it/mod/forum/discuss.php?d=189837#p307681
            'zos://you@themainframe.bigiron.fe/hlq.source.fortran(svd)/some/more/path',
            #userinfo can't contain ':'
            'http://user:password@disco.unimib.it'
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
                'port': '80',
                'path': None,
                'query': None,
                'fragment': None
            },
            #https, ports
            'https://disco.unimib.it:42': {
                'scheme': 'https',
                'userinfo': None,
                'host': 'disco.unimib.it',
                'port': '42',
                'path': None,
                'query': None,
                'fragment': None
            },
            #userinfo
            'http://az-_09@disco.unimib.it': {
                'scheme': 'http',
                'userinfo': 'az-_09',
                'host': 'disco.unimib.it',
                'port': '80',
                'path': None,
                'query': None,
                'fragment': None
            },
            #path TODO initial_slash
            'http://disco.unimib.it/p/a/th': {
                'scheme': 'http',
                'userinfo': None,
                'host': 'disco.unimib.it',
                'port': '80',
                'path': 'p/a/th',
                'query': None,
                'fragment': None
            },
            #query
            'http://az-_09@disco.unimib.it?query-+./@': {
                'scheme': 'http',
                'userinfo': 'az-_09',
                'host': 'disco.unimib.it',
                'port': '80',
                'path':None,
                'query': 'query-+./@',
                'fragment': None
            },
            #fragment
            'http://az-_09@disco.unimib.it#fragment+-/@': {
                'scheme': 'http',
                'userinfo': 'az-_09',
                'host': 'disco.unimib.it',
                'port': '80',
                'path':None,
                'query': None,
                'fragment': 'fragment+-/@'
            },
        }
        for uri, expected in tests.items():
            with self.subTest(uri=uri, expected=expected):
                self.assertDictEqual(
                    self.parser.parse(uri),
                    expected,
                    f"{uri} should parse correctly"
                )
