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
        #https://elearning.unimib.it/mod/forum/discuss.php?d=189837
        with self.assertRaises(MalformedException):
            self.parser.parse("mailto://foo/bar?q")

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
            'https://disco.unimib.it': {
                'scheme': 'https',
                'userinfo': None,
                'host': 'disco.unimib.it',
                'port': '80',
                'path': None,
                'query': None,
                'fragment': None
            },
            'https://user@disco.unimib.it:90': {
                'scheme': 'https',
                'userinfo': 'user',
                'host': 'disco.unimib.it',
                'port': '90',
                'path': None,
                'query': None,
                'fragment': None
            },
        }
        for uri, res in tests.items():
            with self.subTest(uri=uri, expected=res):
                self.assertDictEqual(
                    self.parser.parse(uri),
                    res,
                    f"{uri} should parse correctly"
                )
