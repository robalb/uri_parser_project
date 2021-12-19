import unittest
from .interfaces import PrologParser, UrllibParser, MalformedException

class TestRfc(unittest.TestCase):
    """Basic tests for the subset of rfc3986 described in the assignment
    https://elearning.unimib.it/pluginfile.php/1141330/mod_resource/content/13/20220115%20LP%20E1P%20URI%20v2.pdf
    https://datatracker.ietf.org/doc/html/rfc3986
    most of the tests have been ported from:
    https://github.com/sporkmonger/addressable/blob/master/spec/addressable/uri_spec.rb
    """
    @classmethod
    def setUpClass(self):
        self.parser = PrologParser()
    
    def test_known(self):
        #assignment pdf, page 4/6
        self.assertDictEqual(
            self.parser.parse("http://disco.unimib.it"),
            {
                'scheme': 'http',
                'userinfo': None,
                'host': 'disco.unimib.it',
                'port': '80',
                'path': None,
                'query': None,
                'fragment': None
            }
        )
        #https://elearning.unimib.it/mod/forum/discuss.php?d=189837
        with self.assertRaises( MalformedException):
            self.parser.parse("mailto://foo/bar?q")
