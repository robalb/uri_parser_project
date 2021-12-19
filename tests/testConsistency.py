import unittest
from .interfaces import PrologParser, UrllibParser

class TestConsistency(unittest.TestCase):
    @classmethod
    def setUpClass(self):
        self.prolog = PrologParser()
        self.python = UrllibParser()

    def test_basics(self):
        uri = "http://github.com"
        self.assertDictEqual(
            self.prolog.parse(uri),
            self.python.parse(uri),
        )
