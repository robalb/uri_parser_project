import unittest
from .interfaces import PrologParser, MalformedException
import time,datetime

__unittest = True

class Tests(unittest.TestCase):
    """Prolog specific tests
    """
    @classmethod
    def setUpClass(self):
        self.parser = PrologParser()


    def test_expect_true(self):
        tests = {
            #assignment pdf, page 4/6
            'http://disco.unimib.it': "uri(http, _, _, _, _, _, _)",
            #empty lists
            's://host.com': "uri('s', [], 'host.com', _, [], [], [])",
            #numeric port
            's://host.com:42': "uri(_, _, 'host.com', 42, _, _, _)",
            #only scheme
            'qwe:': "uri(qwe, _, _, _, _, _, _)"
        }
        for string, term in tests.items():
            with self.subTest(string=string, term=term):
                prolog_query = f'uri_parse("{string}", {term})'
                try:
                    query = self.parser.query(prolog_query)
                except MalformedException:
                    self.fail(f"did NOT parse: {string}")
                res = bool(query)
                self.assertTrue(res)

    def test_time_metrics(self):

        print("== TIMING TESTS ==")
        tests = [
            'uri_parse("http://asd", X)',
            'uri_parse("http://user@disco.unimib.it/moodle/login?auth=asd&q=1#12", X)',
        ]
        for test in tests:
            start_time = time.time()
            query = self.parser.query(test)
            end_time = time.time()
            elapsed = end_time - start_time
            print("-----------")
            print( test)
            print( str(datetime.timedelta(seconds=elapsed)))
