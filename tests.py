"""lnanguage-independent Unit tests
https://github.com/yuce/pyswip
"""
from pyswip import Prolog


class Parser:
    def __init__(self):
        pass

    def parse(uri):
        return {}

class PrologParser(Parser):
    def __init__(self):
        self.prolog = Prolog()
        self.prolog.consult("uri-parse.pl")
        pass

    def parse(self, uri):
        #Danger: arbitrary prolog code execution
        prolog_query = f'''
            uri_parse("{uri}", uri(Scheme,
              Userinfo,
              Host,
              Port,
              Path,
              Query,
              Fragment))'''
        query = list(self.prolog.query(prolog_query))
        return query[0]

parser = PrologParser()
print(parser.parse("http://hello.it"))