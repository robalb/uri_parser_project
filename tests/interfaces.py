"""language-independent Unit tests
https://github.com/yuce/pyswip
https://docs.python.org/3/library/subprocess.html
"""
from pyswip import Prolog
from urllib.parse import urlparse
import os

class MalformedException(Exception):
    pass

class ParserInterface:
    """Interface"""

    def __init__(self):
        """Initialize the uri parser"""
        pass

    def parse(self, uri:str) -> dict:
        """Parse an uri, and returns its components"""
        pass

class PrologParser(ParserInterface):
    def __init__(self):
        dirname = os.path.dirname(__file__)
        filename = os.path.join(dirname, '../Prolog/uri-parse.pl')
        self.prolog = Prolog()
        self.prolog.consult(filename)

    def __normalize_none(self, dic):
        """Returns a new dict, with all the empty lists replaced with None"""

        def l(v):
            if isinstance(v, list):
                return None
            return v

        return {k: l(v) for k, v in dic.items()}

    def query(self, query):
        #Danger: This allows arbitrary prolog code execution
        return list(self.prolog.query(query))

    def parse(self, uri):
        prolog_query = f'''
            uri_parse("{uri}", uri(Scheme,
              Userinfo,
              Host,
              Port,
              Path,
              Query,
              Fragment))'''
        query = self.query(prolog_query)
        if len(query) == 0:
            raise MalformedException(uri)
        row = query[0]
        row = self.__normalize_none(row)
        return {
            'scheme': row['Scheme'],
            'userinfo': row['Userinfo'],
            'host': row['Host'],
            'port': row['Port'],
            'path': row['Path'],
            'query': row['Query'],
            'fragment': row['Fragment']
        }

class UrllibParser(ParserInterface):
    def __init__(self):
        pass

    def parse(self, uri):
        out = urlparse(uri)
        port = out.port
        if port is None:
            if out.scheme == 'http':
                port = '80'
            elif out.scheme == 'https':
                port = '443'

        def normalize_none(v):
            """replace empty strings with none"""
            if isinstance(v, str) and len(v) == 0:
                return None
            return v

        return {
            'scheme': normalize_none(out.scheme),
            'userinfo': normalize_none(out.username),
            'host': normalize_none(out.hostname),
            'port': normalize_none(port),
            'path': normalize_none(out.path),
            'query': normalize_none(out.query),
            'fragment': normalize_none(out.fragment)
        }

# prolog = PrologParser()
# python = UrllibParser()
# def parse(uri):
#     try:
#         print("PROLOG", prolog.parse(uri))
#     except MalformedException as e:
#         print("PROLOG: malformed")
#     print("PYTHON", python.parse(uri))

# parse("mailto://foo/bar?q")