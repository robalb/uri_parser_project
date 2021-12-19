"""language-independent Unit tests
https://github.com/yuce/pyswip
"""
from pyswip import Prolog
from urllib.parse import urlparse

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
        self.prolog = Prolog()
        self.prolog.consult("uri-parse.pl")

    def __replace_prolog_none_with_none(self, dic):
        """Returns a new dict, with all the empty lists replaced with None"""

        def l(v):
            if isinstance(v, list):
                return None
            return v

        return {k: l(v) for k, v in dic.items()}

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
        row = query[0]
        row = self.__replace_prolog_none_with_none(row)
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

        def l(v):
            if isinstance(v, str) and len(v) == 0:
                return None
            return v

        return {
            'scheme': l(out.scheme),
            'userinfo': l(out.username),
            'host': l(out.hostname),
            'port': l(port),
            'path': l(out.path),
            'query': l(out.query),
            'fragment': l(out.fragment)
        }

prolog = PrologParser()
python = UrllibParser()
def parse(uri):
    print("PROLOG", prolog.parse(uri))
    print("PYTHON", python.parse(uri))

parse("http://hello.it")
