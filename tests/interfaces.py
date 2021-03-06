"""interfaces for the language-independent Unit tests

This module defines a generic URI-parser interface,
that can be used to parse an URI using either the prolog parser
or the lisp parser.

The prolog program is queried using pyswip
https://github.com/yuce/pyswip
The lisp program is executed as a subprocess
https://docs.python.org/3/library/subprocess.html
"""
from pyswip import Prolog
import subprocess
import os
import re


class MalformedException(Exception):
    """Malformed URI, can't be parsed"""
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


class LispParser(ParserInterface):
  def __init__(self):
    pass

  def parse(self, uri):
    dirname = os.path.dirname(__file__)
    filename = os.path.join(dirname, '../Lisp/uri-parse.lisp')
    lisp_query = f"""(let ((uri (uri-parse "{uri}")))
    (format t ">  ~A~%" (uri-scheme uri))
    (format t ">  ~A~%" (uri-userinfo uri))
    (format t ">  ~A~%" (uri-host uri))
    (format t ">  ~A~%" (uri-port uri))
    (format t ">  ~A~%" (uri-path uri))
    (format t ">  ~A~%" (uri-query uri))
    (format t ">  ~A~%" (uri-fragment uri)))"""
    cmd = [
    'sbcl',
    '--load',
    filename,
    '--eval',
    lisp_query,
    '--non-interactive'
    ]
    out = subprocess.run(cmd, capture_output=True)
    #get the stdout output
    out_string = out.stdout.decode()
    #remove the copyright header
    out_lines = out_string.split("\n")[-8:-1]
    #extract the uri components
    components = []
    for index, line in enumerate(out_lines):
      m = re.search(' (\S+?)$', line)
      if m:
        value = m.group(1)
        #handle port transform
        if index == 3 and value != 'NIL':
          value = int(value)
        #handle NIL transform
        if value == 'NIL':
          value = None
        components.append(value)
      else:
        raise MalformedException(uri)
    return {
        'scheme': components[0],
        'userinfo': components[1],
        'host': components[2],
        'port': components[3],
        'path': components[4],
        'query': components[5],
        'fragment': components[6]
    }

##usage example
#lisp = LispParser()
#print(lisp.parse("http://asd.com"))

