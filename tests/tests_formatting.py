import unittest
import os

__unittest = True

class Tests(unittest.TestCase):
    """Formatting and indentation tests
    """
    def test_80_cols_LISP(self):
      dirname = os.path.dirname(__file__)
      filename = os.path.join(dirname, '../Lisp/uri-parse.lisp')
      self.columns_check(filename)

    def test_80_cols_PROLOG(self):
      dirname = os.path.dirname(__file__)
      filename = os.path.join(dirname, '../Prolog/uri-parse.pl')
      self.columns_check(filename)

    def columns_check(self, path):
      """Check that a file respect the 80 columns limit"""
      line_num = 1
      with open(path, 'r') as f:
        for line in f:
          with self.subTest(line=line_num):
            # 80 chars + \n
            # Note: this also works with crlf, when python reads a file in text mode, crlf is
            # replaced with \n
            if len(line) > 80 + 1:
              self.fail(f"line {line_num} is too long: {line}")
          line_num += 1

    def emacs_check(self, path):
      """Check that a file respects the emacs indentation"""
      pass
