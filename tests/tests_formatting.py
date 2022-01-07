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
      filename = os.path.join(dirname, '../Prolog/uri-parse.prolog')
      self.columns_check(filename)

    def columns_check(self, path):
      """Check that a file respect the 80 columns limit"""
      with open(path, 'r') as f:
        for line in f:
          if len(line) > 80:
            self.fail(f"this line is too long: {line}")
