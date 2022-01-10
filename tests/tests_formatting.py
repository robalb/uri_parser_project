""" Formatting and indentation tests"""
import unittest
import os
import subprocess


__unittest = True

dirname = os.path.dirname(__file__)
lisp_path = os.path.join(dirname, '../Lisp/uri-parse.lisp')
prolog_path = os.path.join(dirname, '../Prolog/uri-parse.pl')

class Tests(unittest.TestCase):
    """Formatting and indentation tests
    """
    def test_80_cols_LISP(self):
      self.columns_check(lisp_path)

    def test_80_cols_PROLOG(self):
      self.columns_check(prolog_path)

    def test_emacs_indent_LISP(self):
      self.emacs_check(lisp_path)

    def test_emacs_indent_PROLOG(self):
      self.emacs_check(prolog_path)

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
      cmd = [
        '/project/tests/bash/diff-emacs-indent.sh',
        path,
      ]
      out = subprocess.run(cmd, capture_output=True)
      out_string = out.stdout.decode()
      if len(out_string) > 0:
        self.fail(f"after formatting the file with emacs, some lines changed:\n{out_string}")

