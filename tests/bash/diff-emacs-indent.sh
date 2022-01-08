#!/bin/sh
#https://cslab.pepperdine.edu/warford/BatchIndentationEmacs.html

path="$1"
dir="$(dirname "${path}")"

# -l ~/projects/uri_parser_project/tests/bash/emacs-format-file.lisp \

emacs -batch $path \
  -l /project/tests/bash/emacs-format-file.lisp \
  -f emacs-format-function

diff $path "$dir/emacs-formatted"

