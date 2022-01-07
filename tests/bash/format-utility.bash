#!/bin/sh
#https://cslab.pepperdine.edu/warford/BatchIndentationEmacs.html

# emacs -batch file-to-indent -l ~/bin/emacs-format-file -f emacs-format-function
emacs -batch ../../Lisp/uri-parse.lisp -l ./emacs-format-file.lisp -f emacs-format-function
