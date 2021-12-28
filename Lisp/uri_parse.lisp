;;; -*- Mode:Lisp -*-

;;; begin of file: uri_parse.pl
(defstruct uri-structure
  scheme
  userinfo
  host
  port
  path
  query
  fragment
)

(defun uri-scheme (uri)
  (uri-structure-scheme uri))
(defun uri-userinfo (uri)
  (uri-structure-userinfo uri))
(defun uri-host (uri)
  (uri-structure-host uri))
(defun uri-port (uri)
  (uri-structure-port uri))
(defun uri-path (uri)
  (uri-structure-path uri))
(defun uri-query (uri)
  (uri-structure-query uri))
(defun uri-fragment (uri)
  (uri-structure-fragment uri))


(defun uri-parse (stringa)
  (make-uri-structure
   :scheme "a"
   :userinfo "b"
   :host "c"
   :port 1
   :path "gigio"
   :query "aa"
   :fragment "dd"))




;;; end of file -- uri_parse.pl