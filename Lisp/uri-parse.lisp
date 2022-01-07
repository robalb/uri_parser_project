;;;; -*- Mode:Lisp -*-
;;;;
;;;; begin of file: uri-parse.pl
;;;; This program implements a URI parser - a simplified version of Rfc3986,

;;; This parser is based on mutually recursive functions/expressions:
;;; Every expression takes a list of character in input, and returns a list 
;;; in output. The length of the list is variable. The last element is a list
;;; containing the characters that were not parsed from the input list.
;;; The other elements are values such as host, path, query... extracted from
;;; the input list


;;; parse-integer error  last lowercase


(defun string-to-list (string)
  (coerce string 'list))

(defun list-to-string (l)
  (if (null l)
      l
    (coerce l 'string)))

(defun list-to-int (l)
  (parse-integer (list-to-string l)))

;;; TODO: every expression hasthe leftover as the last element of its returned list,
;;; but here the leftover is expected to be the first element. it's confusing
(defun make-uri (scheme rest)
  (if (null (first rest))
      (make-uri-aux scheme (second rest) (third rest) (fourth rest)
                    (fifth rest) (sixth rest) (seventh rest))
    (halt-parser)))

(defun make-uri-aux (scheme userinfo host port path query fragment)
  (list (list "Scheme:" scheme)
        (list "Userinfo:" (list-to-string userinfo))
        (list "Host:" (list-to-string host))
        (list "Port:" (if (null port) 80 (list-to-int port)))
        (list "Path:" (list-to-string path))
        (list "Query:" (list-to-string query))
        (list "Fragment:" (list-to-string fragment))))

(defun uri-scheme (uri-structure)
  (second (first uri-structure)))

(defun uri-userinfo (uri-structure)
  (second (second uri-structure)))

(defun uri-host (uri-structure)
  (second (third uri-structure)))

(defun uri-port (uri-structure)
  (second (fourth uri-structure)))

(defun uri-path (uri-structure)
  (second (fifth uri-structure)))

(defun uri-query (uri-structure)
  (second (sixth uri-structure)))

(defun uri-fragment (uri-structure)
  (second (seventh uri-structure)))

(defun uri-display (uri-structure &optional (out-stream t))
  (or (null uri-structure)
      (progn
        (print-uri-element (first uri-structure) out-stream )
        (uri-display (rest uri-structure) out-stream))))

(defun print-uri-element (element out-stream)
  (format out-stream "~11A ~A~%" (first element) (second element)))

(defun uri-parse (stringa)
  (uri-parse-start (string-to-list stringa)))

(defun uri-parse-start (lista)
  (let ((scheme (scheme-parse lista)))
    (let ((scheme-string (string-downcase (list-to-string (first scheme)))))
                                        ;(write (second scheme))
      (cond ((string= scheme-string "mailto") 
             (make-uri "mailto" (parse-mailto (second scheme))))
            ((string= scheme-string "news") 
             (make-uri "news" (parse-news (second scheme))))
            ((string= scheme-string "tel")
             (make-uri "tel" (parse-telfax (second scheme))))
            ((string= scheme-string "fax")
             (make-uri "fax" (parse-telfax (second scheme))))
            ((string= scheme-string "zos")
             (make-uri "zos" (parse-generic-or-zos (second scheme) "zos")))
            (t (make-uri (list-to-string (first scheme))
                         (parse-generic-or-zos
                          (second scheme) (list-to-string (first scheme)))))))))

;;; Helper functions, that can be composed to create expressions

(defun halt-parser (&optional reason)
  "Halt the parser, by signaling a fatal error"
  (if (null reason)
      (error "Invalid URI")
    (error "Invalid URI: ~A" reason)))


(defun leftover (lista)
  "takes in input the list returned from an expression function, and returns 
    the part of the input string that was not parsed by that expression.
    Yes, this is just returning the last element of a list"
  (first (last lista)))


(defun zero-or-more-satisfying (lista pred)
  "parses an expression of the form <Identifier>*
   The identifier is composed by characters satisfying the given predicate"
  (if (or (null lista) (not (funcall pred (first lista))))
      (list nil lista)
    (let ((res-ric (zero-or-more-satisfying (rest lista) pred)))
      (list (cons (first lista) (first res-ric))
            (second res-ric))))) 


(defun one-or-more-satisfying (lista pred)
  "parses an expression of the form <Identifier>+
   The identifier is composed by characters satisfying the given predicate"
  (let ((res (zero-or-more-satisfying lista pred)))
    (if (null (first res))
        (halt-parser)
      res)))

                                        ; naming ideas: must-be-preceded-by-char
(defun preceded-by-char (lista char expr)
  "Parses an expression of the form ['Char' <Expr>]
   This functions wraps an expression function, and
   makes it work only if it's preceded by the given char"
  (if (eql (first lista) char)
      (if (null (rest lista))
          (halt-parser (format nil "unexpected EOF after ~A" char))
        (funcall expr (rest lista)))
    (list nil lista)))


(defun recursive-char-identifier (lista char identifier)
  "parses an expression of the form ['Char' <identifier> ]* "
  (if (eq (first lista) char)
      (let* (
             (res (one-or-more-satisfying (rest lista) identifier))
             (res-rec (recursive-char-identifier (leftover res) char identifier)))
        (list
         (append (append (list char) (first res)) (first res-rec))
         (second res-rec)))
    (list nil lista)))

;;; end of helper functions
;;; expressions for the URI parser

(defun parse-mailto (lista)
  (if (null lista)
      (list nil nil nil nil nil nil nil)
    (let* ((userinfo (userinfo-parse lista))
           (host (preceded-by-char (leftover userinfo) #\@ 'host-parse)))
      (list (leftover host) (first userinfo) (first host) nil nil nil nil))))


(defun parse-news (lista)
  (if (null lista)
      (list nil nil nil nil nil nil nil)
    (let ((host (host-parse lista)))
      (list (leftover host) nil (first host) nil nil nil nil))))


(defun parse-telfax (lista)
  (if (null lista)
      (list nil nil nil nil nil nil nil)
    (let ((userinfo (userinfo-parse lista)))
      (list (leftover userinfo) (first userinfo) nil nil nil nil nil))))


(defun parse-generic-or-zos (lista scheme)
  (let ((authorithy (authorithy-parse lista)))
    (let ((path-query-fragment
           (path-query-fragment-parse (fourth authorithy) scheme)))
      (list (leftover path-query-fragment) (first authorithy)
            (second authorithy) (third authorithy) (first path-query-fragment)
            (second path-query-fragment) (third path-query-fragment)))))


(defun authorithy-parse (lista)
  "Parse the expression '//' [ userinfo '@'] host [':' port]"
  (if (and (eql (first lista) #\/) (eql (second lista) #\/))
      (let* (
             (userinfo (userinfo-parse (rest (rest lista)) #\@))
             (host (host-parse (leftover userinfo)))
             (port (preceded-by-char (leftover host) #\: 'port-parse )))
        (list (first userinfo) (first host) (first port) (leftover port)))
    (list nil nil nil lista)))

(defun path-query-fragment-parse (lista scheme)
  "Parse the expression '/' [path] ['?' query] ['#' fragment]"
  (if (eq (first lista) #\/)
      (let* (
             (path (path-parse-choice (rest lista) scheme))
             (query (preceded-by-char (leftover path) #\? 'query-parse ))
             (fragment (preceded-by-char (leftover query) #\# 'fragment-parse )))
        (list (first path) (first query) (first fragment) (leftover fragment)))
    (list nil nil nil lista)))

(defun path-parse-choice (lista scheme)
  (if (string= scheme "zos")
      (zos-path-parse lista)
    (path-parse lista)))

(defun scheme-parse (lista)
  (must-end-with (one-or-more-satisfying lista 'identificatorep) #\:))

(defun userinfo-parse (lista &optional ends-with)
  (must-end-with (one-or-more-satisfying lista 'identificatorep) ends-with))

(defun host-parse (lista)
  (let* ((res (one-or-more-satisfying lista 'hostp))
         (res-rec (recursive-char-identifier (leftover res) #\. 'hostp)))
    (list (append (first res) (first res-rec)) (leftover res-rec))))

(defun port-parse (lista)
  (one-or-more-satisfying lista 'digitp))

(defun zos-path-parse (lista)
  (let ((res-44 (id44 lista)))
    (if (eql (first (leftover res-44)) #\( )
        (let ((res-8 (id8 (rest (leftover res-44)))))
          (if (eql (first (leftover res-8)) #\) )
              (list (concatenate 'list 
                                 (first res-44) (list #\( ) (first res-8) (list #\) ))
                    (rest (leftover res-8)))
            (halt-parser "missing closing bracket after id8")))
      res-44)))

(defun id44 (lista)
  (let ((res (one-or-more-satisfying lista 'id44p )))
    (if (or (> (length (first res)) 44)
            (or (not (alfap (first (first res))))
                (eql (first (last (first res))) #\. )))
        (halt-parser "id44 can't exceed 44 char length,
         start with a letter, or end with a '.'")
      res)))

(defun id8 (lista)
  (let ((res (one-or-more-satisfying lista 'id8p )))
    (if (or (> (length (first res)) 8)
            (digitp (first (first res))))
        (halt-parser "id8 can't exceed 8 char length or start with a letter")
      res)))

(defun path-parse (lista)
  (let ((res (zero-or-more-satisfying lista 'identificatorep)))
    (if (null (first res))
        (list nil lista)
      (let ((res-rec
             (recursive-char-identifier (leftover res) #\/ 'identificatorep)))
        (list (append (first res) (first res-rec))
              (leftover res-rec))))))


(defun query-parse (lista)
  (one-or-more-satisfying lista 'queryp))

(defun fragment-parse (lista)
  (one-or-more-satisfying lista 'anyp))

(defun identificatorep (char)
  (and (char/= char #\/)
       (char/= char #\?)
       (char/= char #\#)
       (char/= char #\@)
       (char/= char #\:)))

(defun hostp (char)
  (and (char/= char #\.)
       (identificatorep char)))

(defun queryp (char)
  (char/= char #\#))

(defun id44p (char)
  (or (alfanump char)
      (eql #\. char)))

(defun id8p (char)
  (alfanump char))

(defun alfanump (char)
  (or (alfap char)
      (digitp char)))

(defun alfap (char)
  (or (and (char<= char #\Z) (char>= char #\A))
      (and (char<= char #\z) (char>= char #\a))))

(defun digitp (char)
  (and (char<= char #\9) (char>= char #\0)))

(defun anyp (char)
  t)


(defun must-end-with (lista char)
  (if (eq char nil)
      lista
    (if (eq (first (second lista)) char)
        (list (first lista) (rest (second lista)))
      (list nil (append (first lista) (second lista))))))

(defun lunghezza (lista)
  (if (null lista)
      0
    (1+ (lunghezza (rest lista)))))
;;; end of file -- uri_parse.pl
