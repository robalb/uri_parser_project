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



(defun string-to-list (string)
  (coerce string 'list))

(defun list-to-string (l)
  (if (null l)
    l
    (coerce l 'string)
  ))

(defun list-to-int (l) 
  (parse-integer (list-to-string l))
)

;(set  'lista (string-to-list stringa))
(defun urip (lista)
  ;(write lista)
  (if (null lista)
      t
    nil))

;;; TODO: every expression hasthe leftover as the last element of its returned list,
;;; but here the leftover is expected to be the first element. it's confusing
(defun make-uri (scheme rest)
  ;(write rest)
  ;(write (first rest))
  ;(write scheme)
  (if (urip (first rest))
      (make-uri-aux scheme (second rest) (third rest) (fourth rest) (fifth rest) (sixth rest) (seventh rest))
    (make-uri-aux "" nil nil (string-to-list "-1") nil nil nil)))

(defun make-uri-aux (scheme userinfo host port path query fragment)
    (list (list "Scheme:" scheme)
          (list "Userinfo:" (list-to-string userinfo))
          (list "Host:" (list-to-string host))
          (list "Port:" (if (null port) 80 (list-to-int port)))
          (list "Path:" (list-to-string path))
          (list "Query:" (list-to-string query))
          (list "Fragment:" (list-to-string fragment)))
  )

(defun uri-scheme (uri-structure)
  (second (first uri-structure))
  )

(defun uri-userinfo (uri-structure)
  (second (second uri-structure))
  )

(defun uri-host (uri-structure)
  (second (third uri-structure))
  )

(defun uri-port (uri-structure)
  (second (fourth uri-structure))
  )

(defun uri-path (uri-structure)
  (second (fifth uri-structure))
  )

(defun uri-query (uri-structure)
  (second (sixth uri-structure))
  )

(defun uri-fragment (uri-structure)
  (second (seventh uri-structure))
  )

(defun uri-display (uri-structure &optional (out-stream t))
  (or (null uri-structure)
    (progn
      (print-uri-element (first uri-structure) out-stream )
      (uri-display (rest uri-structure) out-stream)
    )
  )
)

(defun print-uri-element (element out-stream)
  (format out-stream "~11A ~A~%" (first element) (second element))
)

;;; TODO: vabene string-downcase? uri-path di "qwe:/PaTh" dovrebbe restituire PaTh
(defun uri-parse (stringa)
  (the-uri-parse (string-to-list (string-downcase stringa)))
  )

(defun the-uri-parse (lista)
  (let ((scheme (scheme-parse lista)))
    (let ((scheme-string (list-to-string (first scheme))))
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
             (make-uri "zos" (parse-generic-and-zos (second scheme) "zos")))
            (t (make-uri (list-to-string (first scheme)) (parse-generic-and-zos (second scheme) (list-to-string (first scheme)))))
            )
      )
    )
  )

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
  "Parses an expression of the form ['Char' <Expr>]"
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
          (append
            (append (list char) (first res))
            (first res-rec))
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
        (list (second host) nil (first host) nil nil nil nil))))


(defun parse-telfax (lista)
  (if (null lista)
      (list nil nil nil nil nil nil nil)
      (let ((userinfo (userinfo-parse lista)))
        (list (second userinfo) (first userinfo) nil nil nil nil nil))))


(defun parse-generic-and-zos (lista scheme)
  (let ((authorithy (authorithy-parse lista)))
    (let ((path-query-fragment (path-query-fragment-parse (fourth authorithy) scheme)))
      (list (fourth path-query-fragment) (first authorithy) (second authorithy) (third authorithy)
            (first path-query-fragment) (second path-query-fragment)
            (third path-query-fragment)))))


(defun authorithy-parse (lista)
  (if (and (eql (first lista) (second lista)) (eql (first lista) #\/))
      (let ((userinfo (userinfo-parse (rest (rest lista)) #\@)))
        (let ((host (host-parse (second userinfo))))
          (if (eq (first (second host)) #\:)
              (let ((port (port-parse (rest (second host)))))
                (list (first userinfo) (first host) (first port) (second port)))
              (list (first userinfo) (first host) nil (second host)))
            ))
      (list nil nil nil lista)))

; (defun authorithy-parse (lista)
;   (if (and (eql (first lista) (second lista)) (eql (first lista) #\/))
;       (let ((userinfo (userinfo-parse (rest (rest lista)) #\@)))
;         (let ((host (host-parse (second userinfo))))
;           (if (eq (first (second host)) #\:)
;               (let ((port (port-parse (rest (second host)))))
;                 (list (first userinfo) (first host) (first port) (second port)))
;               (list (first userinfo) (first host) nil (second host)))
;             ))
;       (list nil nil nil lista)))

(defun path-query-fragment-parse (lista scheme)
  (if (eq (first lista) #\/)
      (let ((path (path-parse-choice (rest lista) scheme)));;attenzione ad host-parse-choice!! path in zos � effettivamente obbligatorio?
        ;(write "path:")
        ;(write path)
        (if (eq (first (second path)) #\?)
            (let ((query (query-parse (rest (second path)))))
              (if (eq (first (second query)) #\#)
                  (let ((fragment (fragment-parse (rest (second query)))))
                    (list (first path) (first query) (first fragment) (second fragment)))
                (list (first path) (first query) nil (second query))))
          (list (first path) nil nil (second path))))
    (list nil nil nil lista))
                    
  )

(defun path-parse-choice (lista scheme)
  (if (string= scheme "zos")
      (zos-path-parse lista)
    (path-parse lista)))

(defun scheme-parse (lista)
  (must-end-with (one-or-more-satisfying lista 'identificatorep) #\:))

(defun userinfo-parse (lista &optional ends-with)
  (must-end-with (one-or-more-satisfying lista 'identificatorep) ends-with))

; (defun host-parse (lista)
;       (if (and (eq (length lista) 1)
;             (identificatorep (first lista)))
;           (list (first lista) (rest lista))
;           (let ((identificatore (one-or-more-satisfying lista 'hostp)))
;             (if (null (first identificatore))
;                 (list nil lista)
;                 (if (and (eql (first (second identificatore)) #\.)
;                       (hostp (second (second identificatore))))
;                     (let ((risultato-ric-host-parse (host-parse (rest (second identificatore)))))
;                       ;(write "risultato-ric:")
;                       ;(write risultato-ric-host-parse)
;                       (list (append (append (first identificatore) (list #\.)) (first risultato-ric-host-parse))
;                         (second risultato-ric-host-parse))) ; *
;                     (list (first identificatore) (second identificatore)))))))

(defun host-parse (lista)
  (let* ((res (one-or-more-satisfying lista 'hostp))
      (res-rec (recursive-char-identifier (leftover res) #\. 'hostp)))
    (list (append (first res) (first res-rec)) (leftover res-rec))))


(defun port-parse (lista)
  (one-or-more-satisfying lista 'digitp))

(defun zos-path-parse (lista)
  (if (alfap (first lista))
      (let ((id44-parsed (must-not-end-with (one-or-more-satisfying lista 'id44p) #\.)))
        (if (<= (lunghezza (first id44-parsed)) 44)
            (if (eql (first (second id44-parsed)) #\()
                (let ((id8-parsed (one-or-more-satisfying (second id44-parsed) 'id8p)))
                  (if (<= (lunghezza (first id8-parsed)) 8)
                      (list (append (append (first id44-parsed) (list #\())
                                    (append (first id8-parsed) (list #\))))
                            (second id8-parsed))
                    (list nil lista)))
              id44-parsed)
          (list nil lista)))
    (list nil lista)
    )
  )

(defun hack-equal (lista char predicate)
  (and (eql (first lista) char)
        (funcall (predicate  (second lista) ))))

(defun path-parse (lista)
  (let ((identificatore (zero-or-more-satisfying lista 'identificatorep)))
    (if (null (first identificatore))
        (list nil lista)
        (let ((res (recursive-char-identifier
                     (second identificatore) #\/ 'identificatorep)))
          (list (append
                  (first identificatore)
                  (first res))
                (second res))))))


(defun query-parse (lista)
  (one-or-more-satisfying lista 'queryp))

(defun fragment-parse (lista)
  (one-or-more-satisfying lista 'any))

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
  (and (char<= char #\9) (char>= char #\0))
  )

(defun any (char)
  t)


(defun must-end-with (lista char)
  (if (eq char nil)
      lista
    (if (eq (first (second lista)) char)
        (list (first lista) (rest (second lista)))
      ;(list nil (rest (second lista)))
      (list nil (append (first lista) (second lista)))
      )
    )
  )

(defun must-not-end-with (lista char)
  (if (eq char nil)
      lista
    (if (eq (first (second lista)) char)
        (list nil (append (first lista) (second lista)))
      (list (first lista) (rest (second lista)))
      )
    )
)

(defun lunghezza (lista)
  (if (null lista)
      0
    (1+ (lunghezza (rest lista)))))
;;; end of file -- uri_parse.pl
