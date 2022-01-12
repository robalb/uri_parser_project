;;;; -*- Mode:Lisp -*-
;;;; begin of file: uri-parse.lisp
;;;;
;;;; 865993 Christian Dotti
;;;; 866359 Adriano Colombo
;;;; 866135 Alberto Ventafridda


;;; Definizione delle interfacce, come richiesto nella consegna

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

(defun uri-parse (stringa)
  (let ((uri (uri-parse-start (string-to-list stringa))))
    (if (haltedp uri)
        (error "wrong syntax in uri")
      uri)))

(defun uri-display (uri-structure &optional (out-stream t))
  (or (null uri-structure)
      (progn
        (print-uri-element (first uri-structure) out-stream)
        (uri-display (rest uri-structure) out-stream))))

(defun print-uri-element (element out-stream)
  (format out-stream "~11A ~A~%" (first element) (second element)))


;;; definizione di Funzioni di supporto, per semplificare alcune operazioni 
;;; ripetute spesso all'interno del programma

(defun string-to-list (string)
  (coerce string 'list))

(defun list-to-string (l)
  (if (null l)
      l
    (coerce l 'string)))

(defun list-to-int (l)
  (parse-integer (list-to-string l)))

(defun make-uri (scheme rest)
  "returns the uri structure, or throws an exception if the remainder of the
   parsed string is not empty"
  (if (or (haltedp scheme) (haltedp rest))
      (halt-parser)
    (if (null (first rest))
        (make-uri-aux scheme (second rest) (third rest) (fourth rest)
                      (fifth rest) (sixth rest) (seventh rest))
      (halt-parser))))

(defun make-uri-aux (scheme userinfo host port path query fragment)
  (list (list "Scheme:" (list-to-string scheme))
        (list "Userinfo:" (list-to-string userinfo))
        (list "Host:" (list-to-string host))
        (list "Port:" (if (null port) 80 (list-to-int port)))
        (list "Path:" (list-to-string path))
        (list "Query:" (list-to-string query))
        (list "Fragment:" (list-to-string fragment))))


;;; Definizione di funzioni il cui scopo è fare in modo
;;; che la struttura del programma rispecchi il più possibile
;;; quella delle regole di produzione della grammatica che deve riconoscere.
;;; Esempio: La produzione [userinfo @] può essere definita tramite
;;; le funzioni (must-end-with @ (one-or-more 'identificatore))

(defun halt-parser (&optional reason)
  "Halts the parser"
  nil)
(defun haltedp (lista)
  "If the input is a list of length 0 then the parser is halted,
    otherwise the parsing can continue"
  (if (eq (length lista) 0)
      t
    nil))

(defun remainder (lista)
  "takes in input the list returned from an expression function, and returns 
    the part of the input string that was not parsed by that expression.
    Yes, this is just returning the last element of a list"
  (first (last lista)))

(defun must-end-with (lista char)
  "Backtracks if the given parser result doesn't end with the given char"
  (if (eq char nil)
      lista
    (if (eq (first (second lista)) char)
        (list (first lista) (rest (second lista)))
      (list nil (append (first lista) (second lista))))))

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

(defun preceded-by-char (lista char expr)
  "Parses an expression of the form ['Char' <Expr>]
   This functions wraps an expression function, and
   makes it work only if it's preceded by the given char"
  (if (eql (first lista) char)
      (if (null (rest lista))
          (halt-parser)
        (funcall expr (rest lista)))
    (list nil lista)))

(defun recursive-char-identifier (lista char identifier)
  "parses an expression of the form ['Char' <identifier> ]* "
  (if (eq (first lista) char)
      (let ((res (one-or-more-satisfying (rest lista) identifier)))
        (if (haltedp res)
            (halt-parser)
          (let ((res-rec (recursive-char-identifier
                          (remainder res) char identifier)))
            (if (haltedp res-rec)
                (halt-parser)
              (list
               (append (append (list char) (first res)) (first res-rec))
               (second res-rec))))))
    (list nil lista)))


;;; Definizione delle funzioni mutualmente recursive per il 
;;; Recursive-descent-parser.
;;; Ogni funzione implementa il riconoscimento di un simbolo non terminale
;;; della grammatica descritta nella consegna.
;;; Dove necessario, le funzioni eseguono backtracking.

(defun uri-parse-start (lista)
  (let* ((scheme (scheme-parse lista))
         (scheme-str (list-to-string (first scheme)))
         (scheme-str-down (string-downcase scheme-str)))
    (make-uri (first scheme)
              (cond ((string= scheme-str-down "mailto") 
                     (parse-mailto (remainder scheme)))
                    ((string= scheme-str-down "news") 
                     (parse-news (remainder scheme)))
                    ((string= scheme-str-down "tel")
                     (parse-telfax (remainder scheme)))
                    ((string= scheme-str-down "fax")
                     (parse-telfax (remainder scheme)))
                    ((string= scheme-str-down "zos")
                     (parse-generic-or-zos (remainder scheme) "zos"))
                    (t
                     (parse-generic-or-zos (remainder scheme) scheme-str))))))

(defun parse-mailto (lista)
  (if (null lista)
      (list nil nil nil nil nil nil nil)
    (let ((userinfo (userinfo-parse lista)))
      (if (haltedp userinfo)
          (halt-parser)
        (let ((host (preceded-by-char (remainder userinfo) #\@ 'host-parse)))
          (if (haltedp host)
              (halt-parser)
            (list (remainder host)
                  (first userinfo) (first host) nil nil nil nil)))))))

(defun parse-news (lista)
  (if (null lista)
      (list nil nil nil nil nil nil nil)
    (let ((host (host-parse lista)))
      (if (haltedp host)
          (halt-parser)
        (list (remainder host) nil (first host) nil nil nil nil)))))

(defun parse-telfax (lista)
  (if (null lista)
      (list nil nil nil nil nil nil nil)
    (let ((userinfo (userinfo-parse lista)))
      (if (haltedp userinfo)
          (halt-parser)
        (list (remainder userinfo) (first userinfo) nil nil nil nil nil)))))

(defun parse-generic-or-zos (lista scheme)
  (let ((authorithy (authorithy-parse lista)))
    (if (haltedp authorithy)
        (halt-parser)
      (let ((path-query-fragment
             (path-query-fragment-parse (fourth authorithy) scheme)))
        (if (haltedp path-query-fragment)
            (halt-parser)
          (list (remainder path-query-fragment) (first authorithy)
                (second authorithy) (third authorithy)
                (first path-query-fragment) (second path-query-fragment)
                (third path-query-fragment)))))))

(defun authorithy-parse (lista)
  "Parse the expression '//' [ userinfo '@'] host [':' port]"
  (if (and (eql (first lista) #\/) (eql (second lista) #\/))
      (let ((userinfo (userinfo-parse (rest (rest lista)) #\@)))
        (if (haltedp userinfo)
            (halt-parser)
          (let ((host (host-parse (remainder userinfo))))
            (if (haltedp host)
                (halt-parser)
              (let ((port (preceded-by-char (remainder host) #\: 'port-parse)))
                (if (haltedp port)
                    (halt-parser)
                  (list (first userinfo)
                        (first host) (first port) (remainder port))))))))
    (list nil nil nil lista)))

(defun path-query-fragment-parse (lista scheme)
  "Parse the expression '/' [path] ['?' query] ['#' fragment]"
  (if (eq (first lista) #\/)
      (let ((path (path-parse-choice (rest lista) scheme)))
        (if (haltedp path)
            (halt-parser)
          (let ((query (preceded-by-char (remainder path) #\? 'query-parse)))
            (if (haltedp query)
                (halt-parser)
              (let ((fragment (preceded-by-char
                               (remainder query) #\# 'fragment-parse)))
                (if (haltedp fragment)
                    (halt-parser)
                  (list (first path) (first query)
                        (first fragment) (remainder fragment))))))))
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
  (let ((ip (ip-parse lista)))
    (if (null (first ip))
        (let ((res (one-or-more-satisfying lista 'hostp)))
          (if (haltedp res)
              (halt-parser)
            (let ((res-rec (recursive-char-identifier (remainder res)
                                                      #\. 'hostp)))
              (if (haltedp res-rec)
                  (halt-parser)
                (list (append (first res) (first res-rec))
                      (remainder res-rec))))))
      ip)))


(defun port-parse (lista)
  (one-or-more-satisfying lista 'digitp))

(defun zos-path-parse (lista)
  (let ((res-44 (id44 lista)))
    (if (haltedp res-44)
        (halt-parser)
      (if (eql (first (remainder res-44)) #\()
          (let ((res-8 (id8 (rest (remainder res-44)))))
            (if (haltedp res-8)
                (halt-parser)
              (if (eql (first (remainder res-8)) #\))
                  (list (concatenate 'list 
                                     (first res-44) (list #\() (first res-8)
                                     (list #\)))
                        (rest (remainder res-8)))
                (halt-parser))))
        res-44))))

(defun id44 (lista)
  (let ((res (one-or-more-satisfying lista 'id44p)))
    (if (haltedp res)
        (halt-parser)
      (if (or (> (length (first res)) 44)
              (or (not (alfap (first (first res))))
                  (eql (first (last (first res))) #\.)))
          (halt-parser)
        res))))

(defun id8 (lista)
  (let ((res (one-or-more-satisfying lista 'id8p)))
    (if (haltedp res)
        (halt-parser)
      (if (or (> (length (first res)) 8)
              (digitp (first (first res))))
          (halt-parser)
        res))))

(defun path-parse (lista)
  (let ((res (zero-or-more-satisfying lista 'identificatorep)))
    (if (null (first res))
        (list nil lista)
      (if (haltedp res)
          (halt-parser)
        (let ((res-rec
               (recursive-char-identifier (remainder res)
                                          #\/ 'identificatorep)))
          (if (haltedp res-rec)
              (halt-parser)
            (list (append (first res) (first res-rec))
                  (remainder res-rec))))))))

(defun query-parse (lista)
  (one-or-more-satisfying lista 'queryp))

(defun fragment-parse (lista)
  (one-or-more-satisfying lista 'charp))

(defun id44p (char)
  (or (alfanump char)
      (eql #\. char)))

(defun id8p (char)
  (alfanump char))

(defun alfap (char)
  (or (and (char<= char #\Z) (char>= char #\A))
      (and (char<= char #\z) (char>= char #\a))))

(defun digitp (char)
  (and (char<= char #\9) (char>= char #\0)))

(defun alfanump (char)
  (or (alfap char)
      (digitp char)))


;;; Definizione progressivamente restrittiva dei predicati per il riconoscimento
;;; dei caratteri all'interno degli identificatori.
;;; Per semplicità, a differenza dell'rfc, definiamo come base senza restrizioni
;;; qualsiasi carattere stampabile dello standard ascii, ovvero qualsiasi
;;; carattere nel range 0x21 - 0x7E (lo spazio (0x20) è escluso)

(defun charp (char)
  (char>= char #\!))

(defun queryp (char)
  (and (charp char)(char/= char #\#)))

(defun identificatorep (char)
  (and (queryp char)
       (char/= char #\/)
       (char/= char #\?)
       (char/= char #\@)
       (char/= char #\:)))

(defun hostp (char)
  (and (char/= char #\.)
       (identificatorep char)))


;;; Le regole per il riconoscimento di un IPv4 sono ridondanti e non
;;; contribuiscono al funzionamento del programma, dal momento che non
;;; è richiesto di differenziare in alcun modo tra un host e un IPv4.
;;; Sono tuttavia presenti nella grammatica della consegna, e per questo motivo
;;; Le abbiamo implementate e integrate nel programma

(defun ip-parse (lista)
  (let ((res (ip-parse-aux lista)))
    (if (eql (length (first res)) 15)
        res
      (list nil lista))))

(defun is-nnn (digit1 digit2 digit3)
  (let ((nnn (list-to-int (list digit1 digit2 digit3))))
    (and (>= nnn 0)
         (<= nnn 255))))

(defun ip-parse-aux (lista)
  (let ((res (zero-or-more-satisfying lista 'digitp)))
    (if (and (not (null (first res)))
             (eql (length (first res)) 3)
             (is-nnn (first (first res))
                     (second (first res))
                     (third (first res))))
        (if (eql (first (remainder res)) #\.)
            (let ((res-rec (ip-parse-aux (rest (remainder res)))))
              (list (append (append (first res) (list #\.))
                            (first res-rec)) (remainder res-rec)))
          res)
      (list nil lista))))

;;; end of file -- uri_parse.lisp
