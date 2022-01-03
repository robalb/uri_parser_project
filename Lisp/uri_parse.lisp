;;; -*- Mode:Lisp -*-

;;; begin of file: uri_parse.pl


(defun error-mine (where what)
  (write "Errore nella stringa inserita")
  (write where)
  (write what)
  nil
  )


(defun string-to-list (string)
  (coerce string 'list))

(defun list-to-string (l)
  (if (null l)
    l
    (coerce l 'string)
  ))

;(set  'lista (string-to-list stringa))
(defun urip (lista)
  ;(write lista)
  (if (null lista)
      t
    nil))

(defun make-uri (scheme rest)
  ;(write rest)
  ;(write (first rest))
  ;(write scheme)
  (if (urip (first rest))
      (make-uri-aux scheme (second rest) (third rest) (fourth rest) (fifth rest) (sixth rest) (seventh rest))
    (make-uri-aux "" nil nil -1 nil nil nil)))

(defun make-uri-aux (scheme userinfo host port path query fragment)
  (if (null port)
      (list (list "Scheme:" scheme)
            (list "Userinfo:" (list-to-string userinfo))
            (list "Host:" (list-to-string host))
            (list "Port:" 80)
            (list "Path:" (list-to-string path))
            (list "Query:" (list-to-string query))
            (list "Fragment:" (list-to-string fragment)))
    
    (list (list "Scheme:" scheme)
          (list "Userinfo:" (list-to-string userinfo))
          (list "Host:" (list-to-string host))
          (list "Port:" port) ;;;SISTEMARE!! DEVE RITORNARE INTEGER
          (list "Path:" (list-to-string path))
          (list "Query:" (list-to-string query))
          (list "Fragment:" (list-to-string fragment)))
    )
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

(defun uri-display (uri-structure )
  (if (null uri-structure)
    t
    (progn
      (print-uri-element (first uri-structure) t)
      (uri-display (rest uri-structure) )
    )
  )
)

(defun print-uri-element (element out-stream)
  (format out-stream "~11A ~A~%" (first element) (second element))
)

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
             (make-uri "zos" (parse-rest (second scheme) "zos")))
            (t (make-uri (list-to-string (first scheme)) (parse-rest (second scheme) (list-to-string (first scheme)))))
            )
      )
    )
  )

(defun parse-mailto (lista)
  (if (null lista)
      (list nil nil nil nil nil nil nil)
    (let ((userinfo (userinfo-parse lista)))
      (if (eql (first (second userinfo)) #\@)
          (let ((host (host-parse (second userinfo))))
            (list (second host) (first userinfo) (first host) nil nil nil nil))
       ;(list (list "Userinfo:" (first userinfo)) (list "Host:" (host-parse (second userinfo))))
       ;(list (list "Userinfo:" (first userinfo)) (list "Host:" nil)))))
        (list (second userinfo) (first userinfo) nil nil nil nil nil)
        )
      )
    )
  )

(defun parse-news (lista)
  (if (null lista)
      (list nil nil nil nil nil nil nil)
    (let ((host (host-parse lista)))
      (list (second host) nil (first host) nil nil nil nil))
    ;(list "Host:" (first (host-parse lista)))
    )
  )

(defun parse-telfax (lista)
  (if (null lista)
      (list nil nil nil nil nil nil nil)
    (let ((userinfo (userinfo-parse lista)))
      (list (second userinfo) (first userinfo) nil nil nil nil nil))
    ;(list "Userinfo:" (first (userinfo-parse lista)))
    )
  )

;;; (defun parse-zos (lista)) ne abbiamo bisogno???

(defun parse-rest (lista scheme)
  (let ((authorithy (authorithy-parse lista)))
    (let ((path-query-fragment (path-query-fragment-parse (fourth authorithy) scheme)))
      (list (fourth path-query-fragment) (first authorithy) (second authorithy) (third authorithy)
            (first path-query-fragment) (second path-query-fragment)
            (third path-query-fragment))
      )
    )
  )

(defun authorithy-parse (lista)
  (if (and (eql (first lista) (second lista)) (eql (first lista) #\/))
      (let ((userinfo (userinfo-parse (rest (rest lista)) #\@)))
        (let ((host (host-parse (second userinfo))))
          (if (eq (first (second host)) #\:)
              (let ((port (port-parse (rest (second host)))))
                (list (first userinfo) (first host) (first port) (second port)))
            (list (first userinfo) (first host) nil (second host)))
            ))
    (list nil nil nil lista))
  )

(defun path-query-fragment-parse (lista scheme)
  (if (eq (first lista) #\/)
      (let ((path (path-parse-choice (rest lista) scheme)));;attenzione ad host-parse-choice!! path in zos è effettivamente obbligatorio?
        ;(write "path:")
        ;(write path)
        (if (eq (first (second path)) #\?)
            (let ((query (query-parse (second path))))
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
  (let ((userinfo (must-end-with (one-or-more-satisfying lista 'identificatorep) ends-with)))
    userinfo
   ; (if (and (eq (first userinfo) (second userinfo)) (eq (first userinfo) nil))
   ;     (list nil lista)
   ;   userinfo)
    )
  )

(defun host-parse (lista)
  (if (null lista)
      (list nil lista)
    (let ((identificatore (one-or-more-satisfying lista 'hostp)))
      ;(write "Identificatore:")
      ;(write identificatore)
      (if (eql (first (second identificatore)) #\.)
          (let ((risultato-ric-host-parse (host-parse (rest (second identificatore)))))
            ;(write "risultato-ric:")
            ;(write risultato-ric-host-parse)
            (list (append (append (first identificatore) (list #\.)) (first risultato-ric-host-parse)) (second risultato-ric-host-parse))) ; *
        (list (first identificatore) (second identificatore))
        )))
  )

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
           

(defun path-parse (lista)
  (if (null lista)
      (list nil lista)
    (let ((identificatore (one-or-more-satisfying lista 'identificatorep)))
      ;(write "Identificatore:")
      ;(write identificatore)
      (if (eql (first (second identificatore)) #\/)
          (let ((risultato-ric-path-parse (path-parse (rest (second identificatore)))))
            ;(write "risultato-ric:")
            ;(write risultato-ric-host-parse)
            (list (append (append (first identificatore) (list #\/)) (first risultato-ric-path-parse)) (second risultato-ric-path-parse))) ; *
        (list (first identificatore) (second identificatore))
        )))
  )

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

(defun one-or-more-satisfying (lista pred)
  (if (or (null lista) (not (funcall pred (first lista))))
      (list nil lista)
    (let ((risultato-ric (one-or-more-satisfying (rest lista) pred)))
      (list (cons (first lista) (first risultato-ric)) ;al posto della seconda "list" c'era un cons, ma dopo non potevo fare append #\. in host, alla riga con il commento "*"
            (second risultato-ric))))) 

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
