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
(defun list-to-string (list)
  (coerce list 'string))

;(set  'lista (string-to-list stringa))

(defun uri-parse (stringa)
  (list "URI:" (the-uri-parse (string-to-list (string-downcase stringa))))
  )

(defun the-uri-parse (lista)
  (let ((scheme (scheme-parse lista)))
    (let ((scheme-string (list-to-string (first scheme))))
      (write (second scheme))
      (cond ((string= scheme-string "mailto") 
             (cons (list "Scheme:" "mailto") (parse-mailto (second scheme))))
            ((string= scheme-string "news") 
             (append (list "Scheme:" "news") (parse-news (second scheme))))
            ((string= scheme-string "tel")
             (append (list "Scheme:" "tel") (parse-telfax (second scheme))))
            ((string= scheme-string "fax")
             (append (list "Scheme:" "fax") (parse-telfax (second scheme))))
            ((string= scheme-string "zos")
             (append (list "Scheme:" "zos") (parse-zos (second scheme))))
            (t (append (list "Scheme:" scheme-string) (parse-rest (second scheme))))
            )
      )
    )
  )

(defun parse-mailto (lista)
  (if (null lista)
      (list (list "Userinfo:" nil) (list "Host:" nil))
    (let ((userinfo (userinfo-parse lista)))
      (if (eql (first (second userinfo)) #\@)
          (list (list "Userinfo:" (first userinfo)) (list "Host:" host-parse (second userinfo)))
        (list (list "Userinfo:" (first userinfo)) (list "Host:" nil)))))
  )

(defun parse-news (lista)
  (if (null lista)
      (list "Host:" nil)
    (list "Host:" (first (host-parse lista)))
    )
  )

(defun parse-telfax (lista)
  (if (null lista)
      (list "Userinfo:" nil)
    (list "Userinfo:" (first (userinfo-parse lista)))
    )
  )
(defun scheme-parse (lista)
  (must-end-with (one-or-more-satisfying lista 'identificatorep) #\:))

(defun userinfo-parse (lista &optional ends-with)
  (must-end-with (one-or-more-satisfying lista 'identificatorep) ends-with))

(defun host-parse (lista)
  ;to be defined
  nil)

(defun port-parse (lista)
  (one-or-more-satisfying lista 'digitp))

(defun zos-path-parse (lista)
  ;to be defined
  nil)

(defun path-parse (lista)
  ;to be defined
  nil)

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

(defun digitp (char)
  (and (char<= char #\9) (char>= char #\0))
  )

(defun queryp (char)
  (char/= char #\#))

(defun any (char)
  t)

(defun one-or-more-satisfying (lista pred)
  (if (or (null lista) (not (funcall pred (first lista))))
      (list nil lista)
    (let ((risultato-ric (one-or-more-satisfying (rest lista) pred)))
      (list (cons (first lista) (first risultato-ric))
            (second risultato-ric))))) 

(defun must-end-with (lista char)
  (if (eq char nil)
      lista
    (if (eq (first (second lista)) char)
        (list (first lista) (rest (second lista)))
      (list nil (rest (second lista))))
    )
  )
;;; end of file -- uri_parse.pl