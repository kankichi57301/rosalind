(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/port port->lines))
	(require (only-in mzlib/string read-from-string))
	(require srfi/13)
	
(define (read-file n)
  (call-with-input-file (eval (string->symbol (format "*ros~a*" n)))
    (lambda(in)
      (port->lines in))))


(define (read-file* filename)
  (call-with-input-file filename
    (lambda(in)
      (port->lines in))))

(define (read-file+ filename)
  (call-with-input-file filename
    (lambda(in)
      (read in))))


;;
(define (edit-list-0 strlist pred acc)
  (if (null? strlist)
      acc
      (if (pred (car strlist))
	  (edit-list-0 (cdr strlist) pred (cons "" acc))
	  (edit-list-0 (cdr strlist) pred (cons (string-append (car acc)(car strlist))
						(cdr acc))))))


(define (edit-list strlist pred)
  (edit-list-0 strlist pred '()))

(define (header? str)
  (string=? ">" (string-take str 1)))
	 
(define (edit-fasta strlist)
  (reverse
   (edit-list strlist header?)))

(define (newick->sexp str)
  (read-from-string
  (regexp-replace
   #rx";"
    (regexp-replace* #rx"," str " ")
    "")))
;;
;; -*- test data --*--
;;
(define nw1 "(rat,(dog,cat),(rabbit,(elephant,mouse)));")
(define nw2 "(rat,(cat,dog),(elephant,(mouse,rabbit)));")
)
