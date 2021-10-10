#lang racket
;;rosalind
;;Enumerating k-mers Lexicographically
;;[LEXF]
(require srfi/13) 
(require "readfileA.ss")

(define (my-permutation lst n)
  (if (= 1 n)
      (map list lst)
      (append-map (lambda(x)
	     (let ((p-1 (my-permutation lst (- n 1))))
	       (map
		(lambda(y)(cons x y)) p-1)))
	   lst)))
	     

(define *lexf_out* "data\\lexf_out.txt")

(define (ros_lexf . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_lexf.txt"
		    (format "data\\rs_lexf~a.txt" (car n)))))
	 (symbol-list (string-tokenize (car data)))
	 (num (string->number (cadr data)))
	 (res '())
	 )
    (set! res
	  (sort
	   (map (lambda(x)(apply string-append x))
		(my-permutation symbol-list num)) string<))
    
    (call-with-output-file *lexf_out*
      (lambda(out)
	(for-each (lambda(x)(displayln x out)) res))
	#:exists 'truncate/replace)
    res
))

