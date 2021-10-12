#lang racket
;; rosalind
;; Ordering Strings of Varying Length Lexicographically 
;; [LEXV] 2021/10/12 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *lexv_out* "data\\lexv_out.txt")

(define (ros_lexv . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_lexv.txt"
		    (format "data\\rs_lexv~a.txt" (car n)))))
	 (symbols (string-tokenize (car data)))
	 (n (string->number (cadr data)))
	 (res (map (lambda(x)(apply string-append x))				
		   (solve-lexv symbols n)))
	 )

    (call-with-output-file *lexv_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
))

(define (solve-lexv lst n)
  (if (= n 1)
      (map list lst)
      (append-map (lambda(x)
		    (map (lambda(y)(cons x y))
			 (cons '("") (solve-lexv lst (- n 1)))))
		  lst)))
