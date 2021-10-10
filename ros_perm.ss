#lang racket
;;rosalind
;;Enumerating Gene Orders
;;[PERM]
(require srfi/1)
(require (only-in math/number-theory factorial))
(require srfi/13)
(require "readfileA.ss")
(define *perm_out* "data/perm_out.txt")

(define (ros_perm . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data/rosalind_perm.txt"
		    (format "data/rs_perm~a.txt" (car n)))))
	 (n (string->number (car data)))
	 )
        
    (call-with-output-file *perm_out*
      (lambda(out)
	(solve-perm n out))
	#:exists 'truncate/replace)
    #t
))

(define (solve-perm n out)
  (displayln (factorial n) out)
  (for-each (lambda(x)
	 (for-each (lambda(y) (display (format "~s " y) out)) x)
	 (display "\n" out))
 (permutations (iota n 1))))
