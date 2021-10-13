#lang racket
;; rosalind
;; Introduction to Set Operations
;; [SETO] 2012/10/13 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(define *seto_out* "data\\seto_out.txt")

(define (ros_seto . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_seto.txt"
		    (format "data\\rs_seto~a.txt" (car n)))))
    	 (n (string->number (car data)))
	 (sets
	  (map (lambda(y)(map string->number y))
	       (map
		(lambda(x)(regexp-match* #rx"[0-9]+" x))(cdr data))))
	 (set1 (car sets))
	 (set2 (cadr sets))
	 (full (iota n 1))
	 )

    (call-with-output-file *seto_out*
      (lambda (out)
	 (display-list out (lset-union = set1 set2))
	 (display-list out (lset-intersection = set1 set2))
	 (display-list out (lset-difference = set1 set2))
	 (display-list out (lset-difference = set2 set1))
	 (display-list out (lset-difference = full set1))
	 (display-list out (lset-difference = full set2)))
      #:exists 'truncate/replace
      )
   ))

(define (display-list out lis)
  (display "{" out)
  (for-each (lambda(x)(display x out))
	    (add-between (sort lis <) ", "))
  (display "}\n" out)
  )

