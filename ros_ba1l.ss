#lang racket
;; rosalind
;; Implement PatternToNumber
;; [BA1L] 2021/07/06 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(define *ba1l_out* "data\\ba1l_out.txt")

(define nuc-num '((#\A 0)
		  (#\C 1)
		  (#\G 2)
		  (#\T 3)))

(define (dna->num listdna acc)
  (if (null? listdna)
      acc
      (dna->num (cdr listdna)
		(+ (cadr (assoc (car listdna) nuc-num))
		   (* 4 acc)))))

(define (ros_ba1l . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba1l.txt"
		    (format "data\\rs_ba1l~a.txt" (car n)))))
       )
    (dna->num (string->list(car data)) 0)
    #|
    (call-with-output-file *maj_out*
      (lambda(out)
	(display data out))
      #:exists 'truncate/replace)
    |#
))

	 
				

