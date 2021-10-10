#lang racket
;; 
;; Perfect Matchings and RNA Secondary Structures
;; [PMCH]
(require "readfileA.ss")
(require srfi/1)
(require srfi/13)
(require math/number-theory)


(define (solve_pmch dnastr)
  (let* ((dna-list (string->list dnastr))
	 (a-count (length (indexes-of dna-list #\A)))
	 (c-count (length (indexes-of dna-list #\C))))
    (* (factorial a-count)(factorial c-count))))

(define *pmch_out* "data\\pmch_out.txt")

(define (ros_pmch . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_pmch.txt"
		    (format "data\\rs_pmch~a.txt" (car n)))))
	 (res 0)
	 )
    (set! res (solve_pmch (cadr data)))
    
    (call-with-output-file *pmch_out*
      (lambda(out)
	(display res  out))
      #:exists 'truncate/replace)
    res
))


