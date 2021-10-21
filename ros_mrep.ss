#lang racket
;; rosalind
;; Identifying Maximal Repeats
;; [MREP]
;; 2021/02/02 AC
;; 2021/10/21 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "suffix-tree2A.ss")
(define *mrep_out* "data\\mrep_out.txt")

(define (ros_mrep . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_mrep.txt"
		    (format "data\\rs_mrep~a.txt" (car n)))))
	 (my-string (string-append (car data) "$"))
	 (res "")
	 )

    (set! res (maximal-repeated-string my-string 20))

    (call-with-output-file *mrep_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
    res
    ))


