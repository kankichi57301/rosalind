#lang racket
;; rosalind
;; Implement BWMatching 
;; [BA9L] 2022/01/29 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "bwtB.ss")

(define *ba9l_out* "data\\ba9l_out.txt")

(define (ros_ba9l . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba9l.txt"
		    (format "data\\rs_ba9l~a.txt" (car n)))))
	 (strlist (string->list (car data)))
	 (search-list (string-tokenize (cadr data)))
	 (ans '())
	 )
    
    (displayln (length strlist))
    (set! ans (find-count-M strlist search-list))
    
    (call-with-output-file *ba9l_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  ans))
      #:exists 'truncate/replace)
    
    ans
   
))

