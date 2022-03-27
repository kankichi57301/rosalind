#lang racket
;; rosalind
;; Solve the Turnpike Problem
;; [BA4M] 2021/
(require racket/include)

(require srfi/1)
(require srfi/13)
(require srfi/19)

(include "pdp.ss")

(require "readfileA.ss")
(require "roslibA.ss")

(define *ba4m_out* "data\\ba4m_out.txt")
;(define *time* #f)

(define (ros_ba4m . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba4m.txt"
		    (format "data\\rs_ba4m~a.txt" (car n)))))
	 (nlist (map string->number (string-tokenize (car data))))
	 (nlist+  (filter positive? nlist))
	 (res '())
	 )
    (displayln (format "len=~a" (length nlist+)))
    (set! *time* (current-time))
    (solve-pdp nlist+)
    (set! res (sort *ans* <))
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
    
    
    (call-with-output-file *ba4m_out*
      (lambda(out)
	(for-each (lambda(n)
		    (display (format "~a " n) out))
		  res))

      #:exists 'truncate/replace)
    					;res
     
))

;;
(define (mytest2)(ros_ba4m 3))
(require profile)
;;(profile-thunk mytest2)
