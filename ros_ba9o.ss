#lang racket
;; rosalind
;; Find All Approximate Occurrences of a Collection of Patterns in a String
;; [BA9O] 2021/12/26 AC
;;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(require "suffix-treeC.ss")
(define *ba9o_out* "data\\ba9o_out.txt")

(define (ros_ba9o . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba9o.txt"
		    (format "data\\rs_ba9o~a.txt" (car n)))))
	 (str (car data))
	 (patterns (string-tokenize (cadr data)))
	 (d (string->number (caddr data)))
	 (ans '())
	 )
    ;(displayln str)
    ;(displayln patterns)
    ;(displayln d)
    (set! ans (find-str-pos-M str patterns d))
    
    (call-with-output-file *ba9o_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  ans))
      #:exists 'truncate/replace)
    
    ans
))

