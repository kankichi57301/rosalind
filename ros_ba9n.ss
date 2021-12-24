#lang racket
;; rosalind
;; Find All Occurrences of a Collection of Patterns in a String 
;; [BA9N] 2021/12/24 AC
;(require srfi/1)
(require srfi/13)
(require suffixtree)
(require "readfileA.ss")
(require "suffix-treeB.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba9n_out* "data\\ba9n_out.txt")

(define (ros_ba9n . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba9n.txt"
		    (format "data\\rs_ba9n~a.txt" (car n)))))
	 (res '())
	 )
    (displayln (format "strlen=~a" (string-length (car data))))
    (displayln (format "search count=~a" (length (cdr data))))
    (displayln (format "search strlen=~a" (string-length (cadr data))))
    (find-str-pos (format "~a$" (car data)) (cadr data))
    
    (set! res
	  (sort
	   (find-str-pos-M (format "~a$" (car data))(cdr data))
	   <))
    
    (call-with-output-file *ba9n_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
    res
    
))

;(ros_ba9n)
