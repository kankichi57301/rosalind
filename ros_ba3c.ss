#lang racket
;; rosalind
;; Construct the Overlap Graph of a Collection of k-mers
;; [BA3C] 2021/07/11 AC 
;(require srfi/1)
(require srfi/13)
(require srfi/19)
(require "readfileA.ss")
(define *ba3c_out* "data\\ba3c_out.txt")
(define *time* #f)
(define *hash* #f)

(define (ros_ba3c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba3c.txt"
		    (format "data\\rs_ba3c~a.txt" (car n)))))
	 (len (string-length (car data)))
	 (res '())
	 ( data1 (sort data (lambda(x y)(string<? (string-drop x 1)(string-drop y 1)))))
	 ( data2 (sort data (lambda(x y)(string<? (string-drop-right x 1)(string-drop-right y 1)))))
	 (res '())
	 )
    ;(displayln data1)
    ;(displayln data2)
    
    (set! *time* (current-time))
    (set! *hash* (make-hash))
    (sel-matched data1 data2 pred3)
    (set! res (hash-keys *hash*))
    
    
    (call-with-output-file *ba3c_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a -> ~a" (car x)(cadr x)) out))
		  res))
      #:exists 'truncate/replace)
    
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
    #t
))

(define (pred3 x y)
  (let ((s1 (string-drop x 1))
	(s2 (string-drop-right y 1)))
    (if (string=? s1 s2)
	0
	(if (string<? s1 s2)
	    1 -1))))


(define (sel-matched lis1 lis2 pred)
  (if (or (empty? list)(empty? lis2))
      #f
      (let ((cmp (pred (car lis1)(car lis2))))
	(if (= 0 cmp)
	    (begin
	      (hash-set! *hash* (list (car lis1)(car lis2)) #t)
	      (sel-matched lis1 (cdr lis2) pred)
	      )
	    (if (positive? cmp) 
		(sel-matched (cdr lis1)  lis2 pred)
		(sel-matched lis1  (cdr lis2) pred))))))
