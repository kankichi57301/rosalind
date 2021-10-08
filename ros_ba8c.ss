#lang racket
;; rosalind
;; Implement the Lloyd Algorithm for k-Means Clustering
;; [BA8C] 2021/09/16 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba8c_out* "ba8c_out.txt")
(require "k-means.rkt") ;; github からコピー　;;https://github.com/qpwo/k-means-racket

(define *points* '())
(define *centers* '())

(define (ros_ba8c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba8c.txt"
		    (format "rs_ba8c~a.txt" (car n)))))
	 (firstline  (string->numlist (car data)))
	 (k (car firstline))
	 (m (cadr firstline))
	 (points  (map string->numlist (cdr data)))
	 (res '())
	 
	 )
    (set! *points* points)
    ;points
    
    (let-values ([(centers clusters)(cluster points k)])
        (set! res centers))

    
    (call-with-output-file *ba8c_out*
      (lambda(out)
	(for-each (lambda(point)(displayln (string-join (map (lambda(n)(number->string (round3 n)))point)) out))
		  res))
      #:exists 'truncate/replace)
    #t
    
))

(define (round3 num)
  (exact->inexact (/ (exact-round (* 1000 num)) 1000)))

