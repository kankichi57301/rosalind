#lang racket
;; rosalind
;; Find a Median String
;; [BA2B] 2021/07/11 AC
;(require srfi/1)
(require srfi/13)
(require "roslibA.ss")
(require "readfileA.ss")
(define *ba2b_out* "data\\ba2b_out.txt")
(define *dnas* '())

(define (ros_ba2b . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba2b.txt"
		    (format "data\\rs_ba2b~a.txt" (car n)))))
	 (k (string->number (car data)))
	 (dnas (map string->list (cdr data)))
	 (len (string-length (cadr data)))
	 (res (apply string (car (min-item
				  (map (lambda(x)(list x (sum-min-ham-dist dnas x)))
				       (all-dnas-list k))
				  cadr))))
	 )

    (call-with-output-file *ba2b_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
    
    res
 ))

;; --*--
(define (min-ham-dist lst pat)
  (min-ham-dist0 lst pat (length lst) (length pat)))

(define (min-ham-dist0 lst pat lstlen patlen)
  ;(displayln (format "arg=~a[~a]" lst lstlen))
  (if (< lstlen patlen)
      999999
      (min (list-hamming-distance lst pat)
	   (min-ham-dist0 (cdr lst) pat (- lstlen 1) patlen))))

(define (sum-min-ham-dist lstlst pat)
  (apply + (map (lambda(lst)(min-ham-dist lst pat)) lstlst))) 
