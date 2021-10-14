#lang racket
;; rosalind
;; Implement DistanceBetweenPatternAndStrings
;; [BA2H] 2021/07/11 AC
;; 2012/10/14 AC
;(require srfi/1)
(require srfi/13)
(require "roslibA.ss")
(require "readfileA.ss")
(define *ba2h_out* "data\\ba2h_out.txt")
(define *dnas* '())

(define (ros_ba2h . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba2h.txt"
		    (format "data\\rs_ba2h~a.txt" (car n)))))

	 (pat  (string->list (car data)))
	 (dnas  (map string->list (string-tokenize (cadr data))))
	 (res  (sum-min-ham-dist dnas pat))
	 )

    ;(displayln data)
    ;(displayln dnas)
    ;(displayln res)
    #|
    (call-with-output-file *ba2b_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
    |#
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
