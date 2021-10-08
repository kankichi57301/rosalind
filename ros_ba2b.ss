;; rosalind
;; Find a Median String
;; [BA2B] 2021/07/11 AC
;(require srfi/1)
(require srfi/13)
(include "roslib.ss")
(include "readfile.ss")
(define *ba2b_out* "ba2b_out.txt")
(define *dnas* '())

(define (ros_ba2b . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba2b.txt"
		    (format "rs_ba2b~a.txt" (car n)))))
	 (k (string->number (car data)))
	 (dnas (map string->list (cdr data)))
	 (len (string-length (cadr data)))
	 (res (apply string (car (min-item
				  (map (lambda(x)(list x (sum-min-ham-dist dnas x)))
				       (all-dnas-list k))
				  cadr))))
	 )

    (displayln res)
    #|
    (call-with-output-file *maj_out*
      (lambda(out)
	(display data out))
      #:exists 'truncate/replace)
    |#
    #t
 ))

;; --*--
(ros_ba2b 1)

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
