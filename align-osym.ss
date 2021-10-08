;;
;; semiglobal alignment
;; 2021/03/09 copied from align-fitting.ss
;; 2021/03/30 AC
(require srfi/1)
(require ffi/unsafe)

;; allocate dp & tb memory direcltly malloced
(define *dp* '())
(define *dp2* '())
(define *matchall* 0)

(include "roslib.ss")
(define *gap-pen* -1) ;; liner gap penalty

(define (init-osym dp len1 len2)
  (let ((len1+ (+ 1 len1))
	(len2+ (+ 1 len2)))
  
    (memset dp 0 (* len1+ len2+) _int32)
    )
  
  
  (for-each (lambda(x)(dp-set! dp x 0 len2 (- x)))(iota len1 1))
  (for-each (lambda(y)(dp-set! dp 0 y len2 (- y)))(iota len2 1))
)

(define (dp-set! dp x y len2 val)
  (ptr-set! dp _int32 (+ (* x (+ 1 len2)) y) val))
(define (dp-ref dp x y len2)
  (ptr-ref dp  _int32 (+ (* x (+ 1 len2)) y)))

(define (fill-osym dp str1 str2 len1 len2)
  
  (for-each (lambda(y)
	      (for-each (lambda(x)
			  (let* ((c (if (equal?  (string-ref str1 (- x 1))
						 (string-ref str2 (- y 1)))
					1 -1))
				       
				 (v1 (+ *gap-pen* (dp-ref dp (- x 1) y  len2)))
				 (v2 (+ *gap-pen* (dp-ref dp x (- y 1)  len2)))
				 (v3 (+ c (dp-ref dp (- x 1) (- y 1)   len2)))
				 (val (max v1 v2 v3))
				 ;(dir (max-index-of (list v3 v1 v2) identity))
				 )
			    
			    
			    (dp-set! dp x y len2 val)
			    
			  )
			  #t
			  )
			(iota len1 1))
	      )
  (iota len2 1)))
;;			  
;;
;;

(define (dump-dp dp len1 len2 )
    (for-each (lambda (y)
		(for-each (lambda(x)
			    (display (format "~a " (dp-ref dp x y len2))))
			  (iota (+ 1 len1)))
		(display "\n"))
	      (iota (+ 1 len2)))
    (displayln "-------")
    )
    

(define *res* '())

(define (osym-align str1 str2)
  (define (my-string-ref str n)
    (string-ref str (- n 1)))
  

  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	)
    (set! *dp* (malloc _int32 (* (+ len1 1)(+ len2 1)) ))
    (set! *dp2* (malloc _int32 (* (+ len1 1)(+ len2 1)) ))
    
    (init-osym *dp* len1 len2)
    (fill-osym *dp* str1 str2 len1 len2)
    (init-osym *dp2* len1 len2)
    (fill-osym *dp2*
	       (string-reverse str1)
	       (string-reverse str2)
	       len1 len2)
    ;(dump-dp *dp* len1 len2)
    ;(dump-dp *dp2* len1 len2)
    
    (displayln (dp-ref *dp* len1 len2 len2))
    (displayln (+ (sumdp  *dp* len1 len2)
		  (sumdp  *dp2* len1 len2)
		  (sum-match str1 str2 len1 len2)))
))
;;
;;
;;
(define (sumdp dp len1 len2 )
  (let ((sum 0))
    (for-each (lambda (y)
		(for-each (lambda(x)
			    (set! sum (+ sum (dp-ref dp x y len2))))
		     (iota len1)))
	      (iota  len2 ))
    sum))



(define (sum-match str1 str2 len1 len2 )
  (let ((sum 0))
    (for-each (lambda (y)
		(for-each (lambda(x)
			    (set! sum (+ sum (if (equal? (string-ref str1 x)
							(string-ref str2 y))
						 1 -1))))
		     (iota  len1)))
	      (iota   len2 ))
    sum))


