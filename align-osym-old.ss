;;
;; semiglobal alignment
;; 2021/03/09 copied from align-fitting.ss
;;
(require srfi/1)
(require ffi/unsafe)

;; allocate dp & tb memory direcltly malloced
(define *dp* '())


(include "roslib.ss")
(define *gap-pen* -1) ;; liner gap penalty

(define (init-osym len1 len2)
  (let ((len1+ (+ 1 len1))
	(len2+ (+ 1 len2)))
    (set! *dp* (malloc _int32 (* len1+ len2+) ))
    (memset *dp* 0 (* len1+ len2+) _int32)
    ;(set! *tb* (malloc _int8  (* len1+ len2+) ))
    ;(memset *tb* 0 (* len1+ len2+) _int8)
    )
  
  
  (for-each (lambda(x)(dp-set! x 0 len2 (- x)))(iota len1 1))
  (for-each (lambda(y)(dp-set! 0 y len2 (- y)))(iota len2 1))
  ;(for-each (lambda(x)(tb-set! x 0 len2 1))    (iota len1 1))
  ;(for-each (lambda(y)(tb-set! 0 y len2 2))    (iota len2 1))
)

(define (dp-set! x y len2 val)
  (ptr-set! *dp* _int32 (+ (* x (+ 1 len2)) y) val))
(define (dp-ref x y len2)
  (ptr-ref *dp*  _int32 (+ (* x (+ 1 len2)) y)))
;(define (tb-set! x y len2 val)
;  (ptr-set! *tb* _int8  (+ (* x (+ 1 len2)) y) val))
;(define (tb-ref x y len2)
;  (ptr-ref *tb*  _int8  (+ (* x (+ 1 len2)) y) ))

(define (fill-osym str1 str2 len1 len2)
  
  (for-each (lambda(y)
	      (for-each (lambda(x)
			  (let* ((c (if (equal?  (string-ref str1 (- x 1))
						 (string-ref str2 (- y 1)))
					1 -1))
				       
				 (v1 (+ *gap-pen* (dp-ref (- x 1) y  len2)))
				 (v2 (+ *gap-pen* (dp-ref x (- y 1)  len2)))
				 (v3 (+ c (dp-ref  (- x 1) (- y 1)   len2)))
				 (val (max v1 v2 v3))
				 (dir (max-index-of (list v3 v1 v2) identity))
				 )
			    ;(displayln (format "~a,~a= ~a ~a ~a" x y v1 v2 v3))
			    
			    (dp-set! x y len2 val)
			    
			  )
			  #t
			  )
			(iota len1 1))
	      )
  (iota len2 1)))
;;			  
;;
;;

(define (dump-dp len1 len2 )
    (for-each (lambda (y)
		(for-each (lambda(x)
			    (display (format "~a " (dp-ref x y len2))))
			  (iota (+ 1 len1)))
		(display "\n"))
	      (iota (+ 1 len2)))
    (displayln "-------")
    )
#|
(define (dump-tb len1 len2 )
    (for-each (lambda (y)
		(for-each (lambda(x)
			    (display (format "~a " (tb-ref x y len2))))
			  (iota (+ 1 len1)))
		(display "\n"))
	      (iota (+ 1 len2))))
|#
    

(define *res* '())

(define (osym-align str1 str2)
  (define (my-string-ref str n)
    (string-ref str (- n 1)))
  

  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	)
    (init-osym len1 len2)
    (fill-osym str1 str2 len1 len2)
    ;(dump-dp len1 len2)
    ;(dump-tb len1 len2)
    (dp-ref len1 len2 len2)
))
;; Å‰‚Ì‚P•¶š‚ğ‹­§“I‚É‡‚í‚¹‚é
(define (osym-align-force-head-match s1 s2)
  (+ (if (equal? (string-ref s1 0)
		 (string-ref s2 0))
	 1 -1)
     (osym-align (substring s1 1)
		 (substring s2 1))))

(define (calcM s t i j)
  (let* ((s0 (substring s 0 (- i 1)))
	 (s1 (substring s (- i 1)))
	 (t0 (substring t 0 (- j 1)))
	 (t1 (substring t (- j 1)))
	 (ans (+ (osym-align s0 t0)
		 (osym-align-force-head-match s1 t1)))
	)
    ;(displayln (list s0 s1 t0 t1 ans))
    ans
    
    ))
    
	
(define (M-nthline s t n)
  (map (lambda(x)(calcM s t n x))(iota (string-length s) 1)))
    
(define (mismatch-score0 list1 list2)
  ;(displayln (format "~a:~a" list1 list2))
  (if (or (null? list1)(null? list2))
      0
      (let ((c1 (car list1))
	    (c2 (car list2)))
	(+
	 (if (or (equal? c1 #\-)(equal? c2 #\-))
	     -1
	     (if (equal? c1 c2)
		1
		-1))
	 (mismatch-score0 (cdr list1)(cdr list2))))))

(define (mismatch-score str1 str2)
  (mismatch-score0 (string->list str1)
		   (string->list str2)))
;;
;;
;;(apply + (flatten (map (lambda(x)(M-nthline str1 str2 x))(iota 7 1))))
(define (M-sum str1 str2)
	       (apply + (flatten (map (lambda(x)(M-nthline str1 str2 x))(iota (string-length str2) 1)))))
