;;
;; alignment for ksim
;; 2021/03/09 copied from align-semigrobal.ss
;;
(require srfi/1)
(require ffi/unsafe)
(include "roslib.ss")
(include "test-ksim.ss")

;; allocate dp & tb memory direcltly malloced
(define *dp* '())
(define *tb* '())
(define *y-min* 0)
(define *str1* "")
(define *str2* "")
(define *len1* 0)
(define *len2* 0)
(define *DEBUG* #f)
(define *DUMP* #f)

(include "roslib.ss")
(define *gap-pen* 1) ;; liner gap penalty

(define (init-ksim len1 len2)
  (let ((len1+ (+ 1 len1))
	(len2+ (+ 1 len2)))
    (set! *dp* (malloc _int32 (* len1+ len2+) ))
    (memset *dp* 0 (* len1+ len2+) _int32)
    (for-each (lambda(x)(dp-set! x 0 len2 0))(iota len1 1))
    (for-each (lambda(y)(dp-set! 0 y len2 0))(iota len2 1))
    (set! *tb* (malloc _int8  (* len1+ len2+) ))
    (memset *tb* 0 (* len1+ len2+) _int8)
    (for-each (lambda(x)(tb-set! x 0 len2 1))(iota len1 1))
    (for-each (lambda(y)(tb-set! 0 y len2 2))(iota len2 1))
    
    )
)

(define (dp-set! x y len2 val)
  (ptr-set! *dp* _int32 (+ (* x (+ 1 len2)) y) val))
(define (dp-ref x y len2)
  (ptr-ref *dp*  _int32 (+ (* x (+ 1 len2)) y)))
(define (tb-set! x y len2 val)
  (ptr-set! *tb* _int8  (+ (* x (+ 1 len2)) y) val))
(define (tb-ref x y len2)
  (ptr-ref *tb*  _int8  (+ (* x (+ 1 len2)) y) ))

(define (fill-ksim-table str1 str2 len1 len2)
  (for-each (lambda(y)
	      ;(when (= 0 (modulo y 500))
	      ;    (displayln y))
	      (for-each (lambda(x)
			  (let* ((c (if (equal?  (string-ref str1 (- x 1))
						 (string-ref str2 (- y 1)))
					0 1))
			    
				 (v1 (+ *gap-pen* (dp-ref (- x 1) y  len2)))
				 (v2 (+ *gap-pen* (dp-ref x (- y 1)  len2)))
				 (v3 (+ c (dp-ref  (- x 1) (- y 1)   len2)))
				 (val (min v1 v2 v3))
				 (dir (min-index-every   v1 v2 v3)))
			    
			    ;(displayln (format "~a,~a= ~a ~a ~a" x y v1 v2 v3))
			    
			    (dp-set! x y len2 val)
			    (tb-set! x y len2 dir)
			  )
			)
		       (iota len1 1)
		))
	    (iota len2 1)
	    ))
;;			  
;;
;;



(define *res* '()) 
(define (solve-ksim-fast str1 str2 k)
 (let ((len1 (string-length str1))
       (len2 (string-length str2))
       )
   (set! *str1* str1)
   (set! *str2* str2)
   (set! *len1* len1)
   (set! *len2* len2)
   
    (init-ksim len1 len2)
    (fill-ksim-table str1 str2 len1 len2)
    (when *DUMP*
	  (dump-dp)
	  (dump-tb)
    )
    (set! *res* (make-hash))
    (define (conv-start-loc startlist x)
      (map (lambda(st)(list (+ st 1) (- x st ))) startlist))
        
    (for-each (lambda(y)
		(for-each (lambda(x)
			    ;(displayln (format "xy=~a,~a" x y)) 
			    
			    (let* ((sc (dp-ref x y len2))
				   (startlist (if (<= sc k)
						  (traceback x y (- k  (- len1 x)))  ;; 2021/03/17
					      '())))
			      (when (not (null? startlist))
				    ;(displayln (format "xy=~a,~a sl=~a" x y startlist))
				    (for-each (lambda(x)(hash-set! *res* x #f)) (conv-start-loc startlist y))
			    
				    )))
			  (iota (+ k 1) len1 -1)
			  
			  ))
	      (iota len2 1)
	      
	      )
    (sort (hash-map *res* (lambda(x y) x)) double-lessp)
    
    ))
;;--*-- test data --*--
(define s5 "ABCD")
(define s6 "ABCD")

(define *res2* '())
(define (traceback x y k)
  (set! *res2* '())
  (tracebackC x y *len2* k )
  *res2*
  )

(define (tracebackC x y len2 k )
  (when *DEBUG*
	(displayln (format ">~a,~a[~a][~a]" x y k (tb-ref x y len2))))
  (when (<= x k)
	(set! *res2* (cons y *res2*)))
		     
  (if (or (and (= x 0)(= y 0))(< k 0))
      #t
      (let((dir (tb-ref x y len2)))
	(when (> (bitwise-and dir 1) 0)
	      (tracebackC (- x 1) y len2 (- k 1) ))
	(when (> (bitwise-and dir 2) 0)
	      (tracebackC x (- y 1) len2 (- k 1) ))
	(when (> (bitwise-and dir 4) 0)
	      (let ((c (if (equal?  (string-ref *str1* (- x 1))
				    (string-ref *str2* (- y 1)))
			   0 1)))
		(tracebackC (- x 1)(- y 1) len2 (- k c) )))  ;; 2021/03/18 unmatchÇÃèàóù
       ))
)
    
  

    
(define (dump-dp)
  (display "- - ")
  (for-each (lambda(x)(display (format "~a " (string-ref *str1* x))))
	    (iota *len1*))
  (display "\n")
  (for-each (lambda (y)
	      (display (if (> y 0)
			   (format "~a " (string-ref *str2* (- y 1)))
		           "- "))
	      (for-each (lambda(x)
			  ;(display (format "~a " (dp-ref x y *len2*))))
			  
			  (if (and (> x 0)
				   (> y 0)
				   (equal? (string-ref *str1*  (- x 1))
					   (string-ref *str2*  (- y 1))))
			      (display (format "\u001B[42m~a \u001B[49m" (dp-ref x y *len2*)))
			      (display (format "~a " (dp-ref x y *len2*)))))
			  
			(iota (+ 1 *len1*)))
	      (display "\n"))
	    (iota (+ 1 *len2*))))
;;--*-- dump tb --*--
(define (dump-tb)
  (display "- - ")
  (for-each (lambda(x)(display (format "~a " (string-ref *str1* x))))
	    (iota *len1*))
  (display "\n")
  (for-each (lambda (y)
	      (display (if (> y 0)
			   (format "~a " (string-ref *str2* (- y 1)))
		           "- "))
	      (for-each (lambda(x)
			  ;(display (format "~a " (dp-ref x y *len2*))))
			  
			  (if (and (> x 0)
				   (> y 0)
				   (equal? (string-ref *str1*  (- x 1))
					   (string-ref *str2*  (- y 1))))
			      (display (format "\u001B[43m~a \u001B[49m" (tb-ref x y *len2*)))
			      (display (format "~a " (tb-ref x y *len2*)))))
			  
			(iota (+ 1 *len1*)))
	      (display "\n"))
	    (iota (+ 1 *len2*))))
  


(define (my-string-ref str n)
      (string-ref str (- n 1)))



;(solve-ksim-fast s5 s6 1)

;; ç≈è¨ílÇ™ÇQÇ¬à»è„ÇÃèÍçáÇÕbitpatternÇ≈ï‘Ç∑ÅB
(define (min-index-every v1 v2 v3)
  (cond  [(= v1 v2 v3) 7]
	 [(and (= v1 v2)(< v1 v3))    3]
	 [(and (= v2 v3)(< v2 v1))    6]
	 [(and (= v3 v1)(< v3 v2))    5] 
	 [(and (< v1 v2)(< v1 v3))    1]
	 [(and (< v2 v3)(< v2 v1))    2]
	 [(and (< v3 v1)(< v3 v1))    4]
  ))



