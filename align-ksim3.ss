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
	      (when (= 0 (modulo y 500))
		    (displayln y))
	      (for-each (lambda(x)
			  (let* ((c (if (equal?  (string-ref str1 (- x 1))
						 (string-ref str2 (- y 1)))
					0 1))
				       
				 (v1 (+ *gap-pen* (dp-ref (- x 1) y  len2)))
				 (v2 (+ *gap-pen* (dp-ref x (- y 1)  len2)))
				 (v3 (+ c (dp-ref  (- x 1) (- y 1)   len2)))
				 (val (min v1 v2 v3))
				 (dir (min-index-of (list v3 v1 v2) identity))
				 )
			    ;(displayln (format "~a,~a= ~a ~a ~a" x y v1 v2 v3))
			    
			    (dp-set! x y len2 val)
			    (tb-set! x y len2 dir)
			    
			  )
			  #t
			  );
			(iota len1 1)
			)
	      )
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
    ;(dump-dp)
    
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
			  ;'(4)
			  ))
	      (iota len2 1)
	      ;'(7)
	      )
    (displayln (sort (hash-map *res* (lambda(x y) x)) double-lessp))
    
    ))
;;--*-- test data --*--
(define s5 "ABCD")
(define s6 "ABCD")


(define (traceback x y k)
  (tracebackB x y *len2* k '()))

(define (tracebackB x y len2 k acc )
  (when *DEBUG*
	(displayln (format ">~a,~a[~a]" x y k)))
  (let ((nextacc (if (<= x k)
		     (cons y acc)
		     acc)))
    #t
    
    (if (or (and (= x 0)(= y 0))(< k 0))
	nextacc
	(case (tb-ref x y len2)
	      [(1) (tracebackB (- x 1) y len2 (- k 1) nextacc )]
	      [(2) (tracebackB x (- y 1) len2 (- k 1) nextacc )]
	      [(0) (let ((c (if (equal?  (string-ref *str1* (- x 1))
					 (string-ref *str2* (- y 1)))
				0 1)))
		     (tracebackB (- x 1)(- y 1) len2 (- k c) nextacc  ))]  ;; 2021/03/18 unmatch‚Ìˆ—
	      [else (displayln (format "error dir=~a" (tb-ref x y len2)))]
      ))
    
))
    
  

    
(define (dump-dp)
    (for-each (lambda (y)
		(for-each (lambda(x)
			    (display (format "~a " (dp-ref x y *len2*))))
			  (iota (+ 1 *len1*)))
		(display "\n"))
	      (iota (+ 1 *len2*)))
    )

(define (my-string-ref str n)
      (string-ref str (- n 1)))

(define (tracebackA x y k len2 acc acc1 acc2)
  (displayln (format ">~a,~a[~a]" x y k))
  (let ((nextacc (if (<= x k)
		     (cons x acc)
		     acc)))
    #t
    
    (if (or (and (= x 0)(= y 0))(< k 0))
	(begin
					;(displayln (format "trbk x=~a y=~a" x y))
	
	  (list acc acc1 acc2 )
        )
	(case (tb-ref x y len2)
	      [(1) (tracebackA (- x 1) y len2 (- k 1) nextacc (format "~a~a" (my-string-ref *str1* x) acc1)(format "-~a" acc2))]
	      [(2) (tracebackA x (- y 1) len2 (- k 1) nextacc (format "-~a" acc1)(format "~a~a" (my-string-ref *str2* y) acc2))]
	      [(0) (tracebackA (- x 1)(- y 1) len2 k nextacc
			       (format "~a~a" (my-string-ref *str1* x) acc1)
			       (format "~a~a" (my-string-ref *str2* y) acc2))]
	      [else (displayln (format "error dir=~a" (tb-ref x y len2)))]
      ))
    
))

;(solve-ksim-fast s5 s6 1)





