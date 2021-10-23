(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/function identity))
	(require srfi/13)
;;
;; semiglobal alignment
;; 2021/03/09 copied from align-fitting.ss
;;
(require srfi/1)
(require ffi/unsafe)

;; allocate dp & tb memory direcltly malloced
(define *dp* '())
(define *tb* '())
(define *max* 0)
(define *max-x* 0)
(define *max-y* 0)
(define *min-x* -1)
(define *min-y* -1)


(require "roslibA.ss")
(define *gap-pen* -1) ;; liner gap penalty

(define (init-semiglobal len1 len2)
  (let ((len1+ (+ 1 len1))
	(len2+ (+ 1 len2)))
    (set! *dp* (malloc _int32 (* len1+ len2+) ))
    (memset *dp* 0 (* len1+ len2+) _int32)
    (set! *tb* (malloc _int8  (* len1+ len2+) ))
    (memset *tb* 0 (* len1+ len2+) _int8)
    )
  (set! *max-x* 0)
  (set! *max-y* 0)
)

(define (dp-set! x y len2 val)
  (ptr-set! *dp* _int32 (+ (* x (+ 1 len2)) y) val))
(define (dp-ref x y len2)
  (ptr-ref *dp*  _int32 (+ (* x (+ 1 len2)) y)))
(define (tb-set! x y len2 val)
  (ptr-set! *tb* _int8  (+ (* x (+ 1 len2)) y) val))
(define (tb-ref x y len2)
  (ptr-ref *tb*  _int8  (+ (* x (+ 1 len2)) y) ))

(define (fill-semiglobal-table str1 str2 len1 len2)
  
  (for-each (lambda(y)
	      
	      (when (= 0 (modulo y 500))
		    (displayln y))
	      
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
			    (tb-set! x y len2 dir)
			    
			    (when (and (or (= x len1) (= y len2))(>= val *max*))      ;;fitting align
				  (begin
				    (set! *max* val)
				    (set! *max-x* x)
				    (set! *max-y* y)))
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
    )

(define *res* '())

(define (semiglobal-align str1 str2)
  (define (my-string-ref str n)
    (string-ref str (- n 1)))
  

  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	)
    (init-semiglobal len1 len2)
    (fill-semiglobal-table str1 str2 len1 len2)
    ;(dump-dp len1 len2)
        
    (define (fukugen x y acc1 acc2)
      ;(displayln (format ">~a,~a" x y))
      (if (or (= x 0)(= y 0))
	  (begin
	    (set! *min-x* x)
	    (set! *min-y* y)
	    ;(displayln (format "min>~a,~a" x y))
	    (list acc1 acc2)
	  )
	  (case (tb-ref x y len2)
	    [(1) (fukugen (- x 1) y     (cons (my-string-ref str1 x) acc1) (cons #\- acc2))]
	    [(2) (fukugen x (- y 1)     (cons #\- acc1) (cons (my-string-ref str2 y)acc2))]
	    [(0) (fukugen (- x 1)(- y 1)(cons (my-string-ref str1 x) acc1)(cons (my-string-ref str2 y)acc2))]
	    [else (displayln (format "error dir=~a" (tb-ref x y len2)))]
	    )
	  )
      )
    (define (fukugen* x y)
      (fukugen x y '() '()))

    ;(displayln (format "fkst ~a,~a" *max-x* *max-y*))
    
    (let* (
	   (res2 (fukugen* *max-x* *max-y*))
	   (res3 (map (lambda(x)(apply string x)) res2))
	   (dna1 (string-append (if (> *min-x* 0)
				    (string-take str1 *min-x*)
				    (make-string *min-y* #\-))
				(car res3)
				(if (> len1 *max-x*)
				    (string-take-right str1 (- len1 *max-x*))
				    (make-string (- len2 *max-y*) #\-)
				)))
	   (dna2 (string-append (if (> *min-y* 0)
				    (string-take str2 *min-y*)
				    (make-string *min-x* #\-))
				(cadr res3)
				(if (> len2 *max-y*)
				    (string-take-right str2 (- len2 *max-y*))
				    (make-string (- len1 *max-x*) #\-)
				    )))
	   
	  )
      ;(set! *res* res3)
      (list *max* dna1 dna2))))
    
    
   
   
    
    







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
;(semiglobal-align "TAGGCTTA" "TAGATA")
;;
)

