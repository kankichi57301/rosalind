;;
;; semiglobal alignment
;; 2021/03/09 copied from align-fitting.ss
;;
(require srfi/1)

(define *dp* '())
(define *tb* '())
(define *max* 0)
(define *max-x* 0)
(define *max-y* 0)
(define *min-x* -1)
(define *min-y* -1)


(include "roslib.ss")
(define *gap-pen* -1) ;; liner gap penalty

(define (init-semiglobal len1 len2)
  (set! *dp* (make-hash))
  (set! *tb* (make-hash)) ;; dir hash

  (for-each (lambda(x)(hash-set! *dp* `(,x 0) 0))(iota (+ 1 len1)))
  (for-each (lambda(y)(hash-set! *dp* `(0 ,y) 0))(iota (+ 1 len2)))
  ;(for-each (lambda(x)(hash-set! *tb* `(,x 0) 1))(iota (+ 1 len1)))
  ;(for-each (lambda(y)(hash-set! *tb* `(0 ,y) 2))(iota (+ 1 len2)))
  )

(define (fill-semiglobal-table str1 str2 len1 len2)
  (for-each (lambda(y)
	      (when (= 0 (modulo y 100))
		    (displayln y))
	      
	      (for-each (lambda(x)
			  (let* ((c (if (equal?  (string-ref str1 (- x 1))
						 (string-ref str2 (- y 1)))
					1 -1))
				    
				       
				 (v1 (+ *gap-pen* (hash-ref *dp* `(,(- x 1) ,y))))
				 (v2 (+ *gap-pen* (hash-ref *dp* `(,x  ,(- y 1)))))
				 (v3 (+ c 
					(hash-ref *dp* `(,(- x 1) ,(- y 1)))))
				 (val (max v1 v2 v3))
				 (dir (max-index-of (list v1 v2 v3) identity))
				 )
			    (hash-set! *dp*  (list x y) val)
			    (hash-set! *tb*  (list x y) dir)
			    (when (and (or (= x len1) (= y len2))(>= val *max*))      ;;fitting align �Q�ڂ̕�����͑S�����킹��
				  (begin
				    (set! *max* val)
				    (set! *max-x* x)
				    (set! *max-y* y)))

			  ;(displayln (list x y val))
			  ))

			(iota len1 1)))
  (iota len2 1)))
;;			  
;;--*-- tbc
;;
(define *res* '())

(define (semiglobal-align str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	)
    (init-semiglobal len1 len2)
    ;(displayln "INIT OK")
    (fill-semiglobal-table str1 str2 len1 len2)
    ;(displayln "FILL OK")
    ;;(dump-dp len1 len2)
    
    (define (my-string-ref str n)
      (string-ref str (- n 1)))
    
    (define (fukugen len1 len2 acc1 acc2)
      ;(displayln (format "~a,~a" len1 len2))
      (if (or (= len1 0)(= len2 0))
	  (begin
	    (set! *min-x* len1)
	    (set! *min-y* len2)
	    (list acc1 acc2)
	  )
	  (case (hash-ref *tb* (list len1 len2))
	    [(0) (fukugen (- len1 1) len2     (cons (my-string-ref str1 len1) acc1) (cons #\- acc2))]
	    [(1) (fukugen len1 (- len2 1)     (cons #\- acc1) (cons (my-string-ref str2 len2)acc2))]
	    [(2) (fukugen (- len1 1)(- len2 1)(cons (my-string-ref str1 len1) acc1)(cons (my-string-ref str2 len2)acc2))]

	    )
	  )
      )
    (define (fukugen* len1 len2)
      (fukugen len1 len2 '() '()))

    ;(displayln (format "~a,~a:~a" *max-x* *max-y* *max*))
    
    (let* (
	   (res2 (fukugen* *max-x* *max-y*))
	   (res3 (map (lambda(x)(apply string x)) res2))
	   (dna1 (string-append (if (> *min-x* 0)
				    (string-take str1 *min-x*)
				    (make-string *min-y* #\-))
				(car res3)
				(if (> len1 *max-x*)
				    (string-take str1 (- len1 *max-x*))
				    (make-string (- len2 *max-y*) #\-)
				)))
	   (dna2 (string-append (if (> *min-y* 0)
				    (string-take str2 *min-y*)
				    (make-string *min-x* #\-))
				(cadr res3)
				(if (> len2 *max-y*)
				    (string-take str2 (- len2 *max-y*))
				    (make-string (- len1 *max-x*) #\-)
				    )))
	   
	  )
      ;(set! *res* res3)
      (list *max* dna1 dna2)
    )
    ;(dump-dp len1 len2)
))


(define (dump-hash)
  (sort
   (hash-map *dp* list)
   (lambda (x y) (if (= (caar x)(caar y))
		     (< (cadar x)(cadar y))
		     (< (caar x)(caar y)))))
)



(define (dump-dp len1 len2)
  (for-each
   (lambda(x)
     (for-each
      (lambda(y)
	   (display (format "~a " (hash-ref *dp* `(,x ,y) -1))))
      (iota (+ 1 len2) 0))
     (display "\n"))
   (iota (+ 1 len1) 0))
)

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
;; (mismatch-score "TAGGCTTA" "TAGA--TA")
;;
