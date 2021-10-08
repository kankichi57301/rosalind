;;
;; local alignment
;; 2021/03/01 copied from editdist2.ss
;;
(require srfi/1)
(include "PAM250.ss")
(define *dp* '())
(define *tb* '())
(define *max* 0)
(define *max-x* 0)
(define *max-y* 0)


(include "roslib.ss")
(define *gap-pen* -5) ;; liner gap penalty

(define (init-local len1 len2)
  (set! *dp* (make-hash))
  (set! *tb* (make-hash)) ;; dir hash

  (for-each (lambda(x)(hash-set! *dp* `(,x 0) 0))(iota (+ 1 len1)))
  (for-each (lambda(y)(hash-set! *dp* `(0 ,y) 0))(iota (+ 1 len2)))
  ;(for-each (lambda(x)(hash-set! *tb* `(,x 0) 1))(iota (+ 1 len1)))
  ;(for-each (lambda(y)(hash-set! *tb* `(0 ,y) 2))(iota (+ 1 len2)))
  )

(define (fill-local-table str1 str2 len1 len2)
  (for-each (lambda(y)
	      (begin
		#|
		(when (= 0 (modulo y 100))
		      (displayln y))
		|#
	      (for-each (lambda(x)
			  (let* ((c (pam250score (string-ref str1 (- x 1))
						 (string-ref str2 (- y 1))))
				       
				 (v1 (+ *gap-pen* (hash-ref *dp* `(,(- x 1) ,y))))
				 (v2 (+ *gap-pen* (hash-ref *dp* `(,x  ,(- y 1)))))
				 (v3 (+ c 
					(hash-ref *dp* `(,(- x 1) ,(- y 1)))))
				 (val (max v1 v2 v3 0))
				 (dir (max-index-of (list v1 v2 v3 0) identity))
				 )
			    (hash-set! *dp*  (list x y) val)
			    (hash-set! *tb*  (list x y) dir)
			    (when (> val *max*)
				  (begin
				    (set! *max* val)
				    (set! *max-x* x)
				    (set! *max-y* y)))

			  ;(displayln (list x y val))
			  ))

			(iota len1 1))))
  (iota len2 1)))
;;			  
;;--*-- tbc
;;
(define *res* '())

(define (local-align str1 str2)
 (let ((len1 (string-length str1))
	(len2 (string-length str2))
	)
    (init-local len1 len2)
    ;(displayln "INIT OK")
    (fill-local-table str1 str2 len1 len2)
    ;(displayln "FILL OK")

    (define (my-string-ref str n)
      (string-ref str (- n 1)))
    
    (define (fukugen len1 len2 acc1 acc2)
      ;(displayln (format "~a,~a" len1 len2))
      (if (or (= len1 0)(= len2 0))
	  (list acc1 acc2)
	  (case (hash-ref *tb* (list len1 len2))
	    [(0) (fukugen (- len1 1) len2      (cons (my-string-ref str1 len1) acc1) (cons #\- acc2))]
	    [(1) (fukugen len1 (- len2 1)     (cons #\- acc1) (cons (my-string-ref str2 len2)acc2))]
	    [(2) (fukugen (- len1 1)(- len2 1)(cons (my-string-ref str1 len1) acc1)(cons (my-string-ref str2 len2)acc2))]
	    [(3) (list acc1 acc2)]
	    )
	  )
      )
    (define (fukugen* len1 len2)
      (fukugen len1 len2 '() '()))

    ;(displayln (format "~a,~a:~a" *max-x* *max-y* *max*))
    
    (let* (
	   (res2 (fukugen* *max-x* *max-y*))  
	  )
      (set! *res* res2)
      (cons *max*
	    (map (lambda(x)(apply string x)) res2))
    )
    ;(dump-dp len1 len2)
))

(define (find-max-pos)
  (max-item (hash-map *dp* list) cadr))


(define (dump-hash)
  (sort
   (hash-map *dp* list)
   (lambda (x y) (if (= (caar x)(caar y))
		     (< (cadar x)(cadar y))
		     (< (caar x)(caar y)))))
)

;--*-- test program --*--
;(local-align "ACDEF" "GCHDER")
;(local-align "MEANLYPRTEINSTRING" "PLEASANTLYEINSTEIN")

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
