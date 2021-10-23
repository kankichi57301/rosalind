(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/function identity))
;;
;;edit-distance #3
;; 2021/01/09 copied from editdist2.ss
;;
(require srfi/1)
(require "roslibA.ss")

(define *dp* '())
(define *tb* '())
(define *way* 0)
(define *gap-pen* 1)

(define (init-edit-distance len1 len2)
  (set! *dp* (make-hash))
  (set! *tb* (make-hash)) ;; dir hash
  (set! *way* (make-hash)) ;;way of path
  (for-each (lambda(x)(hash-set! *dp* `(,x 0) (* *gap-pen* x)))(iota (+ 1 len1)))
  (for-each (lambda(y)(hash-set! *dp* `(0 ,y) (* *gap-pen* y)))(iota (+ 1 len2)))
  (for-each (lambda(x)(hash-set! *tb* `(,x 0) 1))(iota (+ 1 len1)))
  (for-each (lambda(y)(hash-set! *tb* `(0 ,y) 2))(iota (+ 1 len2)))
  (for-each (lambda(x)(hash-set! *way* `(,x 0) 1))(iota (+ 1 len1)))
  (for-each (lambda(y)(hash-set! *way* `(0 ,y) 1))(iota (+ 1 len2)))
  )

(define (fill-edit-distance-table str1 str2 len1 len2)
  (for-each (lambda(y)
	      (for-each (lambda(x)
			  (let* ((c (if (equal? (string-ref str1 (- x 1))
						(string-ref str2 (- y 1)))
				       0 1))  
				 (v1 (+ *gap-pen* (hash-ref *dp* `(,(- x 1) ,y))))
				 (v2 (+ *gap-pen* (hash-ref *dp* `(,x  ,(- y 1)))))
				 (v3 (+ c 
					(hash-ref *dp* `(,(- x 1) ,(- y 1)))))
				 (val (min v1 v2 v3))
				 (dir (min-index-of (list v1 v2 v3) identity))
				 )
			    (hash-set! *dp*  (list x y) val)
			    (hash-set! *tb*  (list x y) dir)
			    (hash-set! *way* (list x y)
				             (+ (if (= v1 val)
						  (hash-ref *way* `(,(- x 1) ,y))
						  0)
						(if (= v2 val)
						  (hash-ref *way* `(,x ,(- y 1)))
						  0)
						(if (= v3 val)
						  (hash-ref *way* `(,(- x 1) ,(- y 1)))
						  0)
                                             ))

			  ;;(displayln (list x y val))
			  ))

			(iota len1 1)))
  (iota len2 1)))
			  

(define (edit-distance str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2)))
    (init-edit-distance len1 len2)
    (fill-edit-distance-table str1 str2 len1 len2)
    (hash-ref *dp* (list len1 len2))
  )
)



(define (edit-align str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	)
    (init-edit-distance len1 len2)
    (fill-edit-distance-table str1 str2 len1 len2)
    

    (define (my-string-ref str n)
      (string-ref str (- n 1)))
    
    (define (fukugen len1 len2 acc1 acc2)
      (if (or (= len1 0)(= len2 0))
	  (list acc1 acc2)
	  (case (hash-ref *tb* (list len1 len2))
	    [(0) (fukugen (- len1 1) len2      (cons (my-string-ref str1 len1) acc1)(cons #\- acc2))]
	    [(1) (fukugen len1 (- len2 1)     (cons #\- acc1) (cons (my-string-ref str2 len2)acc2))]
	    [(2) (fukugen (- len1 1)(- len2 1)(cons (my-string-ref str1 len1) acc1)(cons (my-string-ref str2 len2)acc2))]
	    )
	  )
      )
    
    (cons
     (hash-ref *dp* (list len1 len2))
     (cons
     (modulo (hash-ref *way* (list len1 len2)) 134217727)
     (map (lambda(x)(apply string x))(fukugen len1 len2 '() '()))))
))

(define (dump-hash-way)
  (sort
   (hash-map *way* list)
   (lambda (x y) (if (= (caar x)(caar y))
		     (< (cadar x)(cadar y))
		     (< (caar x)(caar y)))))
)
)


