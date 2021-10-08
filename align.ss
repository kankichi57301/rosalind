;;
;; align.ss
;; 2021/01/09 copied from editdist4.ss
;; constant gap penelty
;;
(require srfi/1)
(define *my-hash* '())
(define *my-hash2* '())
(include "roslib.ss")
(include "blosum62.ss")
(define *gap-pen* -5)

(define (init-edit-distance len1 len2)
  (set! *my-hash* (make-hash))
  (set! *my-hash2* (make-hash)) ;; dir hash
  (hash-set! *my-hash* `(1 0)  *gap-pen* )
  (hash-set! *my-hash* `(0 1)  *gap-pen* )
  (for-each (lambda(x)(hash-set! *my-hash* `(,x 0)   *gap-pen*))(iota len1 1))
  (for-each (lambda(y)(hash-set! *my-hash* `(0 ,y)   *gap-pen*))(iota len2 1))
  (hash-set! *my-hash* '(0 0) 0)
  (for-each (lambda(x)(hash-set! *my-hash2* `(,x 0)  1 ))(iota (+ 1 len1)))
  (for-each (lambda(y)(hash-set! *my-hash2* `(0 ,y)  2 ))(iota (+ 1 len2)))
 
  )

(define (fill-edit-distance-table str1 str2 len1 len2)
  (for-each (lambda(y)
	      (for-each (lambda(x)
			  (let* ((c   (b62score
					 (string-ref str1 (- x 1))
					 (string-ref str2 (- y 1)))
				  )
				       
				 (gap-penx (if (= 0 (hash-ref *my-hash2* (list (- x 1) y) -1))
					       0 *gap-pen*))
				 (gap-peny (if (= 1 (hash-ref *my-hash2* (list x (- y 1)) -1))
					       0 *gap-pen*))
				 
				 (v1 (+ gap-penx (hash-ref *my-hash* (list (- x 1) y))))
				 (v2 (+ gap-peny (hash-ref *my-hash* (list x  (- y 1)))))
				 (v3 (+ c 
					(hash-ref *my-hash* (list (- x 1) (- y 1)))))
				 (val (max v1 v2 v3))
				 (dir (max-index-of (list v1 v2 v3) identity))
				 )

			    
			    (hash-set! *my-hash*  (list x y) val)
			    (hash-set! *my-hash2* (list x y) dir)
			  ;;(displayln (list x y val))
			  ))

			(iota len1 1)))
  (iota len2 1)))
			  
(define (disp-matrix mat)
  (for-each (lambda(line)(displayln (format "~a" line)))
	    mat))
(define *DUMP* #t)
(define (align-gcon str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2)))
    (init-edit-distance len1 len2)
    (fill-edit-distance-table str1 str2 len1 len2)

    (when *DUMP* 
	  (disp-matrix
	   (group-per
	    (map cadr (dump-hash))
	    (+ 1 (string-length str2))))
    )
    (hash-ref *my-hash* (list len1 len2))
  )
)

(define (dump-hash )
  (sort
   (hash-map *my-hash* list)
   (lambda (x y) (if (= (caar x)(caar y))
		     (< (cadar x)(cadar y))
		     (< (caar x)(caar y)))))
	       
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
      (if (and (= len1 0)(= len2 0))
	  (list acc1 acc2)
	  (case (hash-ref *my-hash2* (list len1 len2))
	    [(0) (fukugen (- len1 1) len2      (cons (my-string-ref str1 len1) acc1)(cons #\- acc2))]
	    [(1) (fukugen len1 (- len2 1)     (cons #\- acc1) (cons (my-string-ref str2 len2)acc2))]
	    [(2) (fukugen (- len1 1)(- len2 1)(cons (my-string-ref str1 len1) acc1)(cons (my-string-ref str2 len2)acc2))]
	    )
	  )
      )
    (cons
     (hash-ref *my-hash* (list len1 len2))
     (map (lambda(x)(apply string x))(fukugen len1 len2 '() '())))
))




