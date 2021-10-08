;;
;; align.ss
;; 2021/02/26 copied from align.ss
;; affine gap penelty
;;
(require srfi/1)

(define *dp* '())
(define *tb* '())
(include "roslib.ss")
(include "blosum62.ss")
;; affine gap penalty
(define *a* -11)   ;; opening
(define *b* -1)    ;; extension

(define (init-affine len1 len2)
  (set! *dp* (make-hash))
  (set! *tb* (make-hash)) ;; dir hash
  (hash-set! *dp* `(0 0)  0   )
  ;(hash-set! *dp* `(1 0)  *a* )
  ;(hash-set! *dp* `(0 1)  *a* )
  (for-each (lambda(x)(hash-set! *dp* `(,x 0)(+ *a* (* (- x 1) *b*))))(iota len1 1))
  (for-each (lambda(y)(hash-set! *dp* `(0 ,y)(+ *a* (* (- y 1) *b*))))(iota len2 1))
  (hash-set! *dp* '(0 0) 0)
  (for-each (lambda(x)(hash-set! *tb* `(,x 0)  0 ))(iota (+ 1 len1)))
  (for-each (lambda(y)(hash-set! *tb* `(0 ,y)  1 ))(iota (+ 1 len2)))
)

(define (dp-ref x y)
  (hash-ref *dp* `(,x ,y) -1))

(define (fill-affine-table str1 str2 len1 len2)
  (for-each (lambda(y)
	      (for-each (lambda(x)
			  (let* ((c   (b62score
					 (string-ref str1 (- x 1))
					 (string-ref str2 (- y 1)))
				  )
				       
				 (gap-penx (if (= 0 (hash-ref *tb* (list (- x 1) y) -1))
					       *b* *a*))
				 (gap-peny (if (= 1 (hash-ref *tb* (list x (- y 1)) -1))
					       *b* *a*))
				 
				 (v1 (+ gap-penx (hash-ref *dp* (list (- x 1) y))))
				 (v2 (+ gap-peny (hash-ref *dp* (list x  (- y 1)))))
				 (v3 (+ c 
					(hash-ref *dp* (list (- x 1) (- y 1)))))
				 (val (max v1 v2 v3))
				 (dir (max-index-of (list v1 v2 v3) identity))
				 )

			    
			    (hash-set! *dp*  (list x y) val)
			    (hash-set! *tb*  (list x y) dir)
			  ;;(displayln (list x y val))
			  ))

			(iota len1 1)))
  (iota len2 1)))
			  
(define (disp-matrix mat)
  (for-each (lambda(line)(displayln (format "~a" line)))
	    mat))
(define *DUMP* #t)
(define (align-affine str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2)))
    (init-affine len1 len2)
    ;(fill-affine-table str1 str2 len1 len2)

    (when *DUMP* 
	  (disp-matrix
	   (group-per
	    (map cadr (dump-hash))
	    (+ 1 (string-length str2))))
    )
    ;(hash-ref *dp* (list len1 len2))
  )
)

(define (dump-dp len1 len2)
  (for-each (lambda(y)
	      (for-each (lambda(x)
			  (display (format "~a " (dp-ref x y))))
			(iota (+ len1 1)))
	      (display "\n"))
	    (iota (+ len2 1)))
)

(define (pair-align-affine str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	)
    (init-affine len1 len2)
    (fill-affine-table str1 str2 len1 len2)
    ;(dump-dp len1 len2)

    (define (my-string-ref str n)
      (string-ref str (- n 1)))
    
    (define (fukugen len1 len2 acc1 acc2)
      (if (and (= len1 0)(= len2 0))
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
     (map (lambda(x)(apply string x))(fukugen len1 len2 '() '())))

))

#|
(pair-align-affine "AEEC" "AEC")


|#


