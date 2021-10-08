;;
;;edit-distance #2
;; 2020/12/30 copied from editdist.ss
;;
(require srfi/1)
(define *my-hash* '())
(define *my-hash2* '())
(include "roslib.ss")
(define *m* 1)
(define *d* -2)
(define *g* -1)
(define (init-edit-distance len1 len2)
  (set! *my-hash* (make-hash))
  (set! *my-hash2* (make-hash)) ;; dir hash
  (for-each (lambda(x)(hash-set! *my-hash* `(,x 0) (* x *g*)))(iota (+ 1 len1)))
  (for-each (lambda(y)(hash-set! *my-hash* `(0 ,y) (* y *g*)))(iota (+ 1 len2)))
  (for-each (lambda(x)(hash-set! *my-hash2* `(,x 0) 0))(iota (+ 1 len1)))
  (for-each (lambda(y)(hash-set! *my-hash2* `(0 ,y) 1))(iota (+ 1 len2)))
  )

(define (fill-edit-distance-table str1 str2 len1 len2)

  (for-each (lambda(y)
	      (when (= 0 (modulo y 100))
		    (displayln y))
	      (for-each (lambda(x)
			  (let* ((c (if (equal? (string-ref str1 (- x 1))
						(string-ref str2 (- y 1)))
				       *m* *d*))

				 (v1 (+ *g* (hash-ref *my-hash* (list (- x 1) y))))
				 (v2 (+ *g* (hash-ref *my-hash* (list x  (- y 1)))))
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
			  


(define (max-align-gap str1 str2)
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
     (apply + (map count-gap(fukugen len1 len2 '() '())))
))

(define (count-gap dnalist)
  (apply + (map (lambda(x)(if (equal? #\- x) 1 0)) dnalist)))

#|
(edit-align "AACGTA" "ACACCTA")

|#

  

