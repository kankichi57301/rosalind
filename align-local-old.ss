;;
;; local alignment
;; 2021/03/01 copied from editdist2.ss
;;
(require srfi/1)
(include "PAM250.ss")
(define *dp* '())
(define *tb* '())

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
			    

			  ;;(displayln (list x y val))
			  ))

			(iota len1 1)))
  (iota len2 1)))
;;			  
;;--*-- tbc
;;
(define (local-align str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	)
    (init-local len1 len2)
    ;(fill-local-table str1 str2 len1 len2)
    

    (define (my-string-ref str n)
      (string-ref str (- n 1)))
    
    (define (fukugen len1 len2 acc1 acc2)
      (if (or (= len1 0)(= len2 0))
	  (list acc1 acc2)
	  (case (hash-ref *tb* (list len1 len2))
	    [(0) (fukugen (- len1 1) len2      (cons (my-string-ref str1 len1) acc1)(cons #\- acc2))]
	    [(1) (fukugen len1 (- len2 1)     (cons #\- acc1) (cons (my-string-ref str2 len2)acc2))]
	    [(2) (fukugen (- len1 1)(- len2 1)(cons (my-string-ref str1 len1) acc1)(cons (my-string-ref str2 len2)acc2))]
	    [(3) (list acc1 acc2)]
	    )
	  )
      )
    #|
    (cons
     (hash-ref *dp* (list len1 len2)) 
     (map (lambda(x)(apply string x))(fukugen len1 len2 '() '())))
    |#
    (dump-hash)
    ;(find-max-pos) ;; tbc
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

