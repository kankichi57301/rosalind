;;
;;multiple-align 3 strings
;; 2021/02/18 start
;;
(require srfi/1)
(define *dp* '())
(define *tb* '())
(include "roslib.ss")

(define (init-dp-3d len1 len2 len3)
  (set! *dp* (make-hash))
  (set! *tb* (make-hash)) ;; traceback hash
  (hash-set! *dp*  '(0 0 0) 0)
  (for-each (lambda(x)(hash-set! *dp*  `(,x 0 0) x))(iota (+ 1 len1)))
  (for-each (lambda(y)(hash-set! *dp*  `(0 ,y 0) y))(iota (+ 1 len2)))
  (for-each (lambda(z)(hash-set! *dp*  `(0 0 ,z) z))(iota (+ 1 len3)))
  (for-each (lambda(x)(hash-set! *tb* `(,x 0 0) 4))(iota (+ 1 len1)))
  (for-each (lambda(y)(hash-set! *tb* `(0 ,y 0) 5))(iota (+ 1 len2)))
  (for-each (lambda(z)(hash-set! *tb* `(0 0 ,z) 6))(iota (+ 1 len3)))
  )

(define (score3 c1 c2 c3)
  (if (and (equal? c1 c2)(equal? c2 c3))
      0
      (if (or  (equal? c1 c2)(equal? c2 c3)(equal? c3 c1))
	  2
	  3)))
(define (score2 c1 c2)
  (if (equal? c1 c2)
      2
      3
      ))



(define *INF* 999)

;;--*--
(define (fill-dp-2d-xy str1 str2 str3 len1 len2 len3)
  ;(displayln "XY!")
  (for-each
   (lambda(x)
     (for-each
      (lambda(y)
	(let* ((z 0)
	       (c1 (my-string-ref str1 x))  ;;string-ref x-1
	       (c2 (my-string-ref str2 y))
	       (c3 #\-)
	       
	       (c12  (score2 c1 c2))
	       ;(c23  (score2 c2 c3))
	       ;(c31  (score2 c3 c1))
	       ;(c123 (score3 c1 c2 c3))
				  
	       (v1   (+ 3    (hash-ref *dp* `(,(- x 1) ,y ,z))))
	       (v2   (+ 3    (hash-ref *dp* `(,x ,(- y 1) ,z))))
	       (v3   (+ 3    *INF*))
	       (v12  (+ c12  (hash-ref *dp* `(,(- x 1) ,(- y 1) ,z))))
	       (v23  *INF*)
	       (v31  *INF*)
	       (v123 *INF*)		
	       
	       (val (min v123 v12 v23 v31 v1 v2 v3))
	       (dir (min-index-of (list v123 v12 v23 v31 v1 v2 v3) identity))
	       )
			    
	  (hash-set! *dp*  (list x y z) val)
	  (hash-set! *tb*  (list x y z) dir)
	  
	  ))
      (iota len2 1)))
   (iota len1 1)))
;;--*-- 
(define (fill-dp-2d-yz str1 str2 str3 len1 len2 len3)
  ;(displayln "YZ!")
  (for-each
   (lambda(y)
     (for-each
      (lambda(z)
	(let* ((x 0)
	       (c1 #\-)
	       (c2 (my-string-ref str2 y))
	       (c3 (my-string-ref str3 z))  ;;string-ref z-1
	       
	       ;(c12  (score2 c1 c2))
	       (c23  (score2 c2 c3))
	       ;(c31  (score2 c3 c1))
	       ;(c123 (score3 c1 c2 c3))
				  
	       (v1   (+ 3    *INF*))
	       (v2   (+ 3    (hash-ref *dp* `(,x ,(- y 1) ,z))))
	       (v3   (+ 3    (hash-ref *dp* `(,x ,y ,(- z 1)))))
	       (v12  *INF*)
	       (v23  (+ c23  (hash-ref *dp* `(,x ,(- y 1) ,(- z 1)))))
	       (v31  *INF*)
	       (v123 *INF*)		
	       
	       (val (min v123 v12 v23 v31 v1 v2 v3))
	       (dir (min-index-of (list v123 v12 v23 v31 v1 v2 v3) identity))
	       )
			    
	  (hash-set! *dp*  (list x y z) val)
	  (hash-set! *tb*  (list x y z) dir)
	  
	  ))
      (iota len3 1)))
   (iota len2 1)))
;; --*--
(define (fill-dp-2d-zx str1 str2 str3 len1 len2 len3)
  ;(displayln "ZX!")
  (for-each
   (lambda(z)
     (for-each
      (lambda(x)
	(let* ((y 0)
	       (c1 (my-string-ref str1 x))  ;;string-ref x-1
	       (c2 #\-)
	       (c3 (my-string-ref str3 z))
	       
	       ;(c12  (score2 c1 c2))
	       ;(c23  (score2 c2 c3))
	       (c31  (score2 c3 c1))
	       ;(c123 (score3 c1 c2 c3))
				  
	       (v1   (+ 3    (hash-ref *dp* `(,(- x 1) ,y ,z))))
	       (v2   (+ 3    *INF*))
	       (v3   (+ 3    (hash-ref *dp* `(,x ,y ,(- z 1)))))
	       (v12  *INF*)
	       (v23  *INF*)
	       (v31  (+ c31  (hash-ref *dp* `(,(- x 1) ,y ,(- z 1)))))
	       (v123 *INF*)		
	       
	       (val (min v123 v12 v23 v31 v1 v2 v3))
	       (dir (min-index-of (list v123 v12 v23 v31 v1 v2 v3) identity))
	       )
			    
	  (hash-set! *dp*  (list x y z) val)
	  (hash-set! *tb*  (list x y z) dir)
	  
	  ))
      (iota len1 1)))
   (iota len3 1)))


(define (fill-dp-table-3d str1 str2 str3 len1 len2 len3)
  ;(displayln "3D!")
  (for-each
   (lambda(y)
     (for-each
      (lambda(x)
	(for-each
	 (lambda(z)
	   (let* ((c1 (my-string-ref str1 x))  ;;string-ref x-1
		  (c2 (my-string-ref str2 y))
		  (c3 (my-string-ref str3 z))

		  (c12  (score2 c1 c2))
		  (c23  (score2 c2 c3))
		  (c31  (score2 c3 c1))
		  (c123 (score3 c1 c2 c3))
				  
		  (v1   (+ 3    (hash-ref *dp* `(,(- x 1) ,y ,z))))
		  (v2   (+ 3    (hash-ref *dp* `(,x ,(- y 1) ,z))))
		  (v3   (+ 3    (hash-ref *dp* `(,x ,y ,(- z 1)))))
		  (v12  (+ c12  (hash-ref *dp* `(,(- x 1) ,(- y 1) ,z))))
		  (v23  *INF*)
		  (v31  *INF*)
		  (v123 (+ c123 (hash-ref *dp* `(,(- x 1) ,(- y 1) ,(- z 1)))))
					 
		  (val (min v123 v12 v23 v31 v1 v2 v3))
		  (dir (min-index-of (list v123 v12 v23 v31 v1 v2 v3) identity))
		  )
			    
			    (hash-set! *dp*  (list x y z) val)
			    (hash-set! *tb*  (list x y z) dir)
			    
			  ))
	 (iota len3 1)))
      (iota len2 1)))
   (iota len1 1)))



(define (dump-hash )
  (sort
   (hash-map *dp* list)
  (lambda(x y)(triple-lessp (car x)(car y))))
  )
(define (dump-hash2 len1 len2 len3)
  (for-each
   (lambda(x)
     (for-each
      (lambda(y)
	(for-each
	 (lambda(z)
	   (display (format "~a " (hash-ref *dp* `(,x ,y ,z) -1))))
	 (iota (+ 1 len1) 0))
	 (display "\n"))
      (iota (+ 1 len2) 0))
     (display "\n"))
   (iota (+ 1 len3) 0))
)

 
(define (my-string-ref str n)
  (string-ref str (- n 1)))



(define (multi-align-3d str1 str2 str3)
  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	(len3 (string-length str3))
	)
    
    (init-dp-3d len1 len2 len3)
    (fill-dp-2d-xy str1 str2 str3 len1 len2 len3)
    (fill-dp-2d-yz str1 str2 str3 len1 len2 len3)
    (fill-dp-2d-zx str1 str2 str3 len1 len2 len3)
    (fill-dp-table-3d str1 str2 str3 len1 len2 len3)
    (dump-hash2 len1 len2 len3)
     
    (define (fukugen3 len1 len2 len3 acc1 acc2 acc3)
      ;(display   (format ":~a" (apply string acc1)))
      ;(displayln (format "|~a" (apply string acc2)))
      ;(displayln (format "|~a" (apply string acc3)))
      (if (and (= len1 0)(= len2 0))
	  (list acc1 acc2 acc3)
	  (case (hash-ref *tb* (list len1 len2 len3))
	    [(4) (fukugen3 (- len1 1) len2 len3  (cons (my-string-ref str1 len1) acc1)(cons #\- acc2)) (cons #\- acc3)]
	    [(5) (fukugen3 len1 (- len2 1) len3  (cons #\- acc1) (cons (my-string-ref str2 len2) acc2) (cons #\- acc3))]
	    [(6) (fukugen3 len1 len2  (- len3 1) (cons #\- acc1) (cons #\- acc2) (cons (my-string-ref str2 len2) acc3))]
	    [(1) (fukugen3 (- len1 1)(- len2 1) len3
			   (cons (my-string-ref str1 len1) acc1)
			   (cons (my-string-ref str2 len2) acc2)
			   acc3 )]
	    [(2) (fukugen3 len1 (- len2 1) (- len3 1)
			   acc1
			   (cons (my-string-ref str2 len2) acc2)
			   (cons (my-string-ref str3 len3) acc3) )]
	    [(3) (fukugen3 (- len1 1)  len2  (- len3 1)
			   (cons (my-string-ref str1 len1) acc1)
			   acc2
			   (cons (my-string-ref str2 len2) acc3)	)]
	    [(0) (fukugen3 (- len1 1)(- len2 1)(- len3 1)
			   (cons (my-string-ref str1 len1) acc1)
			   (cons (my-string-ref str2 len2) acc2)
			   (cons (my-string-ref str2 len3) acc3))]
	    )
	  )
      )
    (cons
     (hash-ref *dp* (list len1 len2 len3))
     (map (lambda(x)(apply string x))(fukugen3 len1 len2 len3 '() '() '())))

))

(define (loadme)
  (load "align5.ss"))


;--*-- test program --*--
#|
(define aa (multi-align-3d "AT" "AT" "AT"))
(filter (lambda(x)(= (caar x) 0)) aa)
(filter (lambda(x)(= (cadar x) 0)) aa)
(filter (lambda(x)(= (caddar x) 0)) aa)
(mult-dna-score '("AT" "AT" "AT"))
|#
