;;
;;multiple-align 3 strings
;; 2021/02/18 start
;;
(require srfi/1)
(define *dp* '())
(define *tb* '())
(include "roslib.ss")
(displayln "align6.ss loaded.")

(define (init-dp-3d len1 len2 len3)
  (set! *dp* (make-hash))
  (set! *tb* (make-hash)) ;; traceback hash
  (hash-set! *dp*  '(0 0 0) 0)
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

(define (scoren . clist)
  (apply + (map (lambda(x)(if (apply equal? x) 0 1))
		(combinations clist 2))))

(define *INF* 999)

(define (my-string-ref2 str n)
  (if (<= n 0)
      #\-
      (string-ref str (- n 1))))

(define (fill-dp-table-3d str1 str2 str3 len1 len2 len3)
  ;(displayln "3D!")
  (for-each
   (lambda(x)
     (for-each
      (lambda(y)
	(for-each
	 (lambda(z)
	   (when (or (> x 0)(> y 0)(> z 0))
		 (let* ((c1 (my-string-ref2 str1 x))  ;;string-ref x-1
			(c2 (my-string-ref2 str2 y))
			(c3 (my-string-ref2 str3 z))
			
			(c12  (scoren c1 c2 #\-))
			(c23  (scoren #\- c2 c3))
			(c31  (scoren c3 #\- c1))
			(c123 (scoren c1 c2  c3))
				  
			(v1   (+ 2    (hash-ref *dp* `(,(- x 1) ,y ,z) *INF*)))
			(v2   (+ 2    (hash-ref *dp* `(,x ,(- y 1) ,z) *INF*)))
			(v3   (+ 2    (hash-ref *dp* `(,x ,y ,(- z 1)) *INF*)))
			(v12  (+ c12  (hash-ref *dp* `(,(- x 1) ,(- y 1)  ,z) *INF*)))
			(v23  (+ c23  (hash-ref *dp* `(,x ,(- y 1)  ,(- z 1)) *INF*)))
			(v31  (+ c31  (hash-ref *dp* `(,(- x 1) ,y  ,(- z 1)) *INF*)))
			(v123 (+ c123 (hash-ref *dp* `(,(- x 1) ,(- y 1) ,(- z 1)) *INF*)))
			
			(val (min v123 v12 v23 v31 v1 v2 v3))
			(dir (min-index-of (list v123 v12 v23 v31 v1 v2 v3) identity))
			)
		   (hash-set! *dp*  (list x y z) val)
		   (hash-set! *tb*  (list x y z) dir))))
	     
	     
	 (iota (+ len3 1))))
      (iota (+ len2 1))))
   (iota (+ len1 1))))



(define (dump-hash )
  (sort
   (hash-map *dp* list)
  (lambda(x y)(triple-lessp (car x)(car y))))
  )
(define (dump-dp len1 len2 len3)
  (for-each
   (lambda(x)
     (for-each
      (lambda(y)
	(for-each
	 (lambda(z)
	   (display (format "~a " (hash-ref *dp* `(,x ,y ,z) -1))))
	 (iota (+ 1 len3) 0))
	 (display "\n"))
      (iota (+ 1 len2) 0))
     (display "\n"))
   (iota (+ 1 len1) 0))
)

(define (dump-traceback len1 len2 len3)
  (for-each
   (lambda(x)
     (for-each
      (lambda(y)
	(for-each
	 (lambda(z)
	   (display (format "~a " (hash-ref *dp* `(,x ,y ,z) -1))))
	 (iota (+ 1 len3) 0))
	 (display "\n"))
      (iota (+ 1 len2) 0))
     (display "\n"))
   (iota (+ 1 len1) 0))
)
 
(define (my-string-ref str n)
  (string-ref str (- n 1)))



(define (multi-align-3d str1 str2 str3)
  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	(len3 (string-length str3))
	)
    
    (init-dp-3d len1 len2 len3)
    ;(fill-dp-2d-xy str1 str2 str3 len1 len2 len3)
    ;(fill-dp-2d-yz str1 str2 str3 len1 len2 len3)
    ;(fill-dp-2d-zx str1 str2 str3 len1 len2 len3)
    (fill-dp-table-3d str1 str2 str3 len1 len2 len3)
    ;(dump-dp len1 len2 len3)
    (dump-traceback len1 len2 len3)
     
    (define (fukugen3 len1 len2 len3 acc1 acc2 acc3)
      ;(display   (format ":~a" (apply string acc1)))
      ;(displayln (format "|~a" (apply string acc2)))
      ;(displayln (format "|~a" (apply string acc3)))
      (if (and (= len1 0)(= len2 0)(= len3 0))
	  (list acc1 acc2 acc3)
	  (case (hash-ref *tb* (list len1 len2 len3))
	    [(4) (fukugen3 (- len1 1) len2 len3  (cons (my-string-ref str1 len1) acc1)(cons #\- acc2) (cons #\- acc3))]
	    [(5) (fukugen3 len1 (- len2 1) len3  (cons #\- acc1) (cons (my-string-ref str2 len2) acc2) (cons #\- acc3))]
	    [(6) (fukugen3 len1 len2  (- len3 1) (cons #\- acc1) (cons #\- acc2) (cons (my-string-ref str2 len2) acc3))]
	    [(1) (fukugen3 (- len1 1)(- len2 1) len3
			   (cons (my-string-ref str1 len1) acc1)
			   (cons (my-string-ref str2 len2) acc2)
			   (cons #\- acc3 ))]
	    [(2) (fukugen3 len1 (- len2 1) (- len3 1)
			   (cons #\- acc1)
			   (cons (my-string-ref str2 len2) acc2)
			   (cons (my-string-ref str3 len3) acc3) )]
	    [(3) (fukugen3 (- len1 1)  len2  (- len3 1)
			   (cons (my-string-ref str1 len1) acc1)
			   (cons #\- acc2)
			   (cons (my-string-ref str2 len2) acc3)	)]
	    [(0) (fukugen3 (- len1 1)(- len2 1)(- len3 1)
			   (cons (my-string-ref str1 len1) acc1)
			   (cons (my-string-ref str2 len2) acc2)
			   (cons (my-string-ref str3 len3) acc3))]
	    )
	  )
      )
    
    (cons
     (hash-ref *dp* (list len1 len2 len3))
     (map (lambda(x)(apply string x))(fukugen3 len1 len2 len3 '() '() '())))
))

(define (m-a-3d str1 str2 str3)
  (multi-align-3d str1 str2 str3))
(define (loadme)
  (load "align6.ss"))

(define m-test1 '("GGCCCAGG" "AACATAACAC" "TCCTAGAGA"))
;--*-- test program --*--
; (apply multi-align-3d m-test1)
#|
(define aa (multi-align-3d "AT" "AT" "AT"))
(filter (lambda(x)(= (caar x) 0)) aa)
(filter (lambda(x)(= (cadar x) 0)) aa)
(filter (lambda(x)(= (caddar x) 0)) aa)
(mult-dna-score '("AT" "AT" "AT"))
|#
