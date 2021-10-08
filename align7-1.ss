;;
;;multiple-align 4 strings
;; 2021/02/18 start
;;
(require srfi/1)
(define *dp* '())
(define *tb* '())
(include "roslib.ss")
(displayln "align7.ss loaded.")

(define (init-dp-4d len1 len2 len3 len4)
  (set! *dp* (make-hash))
  (set! *tb* (make-hash)) ;; traceback hash
  (hash-set! *dp*  '(0 0 0 0) 0)
  )

(define (scoren . clist)
  (apply + (map (lambda(x)(if (apply equal? x) 0 1))
		(combinations clist 2))))

(define *INF* 999)

(define (my-string-ref2 str n)
  (if (<= n 0)
      #\-
      (string-ref str (- n 1))))

(define (fill-dp-table-4d str1 str2 str3 str4 len1 len2 len3 len4)
  ;(displayln "3D!")
  (for-each
   (lambda(x)
     (for-each
      (lambda(y)
	(for-each
	 (lambda(z)
	   (for-each
	    (lambda(w)
	      (when (or (> x 0)(> y 0)(> z 0)(> w 0))
		    (let* ((c1 (my-string-ref2 str1 x))  ;;string-ref x-1
			   (c2 (my-string-ref2 str2 y))
			   (c3 (my-string-ref2 str3 z))
			   (c4 (my-string-ref2 str4 w))
			   
			   (c12   (scoren c1 c2 #\- #\-))
			   (c13   (scoren c1 #\- c3 #\-))
			   (c14   (scoren c1 #\- #\- c4))
			   (c23   (scoren #\- c2 c3 #\-))
			   (c24   (scoren #\- c2 #\- c4))
			   (c34   (scoren #\- #\- c3 c4))
			   
			   (c123  (scoren c1 c2  c3 #\-))
			   (c124  (scoren c1 c2  #\- c4))
			   (c134  (scoren c1 #\- c3  c4))
			   (c234  (scoren #\- c2 c3  c4))
			   (c1234 (scoren c1 c2  c3  c4))
			   
			   ; ANY GAP GAP GAP => always 3 
			   (v1    (+ 3    (hash-ref *dp* `(,(- x 1) ,y ,z ,w) *INF*)))
			   (v2    (+ 3    (hash-ref *dp* `(,x ,(- y 1) ,z ,w) *INF*)))
			   (v3    (+ 3    (hash-ref *dp* `(,x ,y ,(- z 1) ,w) *INF*)))
			   (v4    (+ 3    (hash-ref *dp* `(,x ,y ,z ,(- w 1)) *INF*)))

			   (v12   (+ c12  (hash-ref *dp* `(,(- x 1) ,(- y 1)  ,z ,w) *INF*)))
			   (v13   (+ c13  (hash-ref *dp* `(,(- x 1) ,y  ,(- z 1) ,w) *INF*)))
			   (v14   (+ c14  (hash-ref *dp* `(,(- x 1) ,y  ,z ,(- w 1)) *INF*)))
			   (v23   (+ c23  (hash-ref *dp* `(,x ,(- y 1)  ,(- z 1) ,w) *INF*)))
			   (v24   (+ c24  (hash-ref *dp* `(,x ,(- y 1)  ,z ,(- w 1)) *INF*)))
			   (v34   (+ c34  (hash-ref *dp* `(,x ,y  ,(- z 1) ,(- w 1)) *INF*)))

			   (v123  (+ c123 (hash-ref  *dp* `(,(- x 1) ,(- y 1) ,(- z 1) ,w) *INF*)))
			   (v124  (+ c124 (hash-ref  *dp* `(,(- x 1) ,(- y 1) ,z ,(- w 1)) *INF*)))
			   (v134  (+ c134 (hash-ref  *dp* `(,(- x 1) ,y ,(- z 1) ,(- w 1)) *INF*)))
			   (v234  (+ c234 (hash-ref  *dp* `(,x ,(- y 1) ,(- z 1) ,(- w 1)) *INF*)))
			   (v1234 (+ c1234 (hash-ref *dp* `(,(- x 1) ,(- y 1) ,(- z 1) ,(- w 1)) *INF*)))
			   
			   (mylist (list v1234 v123 v124 v134 v234 v12 v13 v14 v23 v24 v34 v1 v2 v3 v4))
			   (val (apply min mylist))
			   (dir (min-index-of mylist identity))
			   
			  )
		(hash-set! *dp*  (list x y z w) val)
		(hash-set! *tb*  (list x y z w) dir)
		      #t
		      )))
	     
	     (iota (+ len4 1))))
	 (iota (+ len3 1))))
      (iota (+ len2 1))))
   (iota (+ len1 1))))



(define (dump-hash )
  (sort
   (hash-map *dp* list)
  (lambda(x y)(quad-lessp (car x)(car y))))
  )

 
(define (my-string-ref str n)
  (string-ref str (- n 1)))

(define *table4* '((1 1 1 1)
		   (1 1 1 0)
		   (1 1 0 1)
		   (1 0 1 1)
		   (0 1 1 1)
		   (1 1 0 0)
		   (1 0 1 0)
		   (1 0 0 1)
		   (0 1 1 0)
		   (0 1 0 1)
		   (0 0 1 1)
		   (1 0 0 0)
		   (0 1 0 0)
		   (0 0 1 0)
		   (0 0 0 1)))

(define (multi-align-4d str1 str2 str3 str4)
  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	(len3 (string-length str3))
	(len4 (string-length str4))
	)
    
    (init-dp-4d len1 len2 len3 len4)
    (fill-dp-table-4d str1 str2 str3 str4 len1 len2 len3 len4)
     
    (define (fukugen4 len1 len2 len3 len4 acc1 acc2 acc3 acc4)
      ;(display   (format ":~a" (apply string acc1)))
      ;(displayln (format "|~a" (apply string acc2)))
      ;(displayln (format "|~a" (apply string acc3)))
      ;(displayln (format "|~a" (apply string acc4)))
      (if (and (= len1 0)(= len2 0)(= len3 0)(= len4 0))
	  (list acc1 acc2 acc3 acc4)
	  (let* ((dir (hash-ref *tb* (list len1 len2 len3 len4)))
		 (v   (list-ref *table4* dir))
		 (d1  (list-ref v 0))
		 (d2  (list-ref v 1))
		 (d3  (list-ref v 2))
		 (d4  (list-ref v 3)))
		      
	    
	    (fukugen4 (- len1 d1)
		      (- len2 d2)
		      (- len3 d3)
		      (- len4 d4)
		      (cons (if (= 1 d1)
				(my-string-ref str1 len1)
				#\- )
			    acc1)
		      (cons (if (= 1 d2)
				(my-string-ref str2 len2)
				#\- )
			    acc2)
		      (cons (if (= 1 d3)
				(my-string-ref str3 len3)
				#\- )
			    acc3)
		      (cons (if (= 1 d4)
				(my-string-ref str4 len4)
				#\- )
			    acc4)
	     )
	  )
      ))
    
    ;(cons
    ; (hash-ref *dp* (list len1 len2 len3 len4))
    ; (map (lambda(x)(apply string x))(fukugen4 len1 len2 len3 len4 '() '() '() '())))
    
    #t
  )
)

(define (m-a-4d str1 str2 str3 str4)
  (multi-align-4d str1 str2 str3 str4))
(define (loadme)
  (load "align7.ss"))

(define m-test2 '("GGCCCAGG" "AACATAACAC" "TCCTAGAGA" "AAACAAAACA"))
;--*-- test program --*--
(define (test001)
  (m-a-4d "AT" "AT" "AT" "AT"))
(define (test002)
  (m-a-4d "AT" "AT" "AT" "AG"))
;(apply multi-align-4d m-test2)
#|
(define aa (multi-align-4d "AT" "AT" "AT" "AT"))
(filter (lambda(x)(= (caar x) 0)) aa)
(filter (lambda(x)(= (cadar x) 0)) aa)
(filter (lambda(x)(= (caddar x) 0)) aa)
(mult-dna-score '("AT" "AT" "AT" "AT"))
|#
