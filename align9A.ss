(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/list combinations))
	(require (only-in racket/function identity))
;;
;;multiple-align 3 strings
;; 2021/11/06
;; invoked from rs_ba5m
(require srfi/1)
(require "roslibA.ss")

(define *dp* '())
(define *tb* '())

(define (init-dp-3d len1 len2 len3)
  (set! *dp* (make-hash))
  (set! *tb* (make-hash)) ;; traceback hash
  (hash-set! *dp*  '(0 0 0) 0)
  )
;;
;; which the score of an alignment column is 1 if all three symbols are identical and 0 otherwise.
;;
(define (score3 x y z)
  ;(display   (format "~a:~a:~a|" x y z))
  (if (and (equal? x y)(equal? y z)) -1 0))

(define *INF* 999)

(define (my-string-ref2 str n)
  (if (<= n 0)
      #\-
      (string-ref str (- n 1))))


(define (fill-dp-table-3d str1 str2 str3 len1 len2 len3)

  (for-each
   (lambda(x)
     (for-each
      (lambda(y)
	(for-each
	 (lambda(z)
	   (when (or (> x 0)(> y 0)(> z 0))
		 (let  ((mylist
			 (map (lambda(blist)
				(let* ((b1 (list-ref blist 0))
				       (b2 (list-ref blist 1))
				       (b3 (list-ref blist 2))
				       
				       (sc (score3 (if (= b1 1)
						       (my-string-ref2 str1 x)
						       #\-)
						   (if (= b2 1)
						       (my-string-ref2 str2 y)
						       #\-)
						   (if (= b3 1)
						       (my-string-ref2 str3 z)
						       #\-)
						   )))
				  (+ sc (hash-ref *dp* `(,(- x b1) ,(- y b2) ,(- z b3)) *INF*))))
			      *table3*)))
		   (hash-set! *dp*  (list x y z) (apply min mylist))
		   (hash-set! *tb*  (list x y z) (min-index-of mylist identity))
		   )))
	 (iota (+ len3 1))))
      (iota (+ len2 1))))
   (iota (+ len1 1))))


(define (dump-hash )
  (sort
   (hash-map *dp* list)
  (lambda(x y)(triple-lessp (car x)(car y))))
  )

 
(define (my-string-ref str n)
  (string-ref str (- n 1)))

(define *table3* '((1 1 1)
		   (1 1 0)
		   (1 0 1)
		   (0 1 1)
		   (1 0 0)
		   (0 1 0)
		   (0 0 1)
		   (0 0 0)))

(define (multi-align-3d str1 str2 str3)
  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	(len3 (string-length str3))
	)
    
    (init-dp-3d len1 len2 len3)
    (fill-dp-table-3d str1 str2 str3 len1 len2 len3)
     
    (define (fukugen3 len1 len2 len3 acc1 acc2 acc3)
      ;(display   (format ":~a" (apply string acc1)))
      ;(displayln (format "|~a" (apply string acc2)))
      ;(displayln (format "|~a" (apply string acc3)))

      (if (and (= len1 0)(= len2 0)(= len3 0))
	  (list acc1 acc2 acc3)
	  (let* ((dir (hash-ref *tb* (list len1 len2 len3)))
		 (v   (list-ref *table3* dir))
		 (d1  (list-ref v 0))
		 (d2  (list-ref v 1))
		 (d3  (list-ref v 2))
		 )
		      
	    
	    (fukugen3 (- len1 d1)
		      (- len2 d2)
		      (- len3 d3)

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
		      
	     )
	  )
      ))
    
    (cons
     (hash-ref *dp* (list len1 len2 len3))
     (map (lambda(x)(apply string x))(fukugen3 len1 len2 len3 '() '() '())))
    
    
  )
)

(define (m-a-3d str1 str2 str3)
  (multi-align-3d str1 str2 str3))



)
