;;
;; align-gcon.ss
;; 2021/02/25 copied from align2.ss
;; constant gap penelty
;;
(require srfi/1)
(require srfi/13)
(define *upper* '())
(define *middle* '())
(define *lower* '())
(define *tb* '()) ;; trace back
(include "roslib.ss")
(include "blosum62.ss")
(define *gap-pen* -5)

(define (init-gcon len1 len2)
  (set! *lower* (make-hash))
  (set! *middle* (make-hash))
  (set! *upper* (make-hash))
  (set! *tb* (make-hash))

  (for-each
   (lambda(i)(hash-set! *lower* `(,i 0)   *gap-pen*))
   (iota len1 1))
  (for-each
   (lambda(i)(hash-set! *middle* `(,i 0)   *gap-pen*))
   (iota len1 1))
  (for-each
   (lambda(i)(hash-set! *upper* `(,i 0)   (* 10 *gap-pen*)))
   (iota len1 1))

  (for-each
   (lambda(j)(hash-set! *lower* `(0 ,j)   (* 10 *gap-pen*)))
   (iota len2 1))
  (for-each
   (lambda(j)(hash-set! *middle* `(0 ,j)   *gap-pen*))
   (iota len2 1))
  (for-each
   (lambda(j)(hash-set! *upper* `(0 ,j)    *gap-pen*))
   (iota len2 1))

  (for-each
   (lambda(i)(hash-set! *tb* `(,i 0)   1))
   (iota len1 1))
  (for-each
   (lambda(j)(hash-set! *tb* `(0 ,j)   2))
   (iota len1 1))
)

(define (fill-gcon-table str1 str2 len1 len2)
  (for-each (lambda(i)
	      (for-each (lambda(j)
			  (hash-set! *lower* `(,i ,j)
				     (max (hash-ref *lower* `(,(- i 1) ,j) 0)
					  (+ (hash-ref *middle* `(,(- i 1) ,j) 0) *gap-pen*)))
			  (hash-set! *upper* `(,i ,j)
				     (max (hash-ref *upper* `(,i ,(- j 1)) 0)
					  (+ (hash-ref *middle* `(,i ,(- j 1)) 0) *gap-pen*)))
			  (let* ((c   (b62score
					 (string-ref str1 (- i 1))
					 (string-ref str2 (- j 1)))
				  ))
			    (hash-set! *middle* `(,i ,j)
				       (max (hash-ref *lower* `(,i ,j) 0)
					    (+ (hash-ref *middle* `(,(- i 1) ,(- j 1)) 0) c)
					    (hash-ref *upper* `(,i ,j) 0)))
			    
			    (hash-set! *tb* `(,i ,j)
				       (max-index-of
					(list
					 (+ (hash-ref *middle* `(,(- i 1) ,(- j 1)) 0) c)
					 (hash-ref *lower* `(,i ,j) 0)
					 (hash-ref *upper* `(,i ,j) 0))
					identity)
			  )))
			(iota len2 1)))
	    (iota len1 1)))
			  

(define (pair-align-gcon str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	)
    (init-gcon len1 len2)
    (fill-gcon-table str1 str2 len1 len2)

    (define (my-string-ref str n)
      (string-ref str (- n 1)))
    
    (define (fukugen len1 len2 acc1 acc2)
      ;(display   (format ":~a" (apply string acc1)))
      ;(displayln (format "|~a" (apply string acc2)))
      (if (and (= len1 0)(= len2 0))
	  (list acc1 acc2)
	  (case (hash-ref *tb* (list len1 len2))
	    [(1) (fukugen (- len1 1) len2      (cons (my-string-ref str1 len1) acc1)(cons #\- acc2))]
	    [(2) (fukugen len1 (- len2 1)     (cons #\- acc1) (cons (my-string-ref str2 len2)acc2))]
	    [(0) (fukugen (- len1 1)(- len2 1)(cons (my-string-ref str1 len1) acc1)(cons (my-string-ref str2 len2)acc2))]
	    )
	  )
      )
    
    
    (cons
     (hash-ref *middle* (list len1 len2))
     (map (lambda(x)(apply string x))(fukugen  len1  len2 '() '())))
    
))

;(pair-align-gcon "PLEASANTLY" "MEANLY")
