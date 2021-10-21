(module rosalind racket/base
	(provide (all-defined-out))
;;
;; align2A.ss
;; 2021/01/09 copied from editdist4.ss
;; constant gap penelty
;;
(require srfi/1)
(require srfi/13)
(define *upper* '())
(define *middle* '())
(define *lower* '())
(require "roslibA.ss")
(require "blosum62A.ss")
(define *gap-pen* -5)

(define (init-edit-distance len1 len2)
  (set! *lower* (make-hash))
  (set! *middle* (make-hash))
  (set! *upper* (make-hash))

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

 
  )

(define (global-align-gcon str1 str2 len1 len2)
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
					    (hash-ref *upper* `(,i ,j) 0))
			  )))
			(iota len2 1)))
  (iota len1 1)))
			  
(define (align-gcon str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2)))
    (init-edit-distance len1 len2)
    
    (global-align-gcon str1 str2 len1 len2)

    (hash-ref *middle* (list len1 len2))
    
  )
)
)


