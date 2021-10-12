#lang racket
;; rosalind
;; Edit Distance
;; [EDIT] 2021/10/12 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")

(define *edit_out* "data\\edit_out.txt")

(define (ros_edit . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_edit.txt"
		    (format "data\\rs_edit~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (res (apply edit-distance dnas))
	 )
    
    (call-with-output-file *edit_out*
      (lambda(out)
	(displayln  res out))
      #:exists 'truncate/replace)
   res
))

;;
;;edit-distance
;;

(define *my-hash* '())

(define (init-edit-distance len1 len2)
  (set! *my-hash* (make-hash))
  (for-each (lambda(x)(hash-set! *my-hash* `(,x 0) x))(iota (+ 1 len1)))
  (for-each (lambda(y)(hash-set! *my-hash* `(0 ,y) y))(iota (+ 1 len2)))
  )

(define (fill-edit-distance-table str1 str2 len1 len2)

  (for-each (lambda(y)
	      (for-each (lambda(x)

			  
			  (let* ((c (if (equal? (string-ref str1 (- x 1))
					       (string-ref str2 (- y 1)))
				       0 1))
			    
				 (val (min (+ 1 (hash-ref *my-hash* (list (- x 1) y)))
					    (+ 1 (hash-ref *my-hash* (list x  (- y 1))))
					    (+ c 
					       (hash-ref *my-hash* (list (- x 1) (- y 1)))))))

			  (hash-set! *my-hash* (list x y) val)
			  ;;(displayln (list x y val))
			  ))

			(iota len1 1)))
  (iota len2 1)))
			  

(define (edit-distance str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2)))
    (init-edit-distance len1 len2)
    (fill-edit-distance-table str1 str2 len1 len2)
    (hash-ref *my-hash* (list len1 len2))
  )
)




