#lang racket
;; rosalind
;;Find the Longest Substring Shared by Two Strings
;; [BA9E] 2021/11/25 AC
;(require srfi/1)
(require racket/hash)
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")

(define *ba9e_out* "data\\ba9e_out.txt")
(define *dp* #f)

(define (ros_ba9e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba9e.txt"
		    (format "data\\rs_ba9e~a.txt" (car n)))))
	 (str1 (car data))
	 (str2 (cadr data))
	 (res "")
	 )
    (set! *dp* (make-hash))
    (let-values ([(val x y)(fill-dp str1 str2)])
      ;;(displayln (format "i=~a j=~a val=~a" x y val))
      (set! res (substring str1 (- x val -1) (+ x 1))))
    
    (call-with-output-file *ba9e_out*
      (lambda(out)
	(displayln res out))
      #:exists 'truncate/replace)
    ))

(define (fill-dp str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	(max-i -1)
	(max-j -1)
	(max-v -1)
	)
    (for-each (lambda(i)(hash-set! *dp* `(,i 0) 0))(iota len1))
    (for-each (lambda(j)(hash-set! *dp* `(0 ,j) 0))(iota len2))
    (for-each (lambda(i)
		(for-each (lambda(j)
			    (let ((val 
				   (if (equal? (string-ref str1 i)(string-ref str2 j))
				       (+ 1 (hash-ref *dp* `(,(- i 1) ,(- j 1))))
				       0)))
			      (hash-set! *dp* `(,i ,j) val)
			      (when (> val max-v)
				    (set! max-v val)
				    (set! max-i i)
				    (set! max-j j))))
			  (iota (- len2 1) 1)))
	      (iota (- len1 1) 1))
    (values max-v max-i max-j)))
				
