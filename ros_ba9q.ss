#lang racket
;; rosalind
;; 
;; [BA9Q] 2022/01/25 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")

(define *ba9q_out* "data\\ba9q_out.txt")

(define (ros_ba9q . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba9q.txt"
		    (format "data\\rs_ba9q~a.txt" (car n)))))
	 (str (car data))
	 (k (string->number (cadr data)))
	 (res '())
	 )
    ;;(displayln str)
    ;;(displayln k)
    (set! res (solve-ba9q str k))
    
    (call-with-output-file *ba9q_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a,~a " (car x)(cadr x)) out))
		  res))
      #:exists 'truncate/replace)
    res
    
))


;;--*-- test data --*--
;;(define str5 "PANAMABANANAS$")  

(define (all-suffix str)
  (sort
   (map(lambda (i)(string-take-right str i))
       (iota (string-length str) 1))
   string<))

(define (suffix-array str)
  (let ((len (string-length str)))
    (map (lambda(x)(- len (string-length x)))
	 (all-suffix str))))

(define (solve-ba9q str k)
  (let ((s-a (suffix-array str))
	(len (string-length str)))
    (filter (lambda(x)(= 0 (modulo (cadr x) k)))
	    (zip (iota len) s-a))))
    
