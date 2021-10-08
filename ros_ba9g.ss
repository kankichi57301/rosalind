#lang racket
;; rosalind
;; Construct the Suffix Array of a String
;; [BA9G] 2021/09/14 AC

(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")

(define *ba9g_out* "ba9g_out.txt")
(define my-tree '())

(define (ros_ba9g . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba9g.txt"
		    (format "rs_ba9g~a.txt" (car n)))))
	 (root '())
	 )
    
    
    (call-with-output-file *ba9g_out*
      (lambda(out)
	(displayln (string-join (map number->string (suffix-array (car data))) ", ") out))
      #:exists 'truncate/replace)
    
    #t
    
    ))




;;
;;suffixarray.ss
;;
(define (all-suffix str)
  (if (= 1 (string-length str))
      (list str)
      (cons str (all-suffix (string-drop str 1)))))

(define (suffix-array0 str)
  (sort (all-suffix str) string<?))

(define (suffix-array str)
  (let ((len (string-length str)))
    (map (lambda(str)(- len (string-length str)))
	 (suffix-array0 str))))
