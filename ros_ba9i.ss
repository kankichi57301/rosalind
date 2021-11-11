#lang racket
;; rosalind
;; Construct the Burrows-Wheeler Transform of a String
;; [BA9I] 2021/09/14 AC
;; 2021/11/11 AC モジュール構成変更
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "bwtA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")

(define *ba9i_out* "data\\ba9i_out.txt")
(define my-tree '())

(define (ros_ba9i . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba9i.txt"
		    (format "data\\rs_ba9i~a.txt" (car n)))))
	 (res (bwt (car data)))
	 )
    
    
    (call-with-output-file *ba9i_out*
      (lambda(out)
	(displayln res  out))
      #:exists 'truncate/replace)
    res
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

		
	      

