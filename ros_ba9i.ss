#lang racket
;; rosalind
;; Construct the Burrows-Wheeler Transform of a String
;; [BA9I] 2021/09/14 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")

(define *ba9i_out* "ba9i_out.txt")
(define my-tree '())

(define (ros_ba9i . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba9i.txt"
		    (format "rs_ba9i~a.txt" (car n)))))
	 )
    
    
    (call-with-output-file *ba9i_out*
      (lambda(out)
	(displayln (bwt (car data)) out))
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

;;filename:bwt.ss
;;Burrows-Wheeler Transform
;;

(define (all-rot str)
  (map (lambda(n)(rot-str str n))(iota (string-length str))))

(define (rot-str str n)
  (string-append (string-drop str n)(string-take str n))) 

(define (bwt str)
  (apply string-append
	 (map (lambda(s)(string-take-right s 1))
	      (sort (all-rot str) string<?))))
