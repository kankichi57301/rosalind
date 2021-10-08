#lang racket
;; rosalind
;; Compute the Squared Error Distortion
;; [BA8B] 2021/09/15 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba8b_out* "ba9b_out.txt")

(define (ros_ba8b . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba8b.txt"
		    (format "rs_ba8b~a.txt" (car n)))))
	 (firstline  (string->numlist (car data)))
	 (k (car firstline))
	 (m (cadr firstline))
	 (centers (map string->numlist (take (cdr data) k)))
	 (points  (map string->numlist (drop data (+ k 2))))
	 (n (length points))
	 )
   (sqer-dist points centers)
    #|
    (call-with-output-file *ba8b_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))


;;
;;Squared Error Distortion
;;


(define (sqer point center)
  (apply + (map(lambda(x)(* x x))(map - point center))))
  
(define (sqerA point centers)
  (apply min (map (lambda(c)(sqer point c)) centers)))


(define (sqer-dist points centers)
  (/
   (apply + (map (lambda(p)(sqerA p centers)) points))
   (length points)
   ))
  


