#lang racket
;; rosalind
;; Reconstruct a String from its Burrows-Wheeler Transform
;; [BA9I] 2021/09/14 AC
;;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")

(define *ba9j_out* "ba9j_out.txt")


(define (ros_ba9j . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba9j.txt"
		    (format "rs_ba9j~a.txt" (car n)))))
	 )
    
    
    (call-with-output-file *ba9j_out*
      (lambda(out)
	(displayln (rev-bwt (car data)) out))
      #:exists 'truncate/replace)
    
    #t
    
    ))



;;filename:bwt.ss
;;Burrows-Wheeler Transform
;;
(require srfi/1)
(require srfi/13)
(define (all-rot str)
  (map (lambda(n)(rot-str str n))(iota (string-length str))))

(define (rot-str str n)
  (string-append (string-drop str n)(string-take str n))) 



(define (bwt str)
  (apply string-append
	 (map (lambda(s)(string-take-right s 1))
	      (sort (all-rot str) string<?))))

(define (str-paste strlist1 strlist2)
	  (map string-append strlist1 strlist2))

(define (rev-bwt bwt)
  (let ((bwtlist (map string (string->list bwt))))
    (car
     (filter (lambda(s)(string=? (string-take-right s 1) "$"))
	     (rev-bwt0 bwtlist bwtlist (- (string-length bwt) 1))))))

(define (rev-bwt0 from bwt n)
  ;(displayln (format "rev-bwt arg=~a" from))
  (if (= n 0)
      from
      (rev-bwt0 (str-paste bwt (sort from string<?)) bwt (- n 1))))

