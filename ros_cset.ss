#lang racket
;; rosalind
;; Fixing an Inconsistent Character Set
;; [CSET] 2021/04/22 AC
;; 2012/10/21 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(require "roslibB.ss")

(define 2bit-all '((#\0 #\0) (#\0 #\1) (#\1 #\0) (#\1 #\1)))
(define *data* '())
(define *cset-out* "data\\cset_out.txt")

(define (ros_cset . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_cset.txt"
		    (format "data\\rs_cset~a.txt" (car n)))))
	 (ndata (map string->list data))
	 (res '())
       )

    (set! res (delete-nth data (find-inconsistent-pos ndata)))
    (call-with-output-file *cset-out*
      (lambda(out)
	(for-each (lambda(x)(displayln (format "~a" x) out))
		  res))
      #:exists 'truncate/replace)
    res
))
;;--*-- test routine--*--
;(ros_cset 1)

(define (inconsistent-char charlist1 charlist2)
  (if  (include? 2bit-all
		 (map list charlist1 charlist2))
       1 0))

(define (inconsitent-all-combi charlistlist)
  (map (lambda(x)
	 (apply +
		(map (lambda(y)
		       (inconsistent-char x y))
		     charlistlist)))
       charlistlist))

(define (find-inconsistent-pos charlistlist)
  (let* ((incons-count (inconsitent-all-combi charlistlist))
	 (pos (find-first-pos (lambda(x)(> x 1)) incons-count)))
    (if pos
	pos
	(find-first-pos (lambda(x)(> x 0)) incons-count))))
    

		     
