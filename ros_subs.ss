#lang racket
;; rosalind
;; Finding a Motif in DNA
;; [SUBS] 2021/10/09
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *subs_out* "data\\subs_out.txt")

(define (ros_subs . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_subs.txt"
		    (format "data\\rs_subs~a.txt" (car n)))))
	 (res '())
	 )
    (set! res (apply find-all-pos data))
    
    (call-with-output-file *subs_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
    res
    
))

(define (find-all-pos str search)
  (reverse
   (find-all-pos0 str search (string-length str) (string-length search) 1 '())))

(define (find-all-pos0 str search len len-search pos acc)
  (if (< len len-search)
      acc
      (if (string=? search (string-take str len-search))
	  (find-all-pos0 (string-drop str 1) search (- len 1) len-search (+ pos 1) (cons pos acc))
	  (find-all-pos0 (string-drop str 1) search (- len 1) len-search (+ pos 1) acc))))
