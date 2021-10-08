#lang racket
;; rosalind
;; Find All Occurrences of a Pattern in a String
;; [BA1D] 2021/07/28 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(define *ba1d_out* "ba1d_out.txt")

(define (ros_ba1d . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba1d.txt"
		    (format "rs_ba1d~a.txt" (car n)))))
	 (pat (car data))
	 (gnome (cadr data))
	 (res (find-all-pos gnome pat))
       )
    (displayln res)
    
    (call-with-output-file *ba1d_out*
      (lambda(out)
	(for-each (lambda(n)(display (format "~a " n) out)) res))
      #:exists 'truncate/replace)
    #t
))

(define (find-all-pos str search )
  (reverse (find-all-pos0 str search (string-length search) 0 '())))

(define (find-all-pos0 str search len pos acc)
  (if (< (string-length str) len)
      acc
      (if (string=? (string-take str len) search)
	  (find-all-pos0 (string-drop str 1) search len (+ 1 pos)(cons pos acc))
	  (find-all-pos0 (string-drop str 1) search len (+ 1 pos) acc))))

