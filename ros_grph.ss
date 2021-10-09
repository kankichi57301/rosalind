#lang racket
;; rosalind
;; 
;; [GRPH] 2021/10/09
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *grph_out* "data\\grph_out.txt")

(define (ros_grph . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_grph.txt"
		    (format "data\\rs_grph~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (names
	  (map (lambda(s)(string-drop s 1))
	       (filter (lambda(str)(string=? ">" (string-take str 1))) data)))
	 (pairs (zip names dnas))
	 )
    
    
    (call-with-output-file *grph_out*
      (lambda(out)
	(for-each (lambda(x)
		    (for-each (lambda(y)
				(when (and
				       (not (string=? (cadr x) (cadr y)))
				       (str-overlap (cadr x) (cadr y) 3))
				      (displayln (format "~a ~a" (car x)(car y)) out )))
			      pairs))
		  pairs))
      #:exists 'truncate/replace)
    #t
))
;; str1 ‚Æstr2‚ªn•¶Žšoverlap‚µ‚Ä‚¢‚é‚©
(define (str-overlap str1 str2 n)
  (string=? (string-take-right str1 n)
	    (string-take       str2 n)))
