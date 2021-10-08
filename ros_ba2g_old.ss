;; rosalind
;; 
;; [BA2G] 20**/**/**
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(define *ba2g_out* "*ba2g_out.txt")

(define (ros_ba2g . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba2g.txt"
		    (format "rs_ba2g~a.txt" (car n)))))
	 (1stline (string-tokenize (car data)))
	 (k (string->number (car 1stline)))
	 (t (string->number (cadr 1stline)))
	 (N (string->number (caddr 1stline)))
	 (dnas (cdr data))
	 )
    (gibbs-sampler dnas k t N)
       #|
    (call-with-output-file *ba2g_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    #t
))

(define (gibbs-sampler dnas k t N)
  (let* ((len (string-length (car dnas)))
	 (bestmotif (map (lambda(dna)(string-take (string-drop dna (random (+ 1 (- len k)))) k))
			 dnas))
	 (i -1))
    
    (displayln bestmotif)


  ))

