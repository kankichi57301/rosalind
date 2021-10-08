#lang racket
;; rosalind
;; 
;; [GC] 20**/**/**
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *gc_out* "gc_out.txt")

(define (ros_gc . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_gc.txt"
		    (format "rs_gc~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (names
	  (map (lambda(s)(string-drop s 1))
	       (filter (lambda(str)(string=? ">" (string-take str 1))) data)))
	 (gc-ratios (map gc-ratio dnas))
	 (res '())
	 )
    (set! res (max-item  (zip names gc-ratios ) cadr))
    
    
    (call-with-output-file *gc_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
    res
    
))

(define (gc-ratio dnastr)
  (let ((dnalist (string->list dnastr)))
    (roundp3 (/ (count(lambda(x)(or (equal? x #\C)(equal? x #\G))) dnalist)
		(length dnalist)))))


