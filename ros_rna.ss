#lang racket
;; rosalind
;; Transcribing DNA into RNA
;; [RNA] 2021/10/07
;; @kankichi57301

(require srfi/13)
(require "readfileA.ss")
(define *rna_out* "data\\rna_out.txt")

(define (ros_rna . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_rna.txt"
		    (format "data\\rs_rna~a.txt" (car n)))))
	 (res '())
	 )
    (set! res
	  (apply string
		 (map (lambda(x)(if (equal? x #\T) #\U x))(string->list (car data)))))

    
    (call-with-output-file *rna_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
    
    res
))



