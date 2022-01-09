#lang racket
;; rosalind
;; Compute the Size of a Spectral Dictionary
;; [BA11I] 2022/01/09 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "monoisotopicA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba11i_out* "ba11i_out.txt")
(define *size* '())
(define *maxweight* 0)

(define (ros_ba11i . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba11i.txt"
		    (format "data\\rs_ba11i~a.txt" (car n)))))
	 (spect (map string->number (string-tokenize (car data))))
	 (thres (string->number (cadr data)))
	 (max_sc (string->number (caddr data)))
	 (spectlen (length spect))
	 )
    (set! *size* (make-hash))
    (displayln (format "spect len=~a" spectlen))
    (displayln (dictsize spect thres max_sc amw4))
    
    #|
    (call-with-output-file ba11i_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

;;--*--
;; amw = amino acid weight
;;
(define (dictsize spect thres maxsc amw)
  (hash-set! *size* '(0 0) 1)
  (apply + (map (lambda(t)(getsize (length spect) t spect amw))
		(range thres (+ 1 maxsc)))))

(define (getsize i t spect amw)
  ;(displayln (format "getsize i=~a t=~a" i t))
  (let ((it (hash-ref *size* `(,i ,t) #f)))
    (if it
	it
	(if (or (<= i 0)(< t 0))
	    (begin
	      (hash-set! *size* `(,i ,t) 0)
	      0)
	    (let ((s (apply + (map (lambda(m)(* 0.05 (getsize (- i m)(- t (list-ref spect (- i 1))) spect amw)))
				   amw))))
	      (hash-set! *size* `(,i ,t) s)
	      s)))))
