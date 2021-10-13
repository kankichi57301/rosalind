#lang racket
;; rosalind
;; Find the Most Frequent Words with Mismatches in a String
;; [BA1I] 2021/07/10 AC
;; 2021/10/13 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba1i_out* "data\\ba1i_out.txt")
(define myhash '())

(define (ros_ba1i . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba1i.txt"
		    (format "data\\rs_ba1i~a.txt" (car n)))))
	 (dna (car data))
	 (2ndline (string-tokenize (cadr data)))
	 (k (string->number (car  2ndline)))
	 (d (string->number (cadr 2ndline)))
         (myhash '())
	 (res '())
	 (maxtimes 0)
	 (ans '())
	 )
    
  (define (solve-ba1i dna k d)
    (solve-ba1i0 dna k d (string-length dna)))
  
  (define (solve-ba1i0 dna k d dnalen )
    (if (< dnalen k)
	#f
	(begin
	  (map (lambda(kmer)(inc-hash! myhash kmer))(all-combi (string-take dna k) d))
	  (solve-ba1i0 (string-drop dna 1) k d (- dnalen 1)))))



    (set! myhash (make-hash))
    (solve-ba1i dna k d)
    (set! res (hash-map myhash list))
    (set! maxtimes (cadr (max-item res (lambda(x)(cadr x)))))
    ;(displayln (format "maxtimes=~a" maxtimes))
    (set! ans (map car (filter (lambda(pair)(= maxtimes (cadr pair))) res)))
    
    (call-with-output-file *ba1i_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  ans))
      #:exists 'truncate/replace)
    ans
))

	 
				

