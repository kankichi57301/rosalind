;; rosalind
;; Solve the Turnpike Problem
;; [BA4M] 2021/08/09
(require srfi/1)
(require srfi/13)
(require srfi/19)
(require "readfileA.ss")
(require "roslibA.ss")
(include "turnpike.ss")
(define *ba4m_out* "ba4m_out.txt")
(define *time* #f)

(define (ros_ba4m . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba4m.txt"
		    (format "rs_ba4m~a.txt" (car n)))))
	 (nlist (map string->number (string-tokenize (car data))))
	 (nlist+ (filter positive? nlist))
	 (res '())
	 )

    (set! res (turnpike nlist+))

    
    (call-with-output-file *ba4m_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    res
    
))

(define (test-ba4m nlist)
    (set! *time* (current-time))  
    (displayln (turnpike nlist))
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*)))))
