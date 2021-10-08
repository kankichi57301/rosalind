;; rosalind
;; Implement NumberToPattern
;; [BA1M] 2021/07/06 AC 
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(define *ba1m_out* "ba1m_out.txt")

(define nuc "ACGT")

(define (ros_ba1m . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba1m.txt"
		    (format "rs_ba1m~a.txt" (car n)))))
	 (index (string->number (car data)))
	 (k     (string->number (cadr data)))
       )
    (number->pattern index k)
    #|
    (call-with-output-file *ba1m_out*
      (lambda(out)
	(display data out))
      #:exists 'truncate/replace)
    |#
))
(define (number->pattern index k)
  (string-take-right (string-append (make-string k #\A)
				    (number->pattern0 index))
		     k))

(define (number->pattern0 index)
  (if (= 0 index)
      ""
      (string-append (number->pattern0 (inexact->exact (floor (/ index 4))))
		     (substring nuc (modulo index 4) (+ 1 (modulo index 4))))))
  
				

