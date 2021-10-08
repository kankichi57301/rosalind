;; rosalind
;; Generate the Convolution of a Spectrum
;; [BA4H] 2021/07/31 AC
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(define *ba4h_out* "ba4h_out.txt")

(define (ros_ba4h . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba4h.txt"
		    (format "rs_ba4h~a.txt" (car n)))))
	 (spect (map string->number (string-tokenize (car data))))
	 (res (convolution spect))
	 )
    
    
    (call-with-output-file *ba4h_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res)
	(display "\n" out))
      #:exists 'truncate/replace)
    #t
    ;res
))




(define (convolution nlist)
  (apply append
	      (sort
	       (group-by identity
			 (filter positive?   ;; 0‚ğœŠO‚·‚é‚±‚Æ@2021/07/31
				 (map (lambda(x)(abs (apply - x)))(combinations nlist 2))))
	       (lambda(x y)(if (= (length x)(length y))
			       (string<? (number->string (car x))(number->string(car y)))
			       (> (length x)(length y)))))))
			       
