;; rosalind
;; Find Frequent Words with Mismatches and Reverse Complements
;; [BA1J] 2021/07/
;(require srfi/1)
(require srfi/13)
(require srfi/19)

(include "readfile.ss")
(include "roslib.ss")
(include "monoisotopic.ss")

(define *time* #f)
(define *ba1j_out* "ba1j_out.txt")
(define *dna* "")
(define myhash '())
(define myhash2 '())

(define (ros_ba1j . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba1j.txt"
		    (format "rs_ba1j~a.txt" (car n)))))
	 (dna (car data))
	 (dnalen (string-length dna))
	 (2ndline (string-tokenize (cadr data)))
	 (k (string->number (car 2ndline)))
	 (halfk (ceiling (/ k 2)))
	 (d (string->number (cadr 2ndline)))
	 (res '())
	 )

    
    
    (define (init-hash k)
      
	    (for-each (lambda(i)
			(for-each (lambda (kmer)
				    (hash-set! myhash (list i kmer) (hamming-distance kmer (string-drop dna i))))
				  (all-dnas halfk)))
			(iota (- dnalen halfk)))
		      
	    (when (odd? k)
		  (for-each (lambda(i)
			      (for-each (lambda(kmer)
					  (hash-set! myhash (list i kmer) (hamming-distance kmer (string-drop dna i))))
					(all-dnas (- halfk 1))))
			    (iota (- dnalen (- halfk 1))))))
	    

    (define (disp-hash n)
      (for-each displayln (take (hash-map myhash list) n)))
;;kmer‚Ì”¼•ª‚Ædna‚Ìƒnƒ~ƒ“ƒO‹——£
    (define (h-d-half pos halfkmer)
      (hash-ref myhash (list pos halfkmer)))

    
    (define (count-all-kmers dna)
      (for-each (lambda(i)
		  (for-each (lambda(kmer1)
			      (let ((d1 (h-d-half i kmer1)))
				(when (<= d1 k)
				      (for-each (lambda(kmer2)
						  (let ((d2 (h-d-half (+ i halfk) kmer2)))
						    (when (<= (+ d1 d2) d)
							  (let ((whole-kmer (format "~a~a" kmer1 kmer2)))
							    ;(displayln (format "~a:~a" (+ d1 d2) k))
							    (inc-hash! myhash2 whole-kmer)))))
						(all-dnas (if (even? k) halfk (- halfk 1)))))))
			      (all-dnas halfk)))
		(iota (- dnalen k))))
			     

			     
    
    (displayln dna)
    (set! *dna* dna)
    ;(set! *time* (current-time))
    (set! myhash (make-hash))
    (set! myhash2 (make-hash))
    (init-hash k)
    (count-all-kmers dna)


    #|
    (call-with-output-file *ba1j_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
    |#
    #t
))



(define (test04 kmer)
  (hash-ref myhash2 kmer 0))


;;
;;
(ros_ba1j 3)
