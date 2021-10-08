;; rosalind
;; Generate Contigs from a Collection of Reads
;; [BA3K] 2021/07/30
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba3k_out* "ba3k_out.txt")
(define k 3)
(define k-1 2)
(define dnac 0)
(define *dnalist* '())
(define *in* '())
(define *out* '())
(define *graph* '())



(define (ros_ba3k . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba3k.txt"
		    (format "rs_ba3k~a.txt" (car n)))))
	 (res '())
	 )
    (set! *in* (make-hash))
    (set! *out* (make-hash))
    (set! k (string-length (car data)))
    (set! k-1 (- k 1))
    (set! *dnalist* data)
    (set! dnac (length data))
    ;;
    (set! *graph* (get-all-graph data))
    (init-ba3k) 

    #|
    (call-with-output-file *ba3k_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a~%" kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
   #t
   ))

(define (count-in edge)
  (inc-hash! *in* (cadr edge)))
(define (count-out edge)
  (inc-hash! *out* (car edge)))
(define (init-ba3k)
  (for-each (lambda(edge)
	      (count-in edge)
	      (count-out edge))
	    *graph*))


(define (connect? former latter)
  (string=? (string-take-right former k-1)
	    (string-take       latter k-1)))

(define (get-all-graph dnas)
  (filter (lambda(pair) 
	    
	    (and (not (= (car pair)(cadr pair)))
			    (connect? (list-ref dnas (car pair))(list-ref dnas (cadr pair)))))
	    
	  (cartesian-expt (iota dnac) 2)))
