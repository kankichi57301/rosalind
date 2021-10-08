;; rosalind
;; 
;; HMM Parameter Estimation Problem
;; [BA10H] 2012/07/03 AC
(require srfi/1)
(require srfi/13)
(require srfi/48)
(include "readfile.ss")
(include "roslib.ss")
(define *ba10h_out* "ba10h_out.txt")
(define matA '())
(define matB '())


(define (ros_ba10h . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba10h.txt"
		    (format "rs_ba10h~a.txt" (car n)))))
	 (emit-seq (string->list (car data)))
	 (seqlen (length emit-seq))
	 (sigma (map (lambda(x)(string-ref x 0))
		     (regexp-match* #rx"[A-Za-z]" (third data))))
	 (nsigma (length sigma))
	 (state-seq (string->list (fifth data)))
	 (state (map (lambda(x)(string-ref x 0))
		     (regexp-match* #rx"[A-Za-z]" (seventh data))))
	 )

    (define (inc-hash! hash key)
      (hash-set! hash key (+ 1 (hash-ref hash key 0))))

    (define (make-trans-list lst)
      (map list lst (drop lst 1)))

    (define (conv-ratio nlist)
      (let ((sum (* 1.0 (apply + nlist))))
	(if (= 0 sum)
	    (make-list nsigma (roundp3 (/ 1.0 nsigma)))
	    (map (lambda(x)(roundp3  (/ x sum)))  nlist))))

    (define (outA out)
      (displaylist (cons "  " state) out)
      (for-each (lambda(x)
		  (displaylist
		   (cons x
			 (conv-ratio
			  (map (lambda(y)(hash-ref matA (list x y) 0))
			 state))) out))
		state))
    
    (define (outB out)
      (displaylist (cons "  " sigma) out)
      (for-each (lambda(x)
		  (displaylist
		   (cons x
			 (conv-ratio
			  (map (lambda(y)(hash-ref matB (list x y) 0))
			 sigma))) out))
		state))

    #|
    (displayln emit-seq)
    (displayln sigma)
    (displayln state-seq)
    (displayln state)
    (displayln seqlen)
    |#
    (set! matA (make-hash))
    (map (lambda(x)(inc-hash!  matA  x))
	 (make-trans-list state-seq))
    
    (set! matB (make-hash))
    (map (lambda(x y)(inc-hash! matB `(,x ,y)))
	 state-seq
	 emit-seq)
 

    (call-with-output-file *ba10h_out*
      (lambda(outport)
	(outA outport)
	(displayln "--------" outport)
	(outB outport))
      #:exists 'truncate/replace)

#t

))
