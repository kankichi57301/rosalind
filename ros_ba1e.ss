;; rosalind
;; Find Patterns Forming Clumps in a String
;; [BA1E] 2021/07/08 WA=> AC 
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(define *ba1e_out* "ba1e_out.txt")
(define kmer-hash '())
(define *DEBUG* #f)
(define (ros_ba1e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba1e.txt"
		    (format "rs_ba1e~a.txt" (car n)))))
	 (dna (car data))
	 (2ndline (string-tokenize (cadr data)))
	 (k (string->number (car 2ndline)))
	 (L (string->number (cadr 2ndline)))
	 (t (string->number (caddr 2ndline)))
	 (res '())
	 )
    (set! kmer-hash (make-hash))
    ;(displayln (list dna k L t))
    (register-kmers dna k)

    (if *DEBUG*
	(set! res
	      
	      (filter (lambda(pair)(>= (length (cadr pair)) t))
		      (hash-map kmer-hash (lambda(x y)(list x(sort  y <))))))
    ;;--*-- NO DEBUG
	(set! res
	      (map car
		   (filter (lambda(pair)(and (>= (length (cadr pair)) t)
					     (range-include (cadr pair) k L t)))
			   (hash-map kmer-hash (lambda(x y)(list x(sort  y <)))))))
    )
		   
    (call-with-output-file *ba1e_out*
      (lambda(out)
	(for-each
	 (lambda(str)
	   (display (format "~a " str) out))
	 res))
      #:exists 'truncate/replace)
    
    #t
    ;res
))

(define (register-kmers dna k)
  (register-kmers0 dna k 0))

(define (register-kmers0 dna k pos)
  (if (< (string-length dna) k)
      #t
      (let ((kmer (string-take dna k)))
	(hash-set! kmer-hash kmer (cons pos (hash-ref kmer-hash kmer '())))
	(register-kmers0 (string-drop dna 1) k (+ 1 pos))
	)))

;; nlist ¸‡‚Å‚ ‚é‚±‚Æ ’·‚³L‚Ì‹æŠÔ‚É•¶Žš—ñ‚ªtŒÂˆÈã‚ ‚é‚©B
(define (range-include nlist k L t )
  
  (if (< (length nlist) t)
      #f
      (begin
	;;(displayln (format "~a:~a ~a dif=~s" (car nlist)(list-ref nlist (- t 1)) L (- (list-ref nlist (- t 1))(car nlist))))
	(if (<=  (- (list-ref nlist (- t 1))(car nlist)) (- L k))
	    #t
	    (range-include (cdr nlist) k L t)))))
