;; rosalind
;; 
;; [BA2B] 2021/05
;(require srfi/1)
(require srfi/13)
(include "roslib.ss")
(include "readfile.ss")
(define *ba2b_out* "***_out.txt")
(define myhash '())
(define myhash2 '())
(define wk '())
(define wk2 '())
(define (ros_ba2b . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba2b.txt"
		    (format "rs_ba2b~a.txt" (car n)))))
	 (k (string->number (car data)))
	 (dnas (cdr data))
	 (len (string-length (cadr data)))
	 ;(wk '())
	 )
    (set! myhash (make-hash))
    (set! myhash2 (make-hash))
    (define (register-kmers dna len k)
      ;(displayln (format "arg=~a" dna))
      (if (< len k)
	  #f
	  (begin
	    (inc-hash! myhash (string-take dna k))
	    (register-kmers (string-drop dna 1) (- len 1) k))))

    (define (register-kmers-dnas dnas k)
      (for-each (lambda(dna)(register-kmers dna len k))
		dnas))

    ;; 
 		  
    
    (register-kmers-dnas dnas k)
    (set! wk (hash-map myhash list))
    (displayln wk)

    (define (wk2-compare pair1 pair2)
      (if (= (caar pair1)(caar pair2))
	  (char<? (cadar pair1)(cadar pair2))
	  (< (caar pair1)(caar pair2))))

    
    (inc-hist wk k myhash2)
    (set! wk2 (hash-map myhash2 list))
    (sort wk2 wk2-compare)
    #|
    (call-with-output-file *maj_out*
      (lambda(out)
	(display data out))
      #:exists 'truncate/replace)
    |#
))

(define (inc-hist0 pair k hash)
  (for-each (lambda(x)
	      (inc-hash2! hash (list x (string-ref (car pair) x)) (cadr pair)))
	    (iota k)))

;; ‰–ŠîˆÊ’u‚²‚Æ‚ÌACGT‚Ì“x”‚ğ‹‚ß‚é„hash‚É“o˜^
(define (inc-hist pairlist k hash)
  (for-each (lambda(pair)(inc-hist0 pair k hash)) pairlist))



				

