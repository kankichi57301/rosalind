;; rosalind
;; 
;; [BA2G] 20**/**/**
;(require srfi/1)
(require srfi/13)
(include "roslib.ss")
(include "readfile.ss")
(define *ba2g_out* "*ba2g_out.txt")

(define (ros_ba2g . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba2g.txt"
		    (format "rs_ba2g~a.txt" (car n)))))
	 (1stline (string-tokenize (car data)))
	 (k (string->number (car 1stline)))
	 (t (string->number (cadr 1stline)))
	 (N (string->number (caddr 1stline)))
	 (dnas (cdr data))
	 )
    (gibbs-sampler dnas k t N)
       #|
    (call-with-output-file *ba2g_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

(define (gibbs-sampler dnas k t N)
  (let* ((len (string-length (car dnas)))
	 (bestmotif (map (lambda(dna)(string-take (string-drop dna (random (+ 1 (- len k)))) k))
			 dnas))
	 (i -1)
	 (profile '())
	 (deleted-1-line-motif '())
	 (ith-seq "")
	 (motifi "")
	 (newmotif '())
	 (bestscore (score-motif-matrix-str bestmotif))
	 (newscore  0)
	 )
;;--*-- --*--
    (define (loop n)
      ;(displayln (format "n=~a" n))
      
      (if (> n 0)
	   (begin
	     (set! i (random t))
	     
	     (set! deleted-1-line-motif (delete-nth bestmotif i))
	     ;(display-motif bestmotif i)
	     (set! profile    (motifs->profile-pseudo-count (map string->list deleted-1-line-motif)))
					;(displayln profile)
	     (set! ith-seq (list-ref dnas i))
	     ;; --*-- tbc Profile-randomly generated k-mer
	     (set! motifi (apply string (profile-random-kmer (string->list ith-seq) profile k)))
	     ;(displayln (format "new ~ath motif= \33[34m~a\33[39m" i  motifi))
	     ;; --*--
	     (set! newmotif (list-set bestmotif i motifi))  ;; replace i-th motif
	     ;; --*--
	     ;(display-motif newmotif i)
	     (set! newscore (score-motif-matrix-str newmotif))
	     (when (= n 1)
		   (displayln (format "best sc=~a newsc=~a" bestscore newscore)))
	     (when (< newscore bestscore)
		   (begin 
		     (set! bestmotif newmotif)
		     (set! bestscore newscore)
		     ))
		   
	     (loop (- n 1))
	     )
	   bestmotif
	   ))

      
  
;;--*-- --*--
    (loop N)
  ))

(define (display-motif motif . n)
  (let ((num (if (empty? n)
		 -1
		 (car n))))
    (for-each (lambda(i lis)
	       (if (= i num)
		   (display (format "\33[31m~a \33[39m" lis))
		   (display (format "~a " lis))))
	     (iota (length motif))
	     motif)
    (display "\n")
    ))
;; --*-- tbc
(define (profile-random-kmer dnacharlist profile k)
 (let* ((all-prob (profile-prob-all dnacharlist profile k))
	(total (apply + all-prob))
	(r (* (random) total))
	(n (find-run-sum-pos all-prob r))
	)
   (displayln (format "prob total=~a" total))
   (take (drop dnacharlist n) k)

   ))

	      
;; num <= sum(nlist)
(define (find-run-sum-pos nlist num)
  (find-run-sum-pos0 nlist num 0))

(define (find-run-sum-pos0 nlist num ans)
  (if (null? nlist)
      -1
      (if (<= num (car nlist))
	  ans
	  (find-run-sum-pos0 (cdr nlist)(- num (car nlist))(+ 1 ans)))))

  
(define (profile-prob-all dnacharlist profile k)
  (if (< (length dnacharlist) k)
      '()
      (cons (kmer-prob (take dnacharlist k) profile)
	    (profile-prob-all (cdr dnacharlist) profile k))))


  
;; test --*--
(define prof1 '((1 2 1 2)(1 4 3 1)(3 1 1 1)))
(define dnacharlist1 (string->list "AAATTCCGG"))
