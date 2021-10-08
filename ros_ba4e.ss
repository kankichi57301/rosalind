;; rosalind
;; Find a Cyclic Peptide with Theoretical Spectrum Matching an Ideal Spectrum
;; [BA4e] 2021/07/
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "monoisotopicA.ss")
(require "roslibA.ss")
(require "roslibB.ss")
(define myhash #f)
(define *ba4e_out* "ba4e_out.txt")
(define *myhash* #f)

(define (ros_ba4e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba4e.txt"
		    (format "rs_ba4e~a.txt" (car n)))))
	 (nlist (map string->number (string-tokenize (car data))))
	 (len (length nlist))
	 (amino-count (calc-amino-count len))
	 (aminos '())
	 
	 )
    
   ;;(displayln nlist)
    (displayln amino-count)
    ;;(displayln aminos)
    (displayln totalw)
    (cand-amino nlist)
    #|
    (call-with-output-file *****_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

(define (calc-amino-count len)
  (tri-num->num (/ (- len 2) 2)))


;; return a[0],a[0]+a[1],a[0]+a[1]+a[2] ....
(define (all-running-sum nlist)
  (all-running-sum0 nlist 0))
  
(define (all-running-sum0 nlist acc)
  (if (empty? nlist)
      '()
      (let ((sum (+ acc (car nlist))))
	(cons sum (all-running-sum0 (cdr nlist) sum)))))
;; return a[0],a[0]+a[1],a[0]+a[1]+a[2] a[n-1]+a[0]+...(until a[n-2]) a[n-2]+a[n-1]+a[0]+...
(define (all-running-sum-circular nlist n)
  (all-running-sum-circular0 (append nlist nlist) n))

(define (all-running-sum-circular0 nlist n)
  (append-map (lambda(from)
		(map (lambda(to)
			;;(list from to))
		       (list-span nlist from to))
		        ;;(apply + (list-span nlist from to)))
		     (range n (+ from (- n 1)))))
	      (range 2  n )))
		
;;(all-running-sum-circular nlist n)'(0 1 2 3 4) 5)		       
		       

;;(define (add-amino-weight? nlist new)

;;--*-- make test data --*--
(define (all-amino-weight-circular nlist)
  (let ((dbl (append nlist nlist))
	(len (length nlist))
	(all (apply + nlist))
	)
    
    (append (cons 0
		  (sort
		   (append-map (lambda(from)
				 (map (lambda(len)
					(apply + (list-span dbl from (+ from len ))))
				      (iota (- len 1))))
			       (iota len))
		   <))
	    (list all))))

(define (solve-ba4e2 spect)
  (solve-ba4e1 spect (set-intersect spect all-amino-weights)))

(define (solve-ba4e1 spect amino-nlist)
  (displayln (format "amino=~a" amino-nlist))
  (solve-ba4e0 spect (list (car amino-nlist)) (cdr amino-nlist) (length amino-nlist)))
		     

(define (solve-ba4e0 spect acc amino-nlist amino-len)
  (displayln (format "acc=~a rest=~a sp-len=~a" acc amino-nlist (length spect)))
  (if (empty? amino-nlist)
      (begin
	(displayln (format "ans=~a" acc))
	acc)
      (for-each (lambda(wt)
		  (let* ((nextacc (cons wt acc))
			 (runsum (all-running-sum nextacc)))
		    ;(displayln (format "nextacc=~a" nextacc))
		    (if (set-include? runsum spect)
			(solve-ba4e0 (exclude runsum spect)(cons wt acc)(delete-once wt amino-nlist)(- amino-len 1))
			#f)))
		amino-nlist)))
			 
(define (cand-amino nlist)
  (let ((cand1 (cand-amino1 nlist))
	(totalw (car (take-right nlist 1))))
    (cand-amino2 cand1 totalw)))

(define (cand-amino2 cand totalw)
  ;(displayln (format "cand=~a" cand))
  (if (= totalw (apply + cand))
      (begin
	(displayln (format "ans=~a" cand))
	cand)
      (for-each (lambda (conflict)(when (include? conflict cand)
					(cand-amino2 (delete-once (caddr conflict) cand) totalw)))
		conflict-diamino)))
  
;;
;; amino-acid‚Ìweight-table‚Æ‚ÌÏW‡‚ðŽæ‚é‚¾‚¯
(define (cand-amino1 nlist)
  (set-intersect nlist all-amino-weights))
  
;;;--*-- test data --*-- --*--
(define spect01 (all-amino-weight-circular '(71 128 113 147)))
(define spect011 (all-amino-weight-circular '(99 128 71 57 113 147)))
(define spect012 (all-amino-weight-circular '(57 57 71 115 186 128 114)))
(define spect02 (all-amino-weight-circular '(71 115 97 186)))
(define spect03 (all-amino-weight-circular '(71 115 71 71 115 97)))
(define spect04 (all-amino-weight-circular '(71 115 71 115 71 97)))
