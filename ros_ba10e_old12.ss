#lang racket
;; rosalind
;;Construct a Profile HMM
;; [BA10E] 2012/06/20 
(require srfi/1)
(require srfi/13)

(require "readfileA.ss")
(require "roslibA.ss")
(require "makeprofileA.ss")
(require "monoisotopicA.ss")
(define *DEBUG* #t)
(define *FILE_OUT* #t)
(define *ba10e_out* "ba10e_out.txt")
;;--*-- output tables --*--
(define *transition* #f) ;; key pair of string,value real number
(define *emit* #f)       ;; key string,value real number

(define *sigma* '())
(define *aligns* '())
(define *tr-align* '())
(define matA #f)
(define matB #f)
(define *all-state* '())
(define *tr* '())


;;
;; states S,E,Mn,In,Dn
;;
(define (ros_ba10e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba10e.txt"
		    (format "rs_ba10e~a.txt" (car n)))))
	 (theta (string->number (car data)))   ;; threshold
	 (sigma (map (lambda(x)(string-ref x 0))  ;;ï¿½oï¿½Í‹Lï¿½ï¿½
		     (regexp-match* #rx"[A-Za-z]" (third data))))
	 (sigmas (cons #\- sigma))
	 (sym-cnt (length sigma))   ;;ï¿½oï¿½Í‹Lï¿½ï¿½ï¿½Ìï¿½ï¿½ï¿½
	 (aligns (map (lambda(y)
			(map (lambda(x)(string-ref x 0))
			     (regexp-match* #rx"[A-Za-z\\-]" y)))
		      (drop data 4)))
	 (seqlen (length (car aligns)))    	 ;;ï¿½Aï¿½ï¿½ï¿½Cï¿½ï¿½ï¿½ï¿½ï¿½gï¿½ï¿½ï¿½Ì’ï¿½ï¿½ï¿½ï¿½iï¿½ï¿½ï¿½Êj
	 (seqcnt (length aligns))              	 ;;ï¿½Aï¿½ï¿½ï¿½Cï¿½ï¿½ï¿½ï¿½ï¿½gï¿½Ì–{ï¿½ï¿½
	 (tr-align (transpose aligns))	         ;;ï¿½]ï¿½uï¿½ï¿½ï¿½ï¿½ï¿½Aï¿½ï¿½ï¿½Cï¿½ï¿½ï¿½ï¿½ï¿½g
	 (st '())
	 (states '())
	 (all-state '()) ;;ï¿½Sï¿½Ä‚ï¿½state (S,E,I0ï¿½ï¿½ï¿½Ü‚ï¿½)
	 (main-count 0)
	 (step-alist '())
	 )

    (include "init_ba10e.ss")

    (set! step-alist (zip states tr-align))

    ;(for-each displayln step-alist)

    ;;
    ;;Mn ï¿½ï¿½ï¿½ï¿½Mn+1ï¿½Ö‚Ì‘Jï¿½Ú‚ï¿½ï¿½oï¿½ï¿½
    ;;
    (define (make-profile-B step m0bin i0bin m1bin)
      (display (format "m0bin=~a " m0bin))
      (display (format "i0bin=~a " i0bin))
      (display (format "m1bin=~a~%" m1bin))
      (let ((m0s (format "M~a" step))
	    (m1s (format "M~a" (+ step 1)))
	    (d0s (format "D~a" step))
	    (d1s (format "D~a" (+ step 1)))
	    (i0s (format "I~a" step))
           )
	(if (not i0bin) ;;I‚ª‚È‚¢step
	    (let ((trans-MM (transpose (list m0bin m1bin))))
	      ;;(displayln (format "\nM-M~a" trans-MM))
	      (set-trans m0s m1s (div* (count-trans-pair '(1 1) trans-MM)
				       (count-trans-pair '(1 2) trans-MM)))
	      (set-trans m0s d1s (div* (count-trans-pair '(1 0) trans-MM)
				       (count-trans-pair '(1 2) trans-MM)))
	      (set-trans d0s m1s (div* (count-trans-pair '(0 1) trans-MM)
				       (count-trans-pair '(0 2) trans-MM)))
	      (set-trans d0s d1s (div* (count-trans-pair '(0 0) trans-MM)
				       (count-trans-pair '(0 2) trans-MM)))
	      )
	    (begin ;;I‚ª‘¶İ‚·‚éstep
	      #f
	    )
	)))
	  
	  
      
    ;; 2‚Í0,1‚Ç‚¿‚ç‚Æ‚àˆê’v

    
    (define (make-prof-A step m0 i0 m1)
      (let ((m0bin (if (empty? m0)
		       (make-list seqcnt 1)
		       (chatlist->binlist (cadar m0))))
	    (i0bin (if (empty? i0)
		       #f
		       (map (lambda(x)(chatlist->binlist (cadr x))) i0)))
	    (m1bin (if (empty? m1)
		       (make-list seqcnt 1)
		       (chatlist->binlist (cadar m1)))))
	(make-profile-B step m0bin i0bin m1bin)
	;;--*-- TBC
	))
	      
      (define (set-trans  from to prob)
      (when (> prob 0.0)
	    (let ((from* (if (string=? from "M0")
			     "S" from))
		  (to* (if (string=? to (format "M~a" (+ 1 main-count)))
			   "E" to)))
	      (when *DEBUG*
		    (displayln (format "\x1b[34mtr ~a=>~a [~a]\x1b[30m" from* to* (round3 prob))))
	      (hash-set! *transition* `(,from ,to) prob))))
    
    (define (between-M-M step)
      (make-prof-A
       step
       (filter (lambda(x)(string=? (format "M~a" step) (car x))) step-alist)
       (filter (lambda(x)(string=? (format "I~a" step) (car x)))  step-alist)
       (filter (lambda(x)(string=? (format "M~a" (+ step 1)) (car x))) step-alist)))	      
;; --*-- STUB of make-profile --*--    
    (define (make-profile)
      (for-each (lambda(n)
		  ;;(displayln "--------")
		  (between-M-M n))
		(iota (+ 1 main-count))))

    (make-profile)

    ;#t
    ;;ï¿½Jï¿½Ú•\ï¿½ï¿½ï¿½Pï¿½sï¿½ï¿½ï¿½oï¿½Í‚ï¿½ï¿½ï¿½ï¿½B
    #|
    (call-with-output-file *ba10e_out*
      (lambda(out)
	(disp-head all-state out)
	(out-trans-line out)
	(displayln "--------" out)
	(disp-head sigma out)
	(out-emit-line out)
	)
      #:exists 'truncate/replace)
    |#
 ))
;;--*-- --*--

;;
(define (gap-ratio chars)
  (let ((len (length chars)))
    (* 1.0 (/ (gap-count chars) len))))

(define (decideMI chars dnacnt theta)
      (if (< (gap-ratio chars) theta)
	  "M"   
	  "I"))
;;
;; MDIï¿½estateï¿½É•ï¿½ï¿½Ô‚ï¿½ï¿½ï¿½ï¿½B
;;

(define (number-states states)
  (reverse (number-states0 states 0 '())))

(define (number-states0 states num acc)
  (if (null? states)
      acc
      (let ((st (string-ref (car states) 0)))
	(if (equal? st #\I)
	    (number-states0 (cdr states) num (cons num acc))
	    (number-states0 (cdr states) (+ num 1) (cons (+ num 1) acc))))))


;; GAP=>0 ï¿½ï¿½ï¿½ï¿½ï¿½ÈŠOï¿½ï¿½1ï¿½É•ÏŠï¿½
(define (chatlist->binlist charlist)
 (map (lambda(c)(if (equal? #\- c) 0 1)) charlist))

(define (all-nogap-count charlistlist)
  (apply + (append-map chatlist->binlist charlistlist)))

(define (count1 binlist)
  (apply + binlist))

;; bitï¿½ï¿½ï¿½]
(define (inv-list binlist)
  (map (lambda(x)(- 1 x)) binlist)) 

;(ros_ba10e 9)

(define (round3 n)
  (/ (round (* 1000 n)) 1000.0))

(define (list-ior binlistlist)
  (map (lambda(x)(apply bitwise-ior x))(apply zip binlistlist)))

(define (bin-eq-wild0 n1 n2)
  (or (= n1 2)(= n1 n2)))
    
(define (bin-eq-wild-pair npair1 npair2)
  (and (bin-eq-wild0 (car npair1)(car npair2))
       (bin-eq-wild0 (cadr npair1)(cadr npair2))))

(define (count-trans-pair pair pairlist)
  (count (lambda(x)(bin-eq-wild-pair pair x)) pairlist))

;;--*-- test data --*--
(define xx '((1 1)(1 1)(1 1)(1 0)(1 0)(0 1)(0 1)(0 1)(0 0)))
;; œ”‚ª0‚È‚çŒ‹‰Ê‚àƒ[ƒ‚É‚·‚é
(define (div* x y)
  (if (= y 0)
      0
      (* 1.0 (/ x y))))

