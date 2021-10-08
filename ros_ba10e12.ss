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
	 (sigma (map (lambda(x)(string-ref x 0))  ;;şÁ½oşÁ½Íµ­şÁ½şÁ½
		     (regexp-match* #rx"[A-Za-z]" (third data))))
	 (sigmas (cons #\- sigma))
	 (sym-cnt (length sigma))   ;;şÁ½oşÁ½Íµ­şÁ½şÁ½şÁ½Ì¼ñ¿½şÁ½şÁ½
	 (aligns (map (lambda(y)
			(map (lambda(x)(string-ref x 0))
			     (regexp-match* #rx"[A-Za-z\\-]" y)))
		      (drop data 4)))
	 (seqlen (length (car aligns)))    	 ;;şÁ½AşÁ½şÁ½şÁ½CşÁ½şÁ½şÁ½şÁ½şÁ½gşÁ½şÁ½şÁ½ÌÄñ¿½şÁ½şÁ½şÁ½işÁ½şÁ½şÁ½Ê¡Ë
	 (seqcnt (length aligns))              	 ;;şÁ½AşÁ½şÁ½şÁ½CşÁ½şÁ½şÁ½şÁ½şÁ½gşÁ½ÌËÜşÁ½şÁ½
	 (tr-align (transpose aligns))	         ;;şÁ½]şÁ½uşÁ½şÁ½şÁ½şÁ½şÁ½AşÁ½şÁ½şÁ½CşÁ½şÁ½şÁ½şÁ½şÁ½g
	 (st '())
	 (states '())
	 (all-state '()) ;;şÁ½SşÁ½Ä¤ñ¿½state (S,E,I0şÁ½şÁ½şÁ½Ü¤ñ¿½)
	 (main-count 0)
	 (step-alist '())
	 )

  (include "init_ba10e.ss")

    (set! step-alist (zip states tr-align))

    ;(for-each displayln step-alist)

    ;;
    ;;Mn şÁ½şÁ½şÁ½şÁ½Mn+1şÁ½Ö¤ÎÁ«şÁ½Ú¤ñ¿½şÁ½oşÁ½şÁ½
    ;;
    (define (make-profile-B step m0bin i0bin m1bin)
      ;(display (format "m0bin=~a " m0bin))
      ;(display (format "i0bin=~a " i0bin))
      ;(display (format "m1bin=~a~%" m1bin))
      (let ((m0s (format "M~a" step))
	    (m1s (format "M~a" (+ step 1)))
	    (d0s (format "D~a" step))
	    (d1s (format "D~a" (+ step 1)))
	    (i0s (format "I~a" step))
	    )
	
	(if (not i0bin) ;;I¤¬¤Ê¤¤step
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
	    (begin ;;I¤¬Â¸ºß¤¹¤ëstep
	      (let ((trans-MIM (transpose (list m0bin (list-ior i0bin) m1bin))))
		;(displayln (format "\x1b[42mMIM=~a\x1b[0m" trans-MIM))
		
		(set-trans m0s i0s (div* (count-trans-pair '(1 1 2) trans-MIM)
					 (count-trans-pair '(1 2 2) trans-MIM)))
		(set-trans m0s m1s (div* (count-trans-pair '(1 0 1) trans-MIM)
					 (count-trans-pair '(1 2 2) trans-MIM)))
		(set-trans m0s d1s (div* (count-trans-pair '(1 0 0) trans-MIM)
					 (count-trans-pair '(1 2 2) trans-MIM)))
		(set-trans d0s i0s (div* (count-trans-pair '(0 1 2) trans-MIM)
					 (count-trans-pair '(0 2 2) trans-MIM)))
		(set-trans d0s m1s (div* (count-trans-pair '(0 0 1) trans-MIM)
					 (count-trans-pair '(0 2 2) trans-MIM)))
		(set-trans d0s d1s (div* (count-trans-pair '(0 0 0) trans-MIM)
					 (count-trans-pair '(0 2 2) trans-MIM)))
		(let* ((i-cnt (all-1-count i0bin)) ;; I-n¤Îgap°Ê³°¤ÎÊ¸»ú¿ô
		       (i-line (apply + (list-ior i0bin)))
		       (self-ratio (* 1.0 (/ (- i-cnt i-line) i-line)))
		       (next-ratio (- 1.0 self-ratio))
		      )
		  ;(display (format "i0bin=~a|" i0bin))
		  ;(display (format "\x1b[31mall i-cnt=~a\x1b[0m|" i-cnt))
		  ;(display (format "\x1b[4m\x1b[32mall i-line=~a\x1b[0m" i-line))
		  ;(display (format "\x1b[46mall i-self-ratio=~a\x1b[0m|" (round3 self-ratio)))
		  ;(displayln (format "\x1b[46mall i-next-ratio=~a\x1b[0m" (round3 next-ratio)))
		  (set-trans i0s i0s self-ratio)
		  (set-trans i0s m1s
			     (* next-ratio
				(div* (count-trans-pair '(2 1 1) trans-MIM)
				      (count-trans-pair '(2 1 2) trans-MIM))))
		  (set-trans i0s d1s
			     (* next-ratio
				(div* (count-trans-pair '(2 1 0) trans-MIM)
				      (count-trans-pair '(2 1 2) trans-MIM))))
	       ))))))
	      
    ;; 2¤Ï0,1¤É¤Á¤é¤È¤â°ìÃ×
    
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
	))
	      
      (define (set-trans  from to prob)
      (when (> prob 0.0)
	    (let ((from* (if (string=? from "M0")
			     "S" from))
		  (to* (if (string=? to (format "M~a" (+ 1 main-count)))
			   "E" to)))
	      (when *DEBUG*
		    (displayln (format "\x1b[34mtr ~a=>~a [~a]\x1b[30m" from* to* (round3 prob))))
	      (hash-set! *transition* `(,from* ,to*) prob))))
    
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
    
    (define (find-chars state)
      (let ((it (filter (lambda(x)(string=? state (car x))) step-alist)))
	(if (null? it)
	    #f
	    (map cadr it))))

    (define (set-emit state step charlist)
      (displayln (format "~a~a:~a" state step (map round3 (normalize-num (sym-count-m charlist sigma)))))
      (hash-set! *emit* (format "~a~a" state step)  (normalize-num (sym-count-m charlist sigma))))
      
                                                  
    (define (make-emit)
      (for-each (lambda(i)
		  (let ((m0 (find-chars (format "M~a" i)))
			(i0 (find-chars (format "I~a" i)))
			)
		    (when m0
			  (set-emit "M" i m0))
		    (when i0
			  (set-emit "I" i i0))))
		(iota main-count 1)))
    
    (make-emit)

    ;;şÁ½JşÁ½ÚÉ½şÁ½şÁ½şÁ½PşÁ½sşÁ½şÁ½şÁ½oşÁ½Í¤ñ¿½şÁ½şÁ½şÁ½B
    
    (call-with-output-file *ba10e_out*
      (lambda(out)
	(disp-head all-state out)
	(out-trans-line out)
	(displayln "--------" out)
	(disp-head sigma out)
	(out-emit-line out)
	)
      #:exists 'truncate/replace)
    
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
;; MDIşÁ½estateşÁ½ÉÊñ¿½şÁ½Ô¤ñ¿½şÁ½şÁ½şÁ½B
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


;; GAP=>0 şÁ½şÁ½şÁ½şÁ½şÁ½È³°şÁ½şÁ½1şÁ½ÉÊÑ´ñ¿½
(define (chatlist->binlist charlist)
 (map (lambda(c)(if (equal? #\- c) 0 1)) charlist))

(define (all-nogap-count charlistlist)
  (apply + (append-map chatlist->binlist charlistlist)))
(define (all-1-count binlistlist)
  (apply + (apply append binlistlist)))
  
(define (count1 binlist)
  (apply + binlist))

;; bitşÁ½şÁ½şÁ½]
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
  (andmap (lambda(x y)(bin-eq-wild0 x y))
	  npair1 npair2))

(define (count-trans-pair pair pairlist)
  (count (lambda(x)(bin-eq-wild-pair pair x)) pairlist))

;;--*-- test data --*--
(define xx '((1 1)(1 1)(1 1)(1 0)(1 0)(0 1)(0 1)(0 1)(0 0)))
;; ½ü¿ô¤¬0¤Ê¤é·ë²Ì¤â¥¼¥í¤Ë¤¹¤ë
(define (div* x y)
  (if (= y 0)
      0
      (* 1.0 (/ x y))))

;;(ros_ba10e 11)
