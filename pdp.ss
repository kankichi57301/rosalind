;;#lang racket
;;
;;A fast algorythm for
;;Partial Digest problem
;;(Turnpike Problem)
;;
;;2022/02/10 @kankichi57301
;;
(require srfi/1)
(require srfi/19)
(require "roslibA.ss" )
(require compatibility/defmacro)
(define *time* '())
(define DEBUG0 #f)  ;; max_d n
(define DEBUG1 #f)  ;; dump tri
(define DEBUG2 #f)  ;; show dx
(define DEBUG3 #f)  ;; step2 phase1 nr[i]==nh[i]
(define DEBUG4 #f)
(define DEBUG41 #f)
(define DEBUG5 #f)  ;; imcompati
(define DEBUG6 #f)  ;; delta-solid
(define DEBUG7 #f)  ;; all w g starts
(define DEBUG8 #f)  ;; check imcompati cart-prod
(define DEBUG9 #f)  ;; step2pahse2case2
(define DEBUG11 #f)  ;;
(define DEBUG12 #f)  ;; dump hline
(define DEBUG13 #f)  ;; maxline
(define DEBUG14 #f)  ;; s2p2c2
(define DEBUG15 #f)  ;; delta etc
(define DEBUG16 #f)  ;; step3case1

(define (mycar x)
  (if (null? x)
      '()
      (car x)))

(define (all-diffs nset)
  (sort
   (map (lambda(x)(apply - x))
	(filter (lambda(y)(> (car y)(cadr y)))
		(cartesian-product nset nset)))
   <))
;; abs diff
(define (abs-diff x y)
  (abs (- x y)))

(include "pdpdat.ss")
;;--*--
(define *dx* '())
(define B0 '())       ;; hlines table
(define *B0* '())
(define dashed '())
(define solid '())
(define N 0)
(define n 0)
(define max-val -1)
(define *ans* '())
(define max-d 0)
(define *dx-count* '())

(define (solve-pdp dx)
  (set! *dx* dx)
  (set! *dx-count* (make-hash))
  (set! B0 (sort (delete-duplicates dx) <))
  (set! *B0* (make-hash (map (lambda(x)(cons x #t)) B0)))
  (set! max-d (apply max dx))
  (set! N (length B0))
  (set! n (tri-num->num (length dx)))
  (set! solid `(0 ,(car (take-right B0 1))))
  (set! dashed (exclude solid B0))
  
  (dx-count-init)
;;
  (when DEBUG0
	(displayln (format "max-d=~a" max-d))
	(displayln (format "n=~a" n)))
  (when DEBUG2
	(displayln (format "dx=\x1b[42m~a\x1b[0m" dx)))
  ;;--*-- main loop --*--
  (loop solid dashed #f 0)
  )
;;;
(define (inc-dx-count n)
  (hash-set! *dx-count* n (+ 1 (hash-ref *dx-count* n 0))))

(define (dx-count-init)
  (for-each (lambda(x)(inc-dx-count x)) *dx*))

;;
;;
;;(define (nthB n)
;;  (list-ref B0 (- n 1)))
(define-macro (nthB n)
  `(list-ref B0 (- ,n 1)))
;; multiplicity of n
(define-macro (nr* bi)
  `(hash-ref *dx-count* ,bi 0))
(define-macro (nr i)
  `(hash-ref *dx-count* (nthB ,i) 0))
;; # of white stars on hline[n]
(define (nwh i  solid dashed)
  (count-diff (nthB i) dashed dashed))
;; # of gray stars on hline[n]
(define (ngh i  solid dashed)
  (+ (count-diff (nthB i) dashed solid)
     (count-diff (nthB i) solid dashed)))
;; # of black stars on hline[n]
(define (nbh i  solid dashed)
  (count-diff (nthB i) solid solid))
(define (nbh* bi  solid dashed)
  (count-diff bi solid solid))
;; # of all stars on hline[i]
;;
;; 2022/03/06 optimize
;;
;;
(define (count-diff0-old dif set1 set2)
  (let ((res 0))
    (for ((i set1))
	 (let ((bf #f)
	       (k 0))
	   (for ((j set2))
		;;(display (format "~a,~a|" i j))
		(when (= dif (- i j))
		      (set! res (+ 1 res))
		      (set! bf #t)
		      (set! set2 (drop set2 k))
		      )
		(when (<= i j)
		      (set! bf #t))
		#:break bf
		(set! k (+ 1 k))
		)))
    res))

(define (count-diff0 dif set1 set2)
  (count-diff1 dif set1 set2 0))

(define (count-diff1 dif set1 set2 acc)
  ;(displayln (format "n1=~a n2=~a" (car set1)(car set2)))
  (if (null? set1)
      acc
      (if (null? set2)
	  acc
	  (let ((d2 (- (car set1)(car set2))))
	    (if (= dif d2)
		(count-diff1 dif (cdr set1)(cdr set2)(+ 1 acc))
		(if (< dif d2)
		    (count-diff1 dif set1 (cdr set2) acc)
		    (count-diff1 dif (cdr set1)  set2 acc)))))))
	    

(define (count-diff dif set1 set2)
  (count-diff0 dif
	       (sort set1 <)
	       (sort set2 <)))




;;
;;2022/03/08
;;
(define (list-abs-diff-stars dif set1 set2)
  (let ((res '()))
    (for ((i set1))
	 (for ((j set2))
	      (when (= dif (abs-diff i j))
		    (set! res (cons `(,i ,j) res)))
	      #t
	      ))
    res))


(define (nh i  solid dashed)
  (let ((allv (append solid dashed)))
    (count-diff (nthB i) allv allv)))

(define (nh* Bi  solid dashed)
  (let ((allv (append solid dashed)))
    (count-diff Bi allv allv)))


;; # of corssing dashed blines on hline[i]
(define (cross-dashed i  solid dashed)
  (+ (* 2 (nwh i solid dashed))(ngh i solid dashed)))
;; # of stars on bline[i]
(define (nb i)
  (+(length solid)(length dashed) -1))

(define (dump-hlines solid dashed turn)
  (when DEBUG12
	(displayln (format "\x1b[32m[~a]\x1b[0msolid=~a|dashed=~a" turn solid dashed))))
  

(define (check-if-imcompatible m n)
  (if 
   (not (member (abs-diff m n) B0))
   #t #f))
#|
(define (check-if-imcompatible m n)
  (not (hash-ref *B0* (abs (- m n)) #f)))
|#

;;---*--- dump triangle ---*---    
    (define (dump-triangle solid dashed turn)
      (dump-hlines solid dashed turn)
      (when DEBUG1
	    (for-each (lambda(i)
			(display   (format "B0[~a] = ~a|" i (nthB i)))
			(display   (format "nr(~a)=~a nwh(~a)=~a " i (nr i) i (nwh i solid dashed)))
			(display   (format "ngh(~a)=~a nbh(~a)=~a:nh(~a)=~a "
					   i (ngh i solid dashed) i (nbh i  solid dashed) i (nh i  solid dashed)))
			(display   (format "~a" (if (nh-flag i  solid dashed) "\x1b[31m*\x1b[0m" "-")))
			(displayln (format "[~a]" (cross-dashed i  solid dashed)))
					;(displayln (format "nb(~a)=~a" i (nb i)))
			)
		      (iota (length B0) 1))))

(define (nh-flag i solid dashed)
      (and (< (nbh i solid dashed)(nr i))
	   (= (nh  i solid dashed)(+ 1 (nr i)))))

    (define (get-max-cross-dashed solid dashed)
      (let ((res '())
	    (maxv -1))
	(for-each (lambda(i)
		    (when (nh-flag i solid dashed)
			  ;;(displayln "-- step 1*> --")
			  ;;(dump-triangle solid dashed 8)
			  (let ((c-d (cross-dashed i solid dashed)))
			    ;;(displayln (format "cros dash[~a]=~a" i c-d))
			    (if (> c-d maxv)
				(begin
				  (set! res (list i))
				  (set! maxv c-d)) 
				(when (= c-d maxv)
				      (set! res (cons i res)))))))
		  (iota (length B0) 1))
	(reverse res)))

;; check nbh=nr on all lines
;;--*-- TBC 3/3
#|
(define (step2-phase1 solid dashed delta-solid)
  (let ((ret #f))                         ;; modified data flag

    (for-each (lambda(i)
		(let ((it (nthB i))) ;; --**--
		  (when (and (member it dashed) (= (nh i solid dashed)(nr i)))
			(let* ((w-g-stars (list-abs-diff-stars it dashed (append solid dashed)))
			       (sel-dash (set-intersect (delete-duplicates (apply append w-g-stars)) dashed))     ;; selected dash lines
			      )
			  (when (not (null? sel-dash))
				(for-each (lambda(i)
					    (when DEBUG3
						  (displayln (format "step2[1] dash=>solid =\x1b[43m~a\x1b[0m" i)))
					    (set! dashed (delete i dashed))
					    (set! solid (cons i  solid))
					    (set! ret #t)
					    (set! delta-solid (cons i delta-solid)))
					  sel-dash))
			  ))))
	      (iota (- (length B0) 1) 1))
	
    (values ret solid dashed delta-solid)             ;; ret return status 02/15 add 
                                                      ;; ret delta-solid 02/24 add
    ))
|#
;;
;;--*-- 2022/3/11 optimize
;;

(define (step2-phase1 solid dashed delta-solid)
  (let ((ret #f))                         ;; modified data flag

    (for-each (lambda(it)

		  (when (= (nh* it solid dashed)(nr* it))
			(let* ((w-g-stars (list-abs-diff-stars it dashed (append solid dashed)))
			       (sel-dash (set-intersect (delete-duplicates (apply append w-g-stars)) dashed))     ;; selected dash lines
			      )
			  (when (not (null? sel-dash))
				(for-each (lambda(i)
					    (when DEBUG3
						  (displayln (format "step2[1] dash=>solid =\x1b[43m~a\x1b[0m" i)))
					    (set! dashed (delete i dashed))
					    (set! solid (cons i  solid))
					    (set! ret #t)
					    (set! delta-solid (cons i delta-solid)))
					  sel-dash))
			  )))
	      dashed)
	
    (values ret solid dashed delta-solid)             ;; ret return status 02/15 add 
                                                      ;; ret delta-solid 02/24 add
    )) 
;;--*-- 2/26
(define (step2-phase2-case2 solid dashed)
  (when DEBUG14
	(displayln "\x1b[46ms2p2c2\x1b[0m"))
  (when DEBUG11
	(dump-triangle solid dashed 11))
  (let ((ret (filter (lambda(bi)(= (nr* bi)(nbh* bi solid dashed))) (drop-right B0 1))))  ;; except N (max bline)
    (when (not (null? ret))
	  (when DEBUG9
		(displayln (format "\x1b[42ms2p2c2\x1b[0m=~a" ret)))
	  (let ((solid2 '())
		(dashed2 '()))
	    (for-each (lambda(x)(let-values([(solid2 dashed2)(delete-dashed-lines-bline-starts x solid dashed)])
				  (set! solid solid2)
				  (set! dashed dashed2)))
				  ret))
    )
    (values solid dashed)
  ))
;;;--*--

(define (delete-dashed-lines-bline-starts bi solid dashed)
   (let ((lines (filter (lambda(x)(= bi (abs-diff (car x)(cadr x))))      ;; dashed lines on "gray" star on the bline
			(cartesian-product dashed solid))))
     (when (and DEBUG9 (not (null? lines)))
	   (displayln (format ":lines=\x1b[45m~a\x1b[0m" lines)))
     (set! dashed (exclude (apply append lines) dashed))
     ;;--*-- TBC 
     (values solid dashed)
     ))


;;; --*-- 2/24
;; 2022/02/24 new argument delte-solid added
;;
;; (solve-pdp '(1 1 2 3 4 5))
;;

(define (step2-phase2 solid dashed delta-solid)
  ;;(dump-hlines solid dashed 5)
  (when DEBUG8
	(displayln (format "\x1b[42m~a\x1b[0m" (cartesian-product dashed delta-solid))))
      (let ((imcompati (filter (lambda(x)(check-if-imcompatible (car x)(cadr x)))
			       (cartesian-product dashed delta-solid)))
	    ;;--*-- 2022/02/25 --*-- if nr(N/2)=1 bline[N/2] is imcompatible
	    ;;--*-- TBC more saitekika
	    (imcompati2 (filter (lambda(x)(and (= 1 (nr (+ 1 (index-of B0 (car x)))))(= (* 2 (car x))(cadr x))))
				(cartesian-product dashed delta-solid)))
	    (ret #f))
	    
	(when (not (null? imcompati))
	      (when DEBUG5
		    (displayln (format "imcompati=~a" imcompati)))
	      (set! dashed (exclude (map car imcompati)  dashed))
	      (set! ret #t))
	;;--*-- 2022/02/25 --*--
	(when (not (null? imcompati2))
	      (when DEBUG5
		    (displayln (format "imcompati2=~a" imcompati2)))
	      (set! dashed (exclude (map car imcompati2)  dashed))
	      (set! ret #t)
	      )
	(let-values ([(solid2 dashed2)(step2-phase2-case2 solid dashed)])
	  (values ret solid2 dashed2))
	))
;;--*--  Step 1 --*--

(define (step1 solid dashed)
  (get-max-cross-dashed solid dashed))
;;--*-- white or gray starts x pos on bline[i]
;;    
;;2022/02/24 new argument delte-solid added
;;
(define (step2 solid dashed delta-solid turn)
  (let-values ([(flag1 solid1 dashed1 delta-solid1)(step2-phase1 solid dashed delta-solid)])
	;;(displayln (format "-- step 2.1 -- f=~a" flag1))
	;;(dump-triangle solid1 dashed1 turn)

    (let-values ([(flag2 solid2 dashed2)(step2-phase2 solid1 dashed1 delta-solid1)])
	  ;;(displayln (format "-- srep 2.2 -- f=~a" flag2))
	  ;;(dump-triangle solid2 dashed2 turn)
      (when (or flag1 flag2)              ;;--*-- loop until no change occurs
	    (let-values ([(solid3 dashed3) (step2 solid2 dashed2 '() (+ turn .01))])
	      (set! dashed2 dashed3)
		  (set! solid2  solid3 )))
	  ;;(dump-triangle solid2 dashed2 6)
      (values solid2 dashed2)
      )))
;;
;; 2022/02/27 
;;
(define (step3case1 solid dashed)
  (if (andmap (lambda(i)(>= (nh i solid dashed)(nr i)))(iota N 1))
      #t
      (begin
	(when DEBUG16
	      (display "s3c1:"))
	#f)))

  
(define (step3case2 solid dashed)
  (if (>= (+ (length solid)(length dashed)) n)                           ;; #t if normal end
      #t
      (begin
	(display "s3c2:")
	#f)))
      
;;
;; step3 prediction
;;
(define (step3 solid dashed)
  (if (and (step3case1 solid dashed)
	   (step3case2 solid dashed)
	   ;(step3case3 solid dashed)
	   )
      #t
      #f
  ))
;;
;;2022/02/24 new argument delte-solid added
;;

(define (step23 solid dashed delta-solid turn)
  (when DEBUG6
	(displayln (format "delta-solid=\x1b[43m~a\x1b[0m" delta-solid)))
  (let-values ([(solid1 dashed1)(step2 solid dashed delta-solid 1)])
	;;(dump-triangle solid1 dashed1 7)
    (if (step3 solid1 dashed1)
	(loop solid1 dashed1 #t (+ 1 turn))
	#f)
    ))

(define (all-wg-stars-on-hline n solid dashed)
       (when DEBUG7
	     (dump-hlines solid dashed 13))
       ;;(displayln (format "cp=~a" (cartesian-product dashed (append dashed solid))))
       (append
	(filter (lambda (x)(equal? n (abs-diff (car x)(cadr x))))
		(cartesian-product dashed solid))
	(filter (lambda (x)(and (equal? n (abs-diff (car x)(cadr x)))
				(< (car x)(cadr x))))
		(cartesian-product dashed dashed))
	))
;;
(include "pdp-loop.ss")

;;
;;(solve-pdp dx01)
;;
(define (put-hline n solid dashed)
  (displayln
  (map
   (lambda(star)
     (format (case (car star)
	       [(1) "\x1b[31m~a\x1b[0m"]
	       [(2) "\x1b[33m~a\x1b[0m"]
	       [(3) "\x1b[34m~a\x1b[0m"])
	     (cdr star)))
   (sort
    (append
     (map (lambda(x)(cons 1 x))(filter (lambda(x)(and (= n (abs-diff (car x)(cadr x)))
						      (< (car x)(cadr x))))
				       (cartesian-product solid  solid )))
     ;(map (lambda(x)(cons 2 x))(filter (lambda(x)(= n (abs-diff (car x)(cadr x))))(cartesian-product solid  dashed)))
     (map (lambda(x)(cons 2 x))(filter (lambda(x)(= n (abs-diff (car x)(cadr x))))(cartesian-product dashed solid )))
     (map (lambda(x)(cons 3 x))(filter (lambda(x)(and (= n (abs-diff (car x)(cadr x)))
						       (< (car x)(cadr x))))
						  (cartesian-product dashed dashed)))
    )
    (lambda(x y)(< (+(cadr x)(caddr x))
		   (+(cadr y)(caddr y))))))

  ))



(define (test-pdp1 x)
  (let* ((d (all-diffs x))
	 (b (delete 0 (delete-duplicates d)))
	 (maxd (apply max b))
	 (solid `(0 ,maxd))
	 (dashed (delete maxd b))
	 )
    (set! *dx* d)
    (set! B0 b)

    (displayln (format "d=~a" d))
    (displayln (format "b=~a" b))
    (dump-triangle solid dashed 0)
    ;(set! dashed (delete 5 dashed))
    
    ;(dump-triangle solid dashed 1)
     ))

;; 
;; 
;; 
	
(define (test-pdp2 x solid+ dashed+)
  (let* ((d (all-diffs x))
	 (b (delete 0 (delete-duplicates d)))
	 (maxd (apply max b))
	 (solid `(0 ,maxd))
	 (dashed (delete maxd b))
	 )
    (set! *dx* d)
    (set! B0 b)

    (displayln (format "d=~a" d))
    (displayln (format "b=~a" b))
    ;;(dump-triangle solid dashed 0)
    (dump-triangle solid+ dashed+ 1)
    ;(dump-triangle solid+ dashed+ 1)
    ;(dump-triangle solid dashed 1)
     ))

;;
;;(test-pdp2 '(0 2 7 10 20 22) '(0 7 10 20 22) '(2))
;;
(define (gen-pdp-test-data n)
  (let ((randlist (map add1 (map (lambda(x)(random 5))(iota n)))))
    (cons 0 (reverse (maplist (lambda(x)(apply + x)) randlist)))))
  
(define (test-pdp3 n)
  (let ((lst (gen-pdp-test-data n)))
    (displayln (format "org=~a" lst))
    (set! *time* (current-time))
    (solve-pdp (all-diffs lst))
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
))

(define (test-pdp4 lst)
    (set! *time* (current-time))
    (solve-pdp  lst)
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
)
  
(define (mytest)(test-pdp4 dx14))
(require profile)
;;(profile-thunk mytest)
;;
;;member function by binary search
;;
(define (mymember n nlist)
  ;;(displayln nlist)
  (if (< n (car nlist))
      #f
      (if (> n (car (take-right nlist 1)))
	  #f
	  (if (< (length nlist) 2)
	      (= n (car nlist))
	      (let* ((half (floor (/ (length nlist) 2)))
		     (center (list-ref nlist half)))
		;;(displayln (format "half=~a" half))
		(if (= n center)
		    #t
		    (if (< n center)
			(mymember n (take nlist half))
			(mymember n (drop nlist half)))))))))
		 
		 
	
