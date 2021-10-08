;;
;; align-local-affine.ss
;; 2021/04/17 copied from align-affine.ss
;; local alignment affine gap penelty
;;
(require srfi/1)
(require ffi/unsafe)
(define *dp* '())
(define *tb* '())
(define *max* 0)
(define *max-x* 0)
(define *max-y* 0)

(include "roslib.ss")
(include "blosum62.ss")
;; affine gap penalty
(define *a* -11)   ;; opening
(define *b* -1)    ;; extension

(define (dp-set! x y len2 val)
  (ptr-set! *dp* _int32 (+ (* x (+ 1 len2)) y) val))
(define (dp-ref x y len2)
  (ptr-ref *dp*  _int32 (+ (* x (+ 1 len2)) y)))
(define (tb-set! x y len2 val)
  (ptr-set! *tb* _int8  (+ (* x (+ 1 len2)) y) val))
(define (tb-ref x y len2)
  (ptr-ref *tb*  _int8  (+ (* x (+ 1 len2)) y) ))

(define (init-local-affine len1 len2)
  (let ((len1+ (+ 1 len1))
	(len2+ (+ 1 len2)))
    (set! *dp* (malloc _int32 (* len1+ len2+) ))
    (memset *dp* 0 (* len1+ len2+) _int32)
    (set! *tb* (malloc _int8  (* len1+ len2+) ))
    (memset *tb* 0 (* len1+ len2+) _int8)
    )
  (set! *max-x* 0)
  (set! *max-y* 0)
)


(define (fill-local-affine-table str1 str2 len1 len2)
  (for-each (lambda(y)
	      (when (= 0 (modulo y 100))
		    (displayln y))
	      (for-each (lambda(x)
			  (let* ((c   (b62score
					 (string-ref str1 (- x 1))
					 (string-ref str2 (- y 1)))
				  )
				       
				 (gap-penx (if (= 0 (tb-ref  (- x 1) y len2 ))
					       *b* *a*))
				 (gap-peny (if (= 1 (tb-ref  x (- y 1) len2 ))
					       *b* *a*))
				 
				 (v1 (+ gap-penx (dp-ref (- x 1) y  len2)))
				 (v2 (+ gap-peny (dp-ref x  (- y 1) len2)))
				 (v3 (+ c 
					(dp-ref  (- x 1) (- y 1) len2)))
				 (val (max v1 v2 v3 0))
				 (dir (max-index-of (list v1 v2 v3 0) identity))
				 )

			    
			    (dp-set! x y len2 val)
			    (tb-set! x y len2 dir)

			    ;(displayln (list x y val))
			    (when (> val *max*)      ;;ç≈ëÂílÇãLò^Ç∑ÇÈ
				  (begin
				    (set! *max* val)
				    (set! *max-x* x)
				    (set! *max-y* y)))

			  ))

			(iota len1 1)))
  (iota len2 1)))
			  
(define (disp-matrix mat)
  (for-each (lambda(line)(displayln (format "~a" line)))
	    mat))

(define (myfmt num)
  (string-take-right (format " ~a" num) 2))
(define (dump-dp len1 len2 )
    (for-each (lambda (y)
		(for-each (lambda(x)
			    (display (format "~a " (myfmt (dp-ref x y len2)))))
			  (iota (+ 1 len1)))
		(display "\n"))
	      (iota (+ 1 len2)))
    )

(define (pair-local-align-affine str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	)
    (init-local-affine len1 len2)
    (set! *max* 0)
    (fill-local-affine-table str1 str2 len1 len2)
    ;;(dump-dp len1 len2)
    (define (my-string-ref str n)
      (string-ref str (- n 1)))
    
    (define (fukugen x y acc1 acc2)
      (if (or (= x 0)(= y 0))  ;; --*-- local-align
	  (list acc1 acc2)
	  (case (tb-ref x y len2)
	    [(0) (fukugen (- x 1) y      (cons (my-string-ref str1 x) acc1)(cons #\- acc2))]
	    [(1) (fukugen x (- y 1)      (cons #\- acc1) (cons (my-string-ref str2 y)acc2))]
	    [(2) (fukugen (- x 1)(- y 1) (cons (my-string-ref str1 x) acc1)(cons (my-string-ref str2 y)acc2))]
	    [(3) (list acc1 acc2)]
	    )
	  )
      )
    (displayln (format "mat x y= ~s,~s" *max-x* *max-y* ))
    (cons
     (dp-ref *max-x* *max-y* len2)
     (map (lambda(x)(apply string x))(fukugen *max-x* *max-y* '() '())))
))

#|
(pair-align-affine "AEEC" "AEC")


|#


