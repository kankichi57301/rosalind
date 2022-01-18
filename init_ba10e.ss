;; filename:init_ba10e.ss

(set! *transition* (make-hash))
(set! *emit* (make-hash))
    
(set! st (map (lambda(x)(decideMI x (length x) theta)) tr-align))
(set! states (map (lambda(x)(format "~a~a" (car x)(cadr x)))
		      (zip st (number-states st))))
					;(displayln  states)
    
(set! *sigma* sigma)
(set! *tr* tr-align)
(set! main-count (count (lambda(x)(or (equal? #\M (string-ref x 0))
					  (equal? #\N (string-ref x 0))))
			    st))
(displayln (format "main count=~a" main-count)) 
;; ���݂����Ȃ��̂��܂ߑS����state�����
(define (all-state0)
  (map (lambda(x)(format "~a~a" (cadr x)(car x)))
       (cartesian-product (iota main-count 1)'("M" "D" "I"))))

(set! all-state `("S" "I0" ,@(all-state0) "E"))

;; st-n �Ƃ���state���������݂��邩
;; st = one of "M" "N" "I"
(define (count-state st n)
  (count
   (lambda(x)(equal? x `(,st ,n)))
   states))

(define (outAlltrans)
  (sort
   (hash-map *transition* list)
   (lambda(x y)
     (< (index-of all-state (caar x))
	    (index-of all-state (caar y)))))
  )
 
(define (trans-1line from)
  (cons from (map (lambda(p)(round3 (hash-ref *transition* (list from p) 0)))
		  all-state)))
    ;;�����sport��
(define (out-trans-line out)
  (for-each
   (lambda(st)(disp-list (trans-1line st) out))
   all-state))
    ;;emit�\���P�s���o�͂���B

(define (emit-1line from)
  (cons from (map roundp3 (hash-ref *emit* from (make-list sym-cnt 0)))))

    ;;�����sport��
(define (out-emit-line out)
  (for-each
   (lambda(st)(disp-list (emit-1line st) out))
   all-state))
;;���X�g���P�s��port�ɏo�͂���B
(define (disp-list list out)
  (for-each (lambda(x)(display (format "~a\t" x) out)) list)
  (display "\n" out))

(define (disp-head headlist out)
  (for-each (lambda(x)(display (format "\t~a" x) out)) headlist)
  (display "\n" out)
  )

(define (emitM n x) ;;
  (displayln (format "emit:~a" x))
  #|
      (displayln (format "M~a=~a"
			 n
			 (normalize-num (sym-count (car x) sigma))))
      (hash-set! *emit*
		(format "M~a" n)
		(normalize-num (sym-count (car x) sigma)))
  |#
)

(define (emitI n x) ;;
      (displayln (format "I~a=~a" n (normalize-num (sym-count-m  x sigma))))
      (hash-set! *emit*
		(format "I~a" n)
		(normalize-num (sym-count-m x sigma))))


