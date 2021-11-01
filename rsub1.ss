;;(define (reverse-subst arg)
;;  #t)
;;;
;;; '(A A A B B C C C) => '((A 3)(B 2)(C 3))
;;;
(define (runlength lst)
  (reverse
   (runlength0 (cdr lst)`((,(car lst) 1)))))


(define (runlength0 lst acc)
  (if (null? lst)
      acc
      (let ((next (car lst)))
	(if (equal? (caar acc) next)
	    (runlength0 (cdr lst)(cons (list (caar acc)(+ 1 (cadar acc)))(cdr acc)))
	    (runlength0 (cdr lst)(cons (list next 1) acc))))))
;;; reversal substitution ‚µ‚Ä‚¢‚é‹æŠÔ‚ğ•Ô‚·B
;;; (A A A B B A A A) => '((2 4))
;;; (A A A B B A A A B B) => '((2 4)(4 7))
(define (reverse-subst charlist)
  (reverse (reverse-subst0 (runlength charlist) 0 '())))

(define (reverse-subst* runlenlist)
  (reverse-subst0 runlenlist 0 '()))

(define (reverse-subst0 runlenlist pos acc)
  (if (< (length runlenlist) 3)
      acc
      (if (equal? (caar runlenlist)(caaddr runlenlist))
	  (reverse-subst0 (cdr runlenlist)
			  (+ (cadar runlenlist) pos)
			  (cons (list (+ pos (cadar runlenlist))
				      (+ pos (cadar runlenlist)(cadr (cadr runlenlist))))
							 acc))
	  (reverse-subst0 (cdr runlenlist)(+ (cadar runlenlist) pos) acc)
	  )))


