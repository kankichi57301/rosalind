(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/list index-of ))
;;
;;filename:bwt.ss
;;
(require srfi/1)


(define (rot-list lst n)
  (if (= n 0)
      lst
      (rot-list (append (cdr lst)(list (car lst))) (- n 1))))

(define (rot-list-n lst)
  (map (lambda(n)(rot-list lst n))(iota (length lst))))


(define (list-lessp x1 x2)
  ;(displayln (format "arg=~a,~a" x1 x2))
  (if (or (null? x1)(null? x2))
      #f
      (if (= (car x1)(car x2))
	  (list-lessp (cdr x1)(cdr x2))
	  (< (car x1)(car x2)))))
	  

(define (rot-list-sort lst)
  	      (sort
	       (rot-list-n (append lst '(0)))
	       list-lessp))

(define (bwt-list lst)
  (map (lambda(x)(car (take-right x 1)))(rot-list-sort lst)))


(define t1 '(1 2 1 1 2 3))
(define t11 (bwt-list t1))
(define t2 '(1 2 1 2 1 3 2 1 3 2 1 1))
(define t21 (bwt-list t2))
(define t3 '(1 3 3 2 1 1 2 3 3 2 1 1 2 3 3))
(define t31 (bwt-list t3))
;;
;;invert bwt
;;

(define (inv-bwt lst . start)
  (let ((fst-line (sort lst <))
	(st (if (null? start)
		0
		(car start)))
	)
    (reverse
     (inv-bwt0 fst-line lst st 1 '() (length lst)))))
     
    

(define (inv-bwt0 fst last st pos acc cnt)
  (if (= cnt 0)
      acc
      (let* ((p (find-nth-pos last st pos))
	     (p2 (find-pos fst p)))
	;(displayln (format "fst=~a" p2))
	(inv-bwt0 fst last (car p2)(cadr p2)(cons (car p2) acc)(- cnt 1))
      )))

;;�d�������v�f�̂��郊�X�g��N�Ԗځi�O�x�[�X�j�̗v�f�ƁA���̃��X�g���̓���v�f�̉��Ԗځi�P�x�[�X�j����Ԃ�
;;             0 1 V 3 4 5
;; (find-pos '(1 2 1 3 1 2) 2) =>
;; '(1 2)
;; ���̏ꍇ�iV�Ŏ����j�Q�Ԗځi�O�x�[�X�j�̗v�f�͂P�ł��̂P�͍�����Q�ԖڂȂ̂�(1 2)��Ԃ�
(define (find-pos lst pos)
  (let ((ans (list-ref lst pos)))
    (list ans (+ 1 (count (lambda(x)(= x ans))(take lst pos))))))
;;
;;�d�������v�f�̂��郊�X�g��val�ƈ�v����v�f������nth�Ԗڂ̂��index��Ԃ��B
;;
(define (find-nth-pos lst val nth)
  (find-nth-pos0 lst val nth 0))
  
(define (find-nth-pos0 lst val nth offset)
  (let ((pos (index-of lst val)))
    ;(displayln (format "index=~a" pos))
    (if (= nth 1)
	(+ offset pos )
	(find-nth-pos0 (drop lst (+ 1 pos)) val (- nth 1) (+ 1 pos offset)))))

)

