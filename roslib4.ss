;;
;;filename:roslib4.ss
;;
(define (sep-by0 list pred acc)
  (if (null? list)
      acc
      (let ((fst (car list)))
	(if (pred fst)
	    (sep-by0 (cdr list) pred (cons '() acc))
	    (sep-by0 (cdr list) pred (cons (cons fst (car acc)) (cdr acc)))))))

(define (sep-by lst pred)
  (let ((ans (sep-by0 lst pred '(()))))
    (map (lambda(x)(map string-tokenize x))(cdr (reverse (map reverse ans))))
    ))

;;
;;--*-- test data --*--
;;
;;(define  aaaa '("a b" "c d" "e f" "" "1 2 3" "4 5 6" "" "x y z" "p q r"))
;;(sep-by0 aaaa (lambda(x)(not (non-empty-string? x))) '(()))
;;(sep-by  aaaa (lambda(x)(not (non-empty-string? x))))
(define (sep-by-emptystr lst)
  (sep-by lst (lambda(x)(not (non-empty-string? x)))))

(define (strlist2num nstrlst)
  (map (lambda(lst)
	 (map string->number lst))
       nstrlst))

(define (pairstrlist2num nstrlst)
  (map (lambda(pairstr)
	 (map string->number (string-tokenize pairstr)))
       nstrlst))

	     
(define (mymember item lst pred)
  (if (null? lst)
      '()
      (if (pred item (car lst))
	  lst
	  (mymember item (cdr lst) pred))))

(define (abs=? n1 n2)
  (or (= n1 n2)
      (= 0 (+ n1 n2))))
