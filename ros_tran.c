;; Rosalind
;; Transitions and Transversions
;; [TRAN]
(require srfi/13)

(define *ros37* "rosalind_corr.txt")
(define *ros370* "rs37.txt")


(define (read-37)
  (call-with-input-file *ros37*
    (lambda(in)
      (port->lines in))))

(define (read-370)
  (call-with-input-file *ros370*
    (lambda(in)
      (port->lines in))))

;;
(define (edit-list-0 strlist pred acc)
  (if (null? strlist)
      acc
      (if (pred (car strlist))
	  (edit-list-0 (cdr strlist) pred (cons "" acc))
	  (edit-list-0 (cdr strlist) pred (cons (string-append (car acc)(car strlist))
						(cdr acc))))))


(define (edit-list strlist pred)
  (edit-list-0 strlist pred '()))

(define (header? str)
  (string=? ">" (string-take str 1)))
	 
(define (edit-fasta strlist)
  (reverse
   (edit-list strlist header?)))






(define (ros37)
  (let* ((fasta-data (edit-fasta(read-370))))
    fasta-data
   ))
