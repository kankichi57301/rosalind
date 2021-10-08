;; rosalind
;; Implement GraphToGenome 
;; [BA6I] 2021/08/11 AC
;(require srfi/1)
;;(require (inly-insrfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")

(require mzlib/string)  ;; string-replace (not srfi/13)
(define *ba6i_out* "ba6i_out.txt")

(define (ros_ba6i . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba6i.txt"
		    (format "rs_ba6i~a.txt" (car n)))))
	 (colored-edge (read-from-string (format "(~a)" (string-replace (car data) "," ""))))
	 (res1 (solve-ba6i (group-delimit colored-edge (lambda(x)(> (car x)(cadr x))))))
	 (res (map solve-ba6g res1))
	 )
    

    (call-with-output-file *ba6i_out*
      (lambda(out)
	(for-each (lambda(lst)
		    (display "(" out)
		    (display (string-join
			      (map (lambda(n)
				     (if (positive? n)
					 (format "+~a" n)
					 (format "~a"  n)))
				   lst)
			      " ")
			     out)
		    (display ")" out))
		  res))
      #:exists 'truncate/replace)
    #t
    
))


(define (group-delimit lst pred)
  (let ((ans (group-delimit0 lst pred '(()))))
    (if (empty? (car (take-right ans 1)))
	(drop-right ans 1)
	ans)))

(define (group-delimit0 lst pred acc)
  (if (null? lst)
      acc
      (group-delimit0 (cdr lst) pred (if (pred (car lst))
					 (add-last-new-group acc (car lst))
					 (add-last acc (car lst))))))

(define (add-last lstlst item)
  (append (drop-right lstlst 1)
	  (list (append (car (take-right lstlst 1)) (list item)))))

(define (add-last-new-group lstlst item)
  (append (add-last lstlst item) '(())))

;;--*-- test --*--
(define aa '((2 4) (3 6) (5 1)))
(define bb '((7 9) (10 12) (11 8)))

(define (solve-ba6i coloredgeslist)
  (map solve-ba6i-0 coloredgeslist))


(define (solve-ba6i-0 coloredges) 
  (let ((chromo (flatten coloredges)))
    (cons (car (take-right chromo 1)) (drop-right chromo 1))
    ))
	
  ;;
  ;; cycle2cchromo
  ;;
(define (cycle-chromo-0 pair)
  (let ((former (car pair))
	(latter (cadr pair)))
    (if (< former latter)
	(/ latter 2)
	(/ (- former) 2))))

(define (cycle-chromo nlist)
  (map cycle-chromo-0 (group-per nlist 2)))

(define (solve-ba6g-0 pair)
  (let ((former (car pair))
	(latter (cadr pair)))
    (if (< former latter)
	(/ latter 2)
	(/ (- former) 2))))

(define (solve-ba6g nlist)
  (map solve-ba6g-0 (group-per nlist 2)))
