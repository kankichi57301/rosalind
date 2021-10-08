;; rosalind
;; Implement ColoredEdges
;; [BA6H] 2021/08/12 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(require mzlib/string)
(define *ba6h_out* "ba6h_out.txt")

(define (ros_ba6h . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba6h.txt"
		    (format "rs_ba6h~a.txt" (car n)))))
	 (data2 (read-from-string (format "(~a)" (car data))))
	 (res   (string-join
		 (map (lambda(lst)(format "(~a, ~a)" (car lst)(cadr lst)))
		      (solve-ba6h data2))
		 ", ")))
    
    (call-with-output-file *ba6h_out*
      (lambda(out)
	(displayln res out))
      #:exists 'truncate/replace)
    
	res
))




;; copied from solve-ba6f 
(define (ch2cycle nlist)
  (append-map (lambda(n)(if (positive? n)
		     `(,(- (* 2 n) 1) ,(* 2 n))
		     `(,(* -2 n) ,(- (* -2 n) 1))))
       nlist))

(define (solve-ba6h-0 nlist)
  (let ((cycle (ch2cycle nlist)))
    (append 
     (group-per (drop-right (cdr cycle) 1) 2)
     `((,(car (take-right cycle 1)) ,(car cycle))))))

(define (solve-ba6h nlistlist)
  (append-map solve-ba6h-0 nlistlist))
