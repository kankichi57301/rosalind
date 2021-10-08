(module rosalind racket/base
	(provide (all-defined-out))
	(require srfi/1)
	
(define (count1 binlist)
  (apply + binlist))

;; bit”½“]
(define (inv-list binlist)
  (map (lambda(x)(- 1 x)) binlist)) 

;(ros_ba10e 9)

(define (round3 n)
  (/ (round (* 1000 n)) 1000.0))

(define (list-ior binlistlist)
  (map (lambda(x)(apply bitwise-ior x))(apply zip binlistlist)))

;;------
;(ros_ba10e 11)
)
