(require srfi/1)
(define prot-char '(#\A #\C #\D #\E #\F #\G #\H #\I #\K #\L
		    #\M #\N #\P #\Q #\R #\S #\T #\V #\W #\Y))

(define blosum62 '(
;;  A  C  D  E  F  G  H  I  K  L  M  N  P  Q  R  S  T  V  W  Y
(A  4  0 -2 -1 -2  0 -2 -1 -1 -1 -1 -2 -1 -1 -1  1  0  0 -3 -2)
(C  0  9 -3 -4 -2 -3 -3 -1 -3 -1 -1 -3 -3 -3 -3 -1 -1 -1 -2 -2)
(D -2 -3  6  2 -3 -1 -1 -3 -1 -4 -3  1 -1  0 -2  0 -1 -3 -4 -3)
(E -1 -4  2  5 -3 -2  0 -3  1 -3 -2  0 -1  2  0  0 -1 -2 -3 -2)
(F -2 -2 -3 -3  6 -3 -1  0 -3  0  0 -3 -4 -3 -3 -2 -2 -1  1  3)
(G  0 -3 -1 -2 -3  6 -2 -4 -2 -4 -3  0 -2 -2 -2  0 -2 -3 -2 -3)
(H -2 -3 -1  0 -1 -2  8 -3 -1 -3 -2  1 -2  0  0 -1 -2 -3 -2  2)
(I -1 -1 -3 -3  0 -4 -3  4 -3  2  1 -3 -3 -3 -3 -2 -1  3 -3 -1)
(K -1 -3 -1  1 -3 -2 -1 -3  5 -2 -1  0 -1  1  2  0 -1 -2 -3 -2)
(L -1 -1 -4 -3  0 -4 -3  2 -2  4  2 -3 -3 -2 -2 -2 -1  1 -2 -1)
(M -1 -1 -3 -2  0 -3 -2  1 -1  2  5 -2 -2  0 -1 -1 -1  1 -1 -1)
(N -2 -3  1  0 -3  0  1 -3  0 -3 -2  6 -2  0  0  1  0 -3 -4 -2)
(P -1 -3 -1 -1 -4 -2 -2 -3 -1 -3 -2 -2  7 -1 -2 -1 -1 -2 -4 -3)
(Q -1 -3  0  2 -3 -2  0 -3  1 -2  0  0 -1  5  1  0 -1 -2 -2 -1)
(R -1 -3 -2  0 -3 -2  0 -3  2 -2 -1  0 -2  1  5 -1 -1 -3 -3 -2)
(S  1 -1  0  0 -2  0 -1 -2  0 -2 -1  1 -1  0 -1  4  1 -2 -3 -2)
(T  0 -1 -1 -1 -2 -2 -2 -1 -1 -1 -1  0 -1 -1 -1  1  5  0 -2 -2)
(V  0 -1 -3 -2 -1 -3 -3  3 -2  1  1 -3 -2 -2 -3 -2  0  4 -3 -1)
(W -3 -2 -4 -3  1 -2 -2 -3 -3 -2 -1 -4 -4 -2 -3 -3 -2 -3 11  2)
(Y -2 -2 -3 -2  3 -3  2 -1 -2 -1 -1 -2 -3 -1 -2 -2 -2 -1  2  7)
))

(define (amino-score char1 char2)
  (b62score char1 char2))

(define (b62score char1 char2)
  (if (or (equal? char1 #\-)(equal? char2 #\-))
      -5
      (list-ref (list-ref blosum62 (index-of  prot-char char1))
		(+ 1 (index-of prot-char char2)))))

(define (b62score-prot str1 str2)
  (apply + (map (lambda(x)(b62score (string-ref str1 x)
				    (string-ref str2 x)))
		(iota (string-length str1)))))

(define *b62-out* "b62.h")

(define (disp-table score-table)
      (call-with-output-file *b62-out*
	(lambda(out)
	  (disp1 score-table out))
	#:exists 'truncate/replace)) 
	  
(define (disp1 score-table out)
  (displayln "char score_table[20][20]={" out)
  (display
   (string-join
    (map
     (lambda(x)
       (format "{~a}"
	       (string-join (map number->string (cdr x)) ",")))
     score-table)
    ",\n")
   out)
  (displayln "};" out)
   )
  
  
;; (disp-table blosum62)
