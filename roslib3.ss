;;
;;monoisotopic mass table 
;;
(require srfi/13)
(include "roslib.ss")
(include "roslib2.ss")

(define monoiso-mass
'(
(A   71.03711 )
(C   103.00919 )
(D   115.02694 )
(E   129.04259 )
(F   147.06841 )
(G   57.02146 )
(H   137.05891 )
(I   113.08406 )
(K   128.09496 )
(L   113.08406 )
(M   131.04049 )
(N   114.04293 )
(P   97.05276 )
(Q   128.05858 )
(R   156.10111 )
(S   87.03203  )
(T   101.04768 )
(V   99.06841  )
(W   186.07931 )
(Y   163.06333 )))

#|
A	Ala	Alanine　アラニン
B	Asx	アスパラギン酸またはアスパラギン (1)
C	Cys	Cysteine　システイン
D	Asp	Aspartic acid　アスパラギン酸
E	Glu	Glutamic acid　グルタミン酸
F	Phe	Phenylalanine　フェニルアラニン
G	Gly	Glycine　グリシン
H	His	Histidine　ヒスチジン
I	Ile	Isoleucine　イソロイシン
K	Lys	Lysine　リシン
L	Leu	Leucine　ロイシン
M	Met	Methionine　メチオニン
N	Asn	Asparagine　アスパラギン (2)
P	Pro	Proline　プロリン
Q	Gln	Glutamine　グルタミン (2)
R	Arg	Arginine　アルギニン
S	Ser	Serine　セリン
T	Thr	Threonine　トレオニン
U	Sec	Selenocysteine　セレノシステイン (3)
V	Val	Valine　バリン
W	Trp	Tryptophan　トリプトファン
X	Xaa	未知またはその他のアミノ酸 (4)
Y	Tyr	Tyrosine　チロシン
Z	Glx	グルタミン酸またはグルタミン (5)
|#

(define (peptide-weight str)
  (round5
   (apply +
	  (map (lambda(x)
		 (cadr (assoc (string->symbol (string x))monoiso-mass)))
	       (string->list str)))))

(define (peptide-weight* str)
  (round5 (peptide-weight str)))

(define (all-suffix str)
  (map  (lambda(x)(string-drop str x))
	(range 0  (string-length str) )))

;;; exclude whole argument itself
(define (all-prefix str)
  (map  (lambda(x)(string-take str x))
	(range 1    (string-length str) )))

(define (all-prefix-suffix str)
   (append (all-prefix str)
	  (all-suffix str)))

(define (all-prefix-suffix-weight str)
  (map peptide-weight (all-prefix-suffix str)))

(define (all-substring str)
  (append-map all-prefix (all-suffix str)))

(define (all-substr-weight str)
  (map peptide-weight (all-substring str)))


(define (weight->amino w)
  (let ((it (find-first (lambda(x)(< (abs (- w (cadr x))) 0.001))
		   monoiso-mass)))
    (if it
	(car it)
	#f)))


(define (my-minus a b)
  (/ (round (* 100000 (abs (- a b)))) 100000))

#|
(define (minkowski-minus s1 s2)
  (map (lambda(x)(apply my-minus x))
       (cartesian-product s1 s2)))
|#

(define (minkowski-minus s1 s2)
  (map (lambda(x)(inexact->exact(* 10000 (apply my-minus x))))
       (cartesian-product s1 s2)))


(define (round5 x)
  (/ (round (* 100000 x)) 100000))

(define 2-letters-mass
  (map 
   (lambda(x)(list (string-append (symbol->string (caar x))
				 (symbol->string (caadr x)))
		  (round5 (+ (cadar x)(cadadr x)))))
   (cartesian-product monoiso-mass monoiso-mass)))
				 

(define (2-letters-weight->amino w)
  (let ((it (find-first (lambda(x)(< (abs (- w (cadr x))) 0.001))
		   2-letters-mass)))
    (if it
	(car it)
	#f)))

#|
(define 3-letters-mass
  (map 
   (lambda(x)(list (apply string-append (map (lambda(y)(symbol->string (car y))) x)) 

		  (round5 (apply + (map cadr x)))))
   (cartesian-product monoiso-mass monoiso-mass monoiso-mass)))
				 

(define (3-letters-weight->amino w)
  (let ((it (find-first (lambda(x)(< (abs (- w (cadr x))) 0.001))
		   3-letters-mass)))
    (if it
	(car it)
	#f)))
|#

(define (peptide-compose-by? str aminolist)
  (if
  (andmap (lambda(x)(member x aminolist))(string->list str))
  #t #f))

(define (reverse-palin? dna)
  (let ((r (string-reverse dna))
	(s (string-map complement dna)))
    (string=? r s)))


(define (complement enki)
  (case enki
    [(#\A) #\T]
    [(#\T) #\A]
    [(#\G) #\C]
    [(#\C) #\G]))

;(define (complement? enki1 enki2)
;  (equal? enki1 (complement enki2)))

(define (rna-complement enki)
  (case enki
    [(#\A) #\U]
    [(#\U) #\A]
    [(#\G) #\C]
    [(#\C) #\G]))

(define (rna-wobble? enki1 enki2)
  (if (member (list enki1 enki2)
	      '((#\A #\U)
		(#\U #\A)
		(#\G #\C)
		(#\C #\G)
		(#\G #\U)
		(#\U #\G)))
      #t #f))


(define (rna-complement? enki1 enki2)
  (equal? enki1 (rna-complement enki2)))

(define codon-table '(
		      ("TTT" "F")("CTT" "L")("ATT" "I")("GTT" "V")
		      ("TTC" "F")("CTC" "L")("ATC" "I")("GTC" "V")
		      ("TTA" "L")("CTA" "L")("ATA" "I")("GTA" "V")
		      ("TTG" "L")("CTG" "L")("ATG" "M")("GTG" "V")
		      ("TCT" "S")("CCT" "P")("ACT" "T")("GCT" "A")
		      ("TCC" "S")("CCC" "P")("ACC" "T")("GCC" "A")
		      ("TCA" "S")("CCA" "P")("ACA" "T")("GCA" "A")
		      ("TCG" "S")("CCG" "P")("ACG" "T")("GCG" "A")
		      ("TAT" "Y")("CAT" "H")("AAT" "N")("GAT" "D")
		      ("TAC" "Y")("CAC" "H")("AAC" "N")("GAC" "D")
		      ("TAA" "Stop")("CAA" "Q")("AAA" "K")("GAA" "E")
		      ("TAG" "Stop")("CAG" "Q")("AAG" "K")("GAG" "E")
		      ("TGT" "C")("CGT" "R")("AGT" "S")("GGT" "G")
		      ("TGC" "C")("CGC" "R")("AGC" "S")("GGC" "G")
		      ("TGA" "Stop")("CGA" "R")("AGA" "R")("GGA" "G")
		      ("TGG" "W")("CGG" "R")("AGG" "R")("GGG" "G")
		      ))

(define start-codon "ATG")
(define stop-codons '("TAG" "TGA" "TAA"))

(define (stop-codon? triple)
  (member triple stop-codons))
