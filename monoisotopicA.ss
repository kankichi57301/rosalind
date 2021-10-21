(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in "roslibA.ss" m-rc runsum delete-once))
	(require (only-in racket/list remove-duplicates range group-by cartesian-product ))
	(require (only-in racket/set set-intersect))
	(require (only-in racket/function identity))
;;
;;monoisotopic mass table 
;;
(require srfi/13)
;(include "roslib.ss")
(require "roslibB.ss")

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
A	Ala	Alanine�@�A���j��
B	Asx	�A�X�p���M���_�܂��̓A�X�p���M�� (1)
C	Cys	Cysteine�@�V�X�e�C��
D	Asp	Aspartic acid�@�A�X�p���M���_
E	Glu	Glutamic acid�@�O���^�~���_
F	Phe	Phenylalanine�@�t�F�j���A���j��
G	Gly	Glycine�@�O���V��
H	His	Histidine�@�q�X�`�W��
I	Ile	Isoleucine�@�C�\���C�V��
K	Lys	Lysine�@���V��
L	Leu	Leucine�@���C�V��
M	Met	Methionine�@���`�I�j��
N	Asn	Asparagine�@�A�X�p���M�� (2)
P	Pro	Proline�@�v������
Q	Gln	Glutamine�@�O���^�~�� (2)
R	Arg	Arginine�@�A���M�j��
S	Ser	Serine�@�Z����
T	Thr	Threonine�@�g���I�j��
U	Sec	Selenocysteine�@�Z���m�V�X�e�C�� (3)
V	Val	Valine�@�o����
W	Trp	Tryptophan�@�g���v�g�t�@��
X	Xaa	���m�܂��͂��̑��̃A�~�m�_ (4)
Y	Tyr	Tyrosine�@�`���V��
Z	Glx	�O���^�~���_�܂��̓O���^�~�� (5)
|#
(define amino-symbol 
  (map (lambda(x)(string-ref (symbol->string (car x)) 0))
       (sort monoiso-mass (lambda(x y)(< (cadr x)(cadr y))))))

(define monoiso-mass-int
  (map (lambda(x)(cons (string-ref (symbol->string (car x)) 0) (inexact->exact (floor (cadr x)))))
       monoiso-mass))
;; for test only
;; imaginary amino acids X and Z =4,5
;;
(define monoiso-mass-int-dummy (append  monoiso-mass-int '((#\X . 4)(#\Z . 5))))


(define (weight-int->amino w)
  (let ((it (find-first (lambda(x)(= w (cdr x)))
		   monoiso-mass-int)))
    (if it
	(car it)
	#f)))

;; �A�~�m�_��WEIGHT�𐮐��l�Ɋۂ߂����̂���d�����������ꗗ
(define all-amino-weights (sort ( remove-duplicates (map (lambda(x)(inexact->exact (floor (cadr x))))
							 monoiso-mass))
				<))
(define amw all-amino-weights)
(define all-amino-weights-dummy (append '(4 5) all-amino-weights ))  						   

(define (peptide-weight str)
  (round5
   (apply +
	  (map (lambda(x)
		 (cadr (assoc (string->symbol (string x))monoiso-mass)))
	       (string->list str)))))

(define (peptide-weight-int str)
   (apply +
	  (map (lambda(x)
		 (inexact->exact (floor (cadr (assoc (string->symbol (string x))monoiso-mass)))))
	       (string->list str))))

	


(define (peptide-weight* str)
  (round5 (peptide-weight str)))

;;; exclude whole argument itself
(define (all-prefix str)
  (map  (lambda(x)(string-take str x))
	(range 1    (string-length str) )))
(define (all-suffix str)
  (map  (lambda(x)(string-take-right str x))
	(range 1    (string-length str) )))
;;; include whole argument itself
(define (all-prefix* str)
  (map  (lambda(x)(string-take str x))
	(range 1    (+ 1 (string-length str) ))))
(define (all-suffix* str)
  (map  (lambda(x)(string-take-right str x))
	(range 1    (+ 1 (string-length str) ))))

(define (all-prefix-suffix str)
   (append (all-prefix str)
	  (all-suffix str)))

(define (all-prefix-suffix-weight str)
  (map peptide-weight (all-prefix-suffix str)))
;;
;; linear peptide
;;
(define (theorical-weight-int str)
  (cons 0
	(sort
	 (map peptide-weight-int (all-substring str))
	 <   )))


(define (all-substring str)
  (append-map all-prefix* (all-suffix* str)))

(define (linear-score spect1 spect2)
  (let ((intersect (set-intersect spect1 spect2))
	(gr-spect1 (group-by identity spect1))
	(gr-spect2 (group-by identity spect2))
	)
    (apply + (map (lambda(x)(min(get-multiplicity gr-spect1 x)
				(get-multiplicity gr-spect2 x)))
		  intersect))))
;;
;;name:get-multiplicity
;;����
;;grouped group-by�֐��ŕ��ނ�������
;;'((1 1 1)(2 2 2 2)(3 3)(4)...)
;;
				 
(define (get-multiplicity grouped item)
  (length (find-first (lambda(gr)(equal? (car gr) item))
		      grouped
		      )))

;;(define mult-test '((1 1 1)(2 2 2 2)(3 3)(4)(5 5)))

;; 
;; circular-substring
(require srfi/1)
(define (circular-all-substring str)
  (let ((dbl (string-append str str))
	(len (string-length str))
	)
    (cons str
	  (append-map
	   (lambda(i)
	     (map (lambda(x)(substring dbl i (+ i x)))
		  (iota (- len 1) 1)))
	   (iota len)))))

(define (circular-weight-int str)
  (cons 0
	(sort
	 (map peptide-weight-int (circular-all-substring str))
	 <   )))



(define (all-substr-weight str)
  (map peptide-weight (all-substring str)))

(define (all-substr-weight-int str)
  (map peptide-weight-int (all-substring str)))


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
		      ("TAA" "<")("CAA" "Q")("AAA" "K")("GAA" "E")
		      ("TAG" "<")("CAG" "Q")("AAG" "K")("GAG" "E")
		      ("TGT" "C")("CGT" "R")("AGT" "S")("GGT" "G")
		      ("TGC" "C")("CGC" "R")("AGC" "S")("GGC" "G")
		      ("TGA" "<")("CGA" "R")("AGA" "R")("GGA" "G")
		      ("TGG" "W")("CGG" "R")("AGG" "R")("GGG" "G")
		      ))
;;;reverse-complement


;;;�P��A�~�m�_����R�h����Ԃ���������ΑS��
(define (amino->codon amino)
  (map car
       (filter (lambda(pair)(string=? (cadr pair) amino))
	       codon-table)))
(define (amino->codon-rc amino)
  (map (lambda(x)(m-rc (car x)))
       (filter (lambda(pair)(string=? (cadr pair) amino))
	       codon-table)))

;;�P��A�~�m�_���琳�K�\����Ԃ�
(define (amino->rx amino)
  (format "(~a)" (string-join (amino->codon amino) "|")))
(define (amino->rx-rc amino)
  (format "(~a)" (string-join (amino->codon-rc amino) "|")))

;;�����A�~�m�_str���琳�K�\����Ԃ�
(define (aminostr->rx aminostr)
  (apply string-append
	 (map (lambda(c)(amino->rx (string c)))
	      (string->list aminostr))))
;; reverse complement
(define (aminostr->rx-rc aminostr)
  (apply string-append
	 (map (lambda(c)(amino->rx-rc (string c)))
	      (string->list (string-reverse aminostr)))))

(define start-codon "ATG")
(define stop-codons '("TAG" "TGA" "TAA"))

(define (stop-codon? triple)
  (member triple stop-codons))

(define conflict-diamino '((57 57 114) (57 71 128) (57 99 156) (57 129 186) (71 115 186) (87 99 186)))

;;



(define (peptide-score spect amino . flag)
  (let* ((table (if (or (null? flag)(not (car flag)))
		  monoiso-mass-int
		  monoiso-mass-int-dummy))
	 (rs (runsum
	     (map (lambda(c)(cdr (assoc c table))) (string->list amino))))
	 (amino-all-weight (car (take-right rs 1)))
	 )

    (if (< (length spect) amino-all-weight)
	#f
	(apply +
	       (map (lambda(x)(list-ref spect (- x 1))) rs)))
    ))
;;
;;amino��char��list
;;
(define (peptide-score* spect amino . flag)
  (if (null? amino)
      0
      (let* ((table (if (or (null? flag)(not (car flag)))
			monoiso-mass-int
			monoiso-mass-int-dummy))
	     (rs (runsum
		  (map (lambda(c)(cdr (assoc c table))) amino)))
	     (amino-all-weight (car (take-right rs 1)))
	     )

	(if (< (length spect) amino-all-weight)
	    #f
	    (apply +
		   (map (lambda(x)(list-ref spect (- x 1))) rs)))
  )))
;;
;;������char
(define (amino-weight amino . flag)
        (let* ((table (if (or (null? flag)(not (car flag)))
			monoiso-mass-int
			monoiso-mass-int-dummy)))
	  (cdr (assoc amino table))))

;;--*-- make test data --*--
(define (make-ba4e-testdata0 dbl len times)
  (map (lambda(x)(apply + (take (drop dbl x) len)))
       (iota times)))

(define (make-ba4e-testdata nlist)
  (let ((dbl (append nlist nlist))
	(len (length nlist)))
    (cons 0
	  (append
	   (sort
	    (append-map (lambda(x)(make-ba4e-testdata0 dbl x len))
			(iota (- len 1) 1))
	    <)
	   (list (apply + nlist))))
  ))

(define (mass-score nlist1 nlist2)
  (if (null? nlist1)
      0
      (if (member (car nlist1) nlist2)
	  (+ 1 (mass-score (cdr nlist1)(delete-once (car nlist1) nlist2)))
	  (mass-score (cdr nlist1) nlist2))))
;;--*-- end of roslibA.ss --*--  
)

