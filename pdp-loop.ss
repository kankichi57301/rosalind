(define (loop solid dashed flag turn)
  ;;(displayln (format "[~a]solid=~a|dashed=~a" turn solid dashed))
  (dump-triangle solid dashed turn)
  (if (= n (length solid))
      (if (equal? (all-diffs solid) (sort *dx* <))
	  (begin
	    (set! *ans* (sort solid <))
	    (displayln (format "\nfind ans=\x1b[42m~a\x1b[0m" *ans*))
	    #t)
	  (begin
	    #f))
      
      (if   flag
	    ;; entry since 2nd turn	  
	    (let* ((max-lines (step1 solid dashed))
		   ;;(max-line1 (mycar max-lines))
		   ;; select C-1 of them = solid 1 dashed
		   )
	      (when DEBUG4
		    (displayln (format "max-lines=~a" max-lines)))
	      (when DEBUG41
		    (displayln (format "max-lines:=\x1b[43m~a\x1b[0m" (map (lambda(x)(nthB x)) max-lines))))
		    
	      (ormap (lambda(max-line1)
		       (let ((wg-stars  (all-wg-stars-on-hline (nthB max-line1) solid dashed)))
			 (when DEBUG13
			       (displayln (format "maxlin=~a stars :~a" (nthB max-line1) wg-stars)))
			     ;; try C ways
			 (ormap (lambda(non-sel-star)
					(let* ((new-solid (delete-duplicates
							   (append (apply append (delete non-sel-star wg-stars)) solid))) 
					       (new-dashed (exclude new-solid (append solid dashed))) ;;--*-- 2021/02/28
					       (delta-solid (exclude solid new-solid))
					       )
					  (when DEBUG15
						(displayln (format
							    "old solid=~a dashed=~a new solid=~a dahsed =~a delta=\x1b[45m~a\x1b[0m"
							    solid dashed new-solid new-dashed delta-solid)))
					  
					  (if (null? (set-intersect non-sel-star delta-solid))
					      (step23 new-solid new-dashed delta-solid (+ 1 turn))
					      (begin (displayln "double stars")
						     #f))
					  
					  ;;(step23 new-solid new-dashed delta-solid (+ 1 turn))
					      ))
				wg-stars)))
		     max-lines)
	      )
	    ;; entry at 1st turn
	    ;;  2022/02/24 new argument delte-solid added (= solid '(0 N)) at 1st turn
	    ;;
	    (step23 solid dashed solid turn))
      ))
