;;
;; n-queens: generate all boards first
;;           (hwk #2, part 1)
;; 

;; make a list of boards for the n-queens problem
(defun makeboards (n) 
  (let ((l1  '(nil)))
    (dotimes (x n l1)
	     (setq l1
		   (mapcan #'(lambda (x) 
			       (do ((num n (- num 1)) 
				    (l2 nil (cons (cons num x) l2)))
                             ((= num 0) l2))) l1)))))

;; check if b is a valid board for the n-queens problem
;; return (list b) if yes, nil otherwise
(defun check-board (b n)
  (do ((num 0 (incf num)) (res (list b)))
      ((or (null res) (eq num (- n 1))) res)
      (do ((j (+ num 1) (incf j)) (b1 0) (b2 0))
	  ((or (eq j n) (null res)) res)
	  (setq b1 (nth num b))
	  (setq b2 (nth j b))
	  (if (or (eq b1 b2)
		  (eq (abs (- b1 b2)) (abs (- num j))))
	      (setq res nil)))))
   
;; solve the n-queens problem: generate all boards first
(defun solve-n-queens (n)
  (mapcan #'(lambda (x) (check-board x n)) (makeboards n)))

;; -------------------------- end of part 1 --------------------------------  

;; ----------------- begin: part 2, version 1, list-based -----------

;;
;; n-queens: finds one solution; each time a queen is added it if there are 
;;           conflicts with previously placed queens; uses lists
;;

;; recursive functions that solves the problem by adding new queens;
;; performs backward checking to see if there are conflicts with the 
;; previously placed queens
(defun rec-n-queens(board size num)
  (do ((i 1 (+ i 1)) (result nil) (board1 board) (canadd nil))
      ((or (> i size) result) result)
      (cond ((and (eql i 1) (eql num 0)) (setq board1 '(1)))
	    (t (setq board1 (append board (list i)))))
      (if (do ((j 0 (+ j 1)) (canadd t))
	      ((eql j num) canadd)
	      (if (or (eql (nth j board) i)
		      (eql (- num j) (abs (- (nth j board) i))))
		  (setq canadd nil)))
	  ;; can add - make recursive call
	  (if (eql size (+ num 1)) 
	    (setq result board1)
	    (setq result (rec-n-queens board1 size (+ num 1)))))))

;; just call this function with the board size as its argument
(defun solve-rec-n-queens (n)
  (rec-n-queens nil n 0))

;; ------------------- end: part 2, version 1, list-based  ------------
      
;; ----------------- begin: part 2, version 2, vector-based -----------
;;
;; n-queens: finds one solution; each time a queen is added it if there are 
;;           conflicts with previously placed queens; uses vectors
;;

;; recursive functions that solves the problem by adding new queens;
;; performs backward checking to see if there are conflicts with the 
;; previously placed queens
(defun rec-n-q (size)
  (incf *num*)
  (incf *num-iter*)
  ;;  (cond ((integerp (/ *num-iter* 1000000))
  ;; (format t "% ~A ~A ~A ~%" *board* *num* *num-iter*))
  ;;	(t nil))
  (do ((i 0 (+ i 1)))
      ((or (eql i size) (eql *num* size)) 
       (if (< *num* size) (decf *num*)))
      (setf (svref *board* *num*) i)
      (if (do ((j 0 (+ j 1)) (canadd t))
	      ((eql j *num*) canadd)
	      (if (or (eql (svref *board* j) i) 
		      (eql (- *num* j) (abs (- (svref *board* j) i))))
		  (setq canadd nil)))
	  ;; can add - make recursive call
	  (rec-n-q size))))

;; just call this function with the board size as its argument
(defun solve-rec-n-q (n)
  (setq *num-iter* 0)
  (setq *board* nil)
  ;; make a vector of fixed length to represent the board
  (setf *board* (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (setq *num* -1)
  (rec-n-q n)
  (format t "~% # of recursive calls = ~A " *num-iter*)
  (if (eql *num* n) 
      *board*
    nil))

;; -------------------- end: part 2, version 2, vector-based -----------


;; --------- BEGIN: n-queens with forward checking -------------------

;; returns t if it is still possible to add more queens
(defun set-values (size row col)
  (do ((i (+ row 1) (+ i 1)) 
       (j (- col 1) (- j 1)) (k (+ col 1) (+ k 1)) (success t))
      ((eql i size) success)
      (if (eql (aref *counts* i col) 0)
	  (decf (svref *free* i))) ;; not free anymore
      (incf (aref *counts* i col)) ;; add number of forward constraints
      ;; diagonals
      (cond ((>= j 0)
	     (if (eql (aref *counts* i j) 0) (decf (svref *free* i)))
	     (incf (aref *counts* i j)))
	    (t nil))
      (cond ((< k size)
	     (if (eql (aref *counts* i k) 0) (decf (svref *free* i)))
	     (incf (aref *counts* i k)))
	    (t nil))
      (if (eql (svref *free* i) 0) (setf success nil))))

;; removes a queen
(defun unset-values (size row col)
  (do ((i (+ row 1) (+ i 1)) (j (- col 1) (- j 1)) (k (+ col 1) (+ k 1)))
      ((eql i size) t)
      (if (eql (aref *counts* i col) 1)
	  (incf (svref *free* i))) ;; free 
      (decf (aref *counts* i col)) ;; remove a forward constraint
      ;; diagonals
      (cond ((>= j 0)
	     (if (eql (aref *counts* i j) 1) (incf (svref *free* i)))
	     (decf (aref *counts* i j)))
	    (t nil))
      (cond ((< k size)
	     (if (eql (aref *counts* i k) 1) (incf (svref *free* i)))
	     (decf (aref *counts* i k)))
	    (t nil))))

;; a recursive function that solves n-queens using forward checking
(defun n-q-fc (size)
  (incf *num*)
  (incf *num-iter*)
  (cond ((integerp (/ *num-iter* 1000000))
	 (format t "~A ~A ~A ~%" *board* *num* *num-iter*))
	(t nil))
  (do ((i 0 (+ i 1)))
      ((or (eql i size) (eql *num* size)) 
       (cond ((< *num* size) (decf *num*) nil) (t t)))
      (cond ((eql (aref *counts* *num* i) 0)
	     (setf (svref *board* *num*) i) ;; set the queen
	     ;; forward checking, set all values, if not ok return
	     ;; set values first
	     (cond ((set-values size *num* i) ;; increment forward counters
		    (if (n-q-fc size) t
		      (unset-values size *num* i)))
		   (t (unset-values size *num* i))))
	    (t nil))))
	

;; call this function to solve the n-queens problems
;; solve the n-queens problem with forward checking
(defun solve-n-q-fc (n)
  (setf *counts* (make-array '(50 50) :initial-element 0))
  (setf *board* (make-array 50 :initial-element 0))
  (setf *free* (make-array 50 :initial-element n))
  (setf *num* -1)
  (setf *num-iter* 0)
  (n-q-fc n)
  (format t "~% # of recursive calls = ~A " *num-iter*)
  (if (eql *num* n) 
      *board*
    nil))










