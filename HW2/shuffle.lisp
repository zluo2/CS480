(defun shuffle (l)
	(do ((oldl l) (newl nil) (i 0 (+ i 1)) (elt 0))
	    ((= i (length l)) newl)
	    (setf elt (random (length oldl)))
	    (setf newl (cons (nth elt oldl) newl))
	    (setf oldl (remove (nth elt oldl) oldl))))

(defun d-shuffle (l)
	(do ((newl nil) (i 15 (- i 1)))
	    ((< i 0) newl)
	    (setf newl (cons (nth i l) (cons (nth (+ i 16) l) newl)))))


(defun remove-nn (l)
	(remove-if-not #'numberp l))

(defun add-nums (l)
	(apply #'+ (remove-if-not #'numberp l)))

(defun conv2 (l)
	(let ((num (first l)) (ll (rest l)))
	     (dolist (elt ll)
		(setf num (+ (* num 2) elt)))
	     num))
