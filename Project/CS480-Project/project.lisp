;; Zhiwen Luo
;; Haitong Yu
;; Zhiwen Wu

;; CS 480 Fall 2017
;; Dr. Duric
;; Project

;; First map from pdf
(defvar *map1* '( (A (B C E)) (B (A E F)) (C (A E F)) (D (F)) (E (A B C F)) (F (B C D E) )))

;; All 50 U.S. States and the District of Columbia.
;;
;; State abbreviations from 
;; http://www.usps.com/ncsc/lookups/abbr_state.txt

(defvar *50-states* '(
                    (AL (GA FL MS TN))             ; AL = Alabama
                    (AK ())                        ; AK = Alaska
                    (AZ (CA NV UT CO NM))          ; AZ = Arizona
                    (AR (TX OK MO TN MS LA))       ; AR = Arkansas
                    (CA (OR NV AZ))                ; CA = California
                    (CO (NM AZ UT WY NE KS OK))    ; CO = Colorado
                    (CT (RI NY MA))                ; CT = Conneticut
                    (DE (MD PA NJ))                ; DE = Delaware
                    (DC (MD VA))                   ; DC = D.C.
                    (FL (GA AL))                   ; FL = Florida
                    (GA (SC NC TN AL FL))          ; GA = Georgia
                    (HI ())                        ; HI = Hawaii
                    (ID (WA OR NV UT WY MT))       ; ID = Idaho
                    (IL (WI IA MO KY IN))          ; IL = Illinois
                    (IN (IL KY OH MI))             ; IN = Indiana
                    (IA (MN SD NE MO IL WI))       ; IA = Iowa
                    (KS (CO OK MO NE))             ; KS = Kansas
                    (KY (MO TN VA WV OH IN IL))    ; KY = Kentucky
                    (LA (TX AR MS))                ; LA = Lousiana
                    (ME (NH))                      ; ME = Maine
                    (MD (DE PA WV DC VA))          ; MD = Maryland
                    (MA (RI CT NY VT NH))          ; MA = Mass
                    (MI (OH IN WI))                ; MI = Michigan
                    (MN (WI IA SD ND))             ; MN = Minnesota
                    (MS (LA AR TN AL))             ; MS = Mississippi
                    (MO (KS NE IA IL KY TN AR OK)) ; MO = Missouri
                    (MT (ID WY SD ND))             ; MT = Montana
                    (NE (WY SD IA MO KS CO))       ; NE = Nebraska
                    (NV (CA OR ID UT AZ))          ; NV = Nevada
                    (NH (ME MA VT))                ; NH = New Hampshire
                    (NJ (NY PA DE))                ; NJ = New Jersey
                    (NM (AZ UT CO OK TX))          ; NM = New Mexico
                    (NY (PA NJ CT MA VT))          ; NY = New York
                    (NC (VA TN GA SC))             ; NC = North Carolina
                    (ND (MT SD MN))                ; ND = North Dakota
                    (OH (PA WV KY IN MI))          ; OH = Ohio
                    (OK (TX NM CO KS MO AR))       ; OK = Oklahoma
                    (OR (WA ID NV CA))             ; OR = Oregon
                    (PA (NY NJ DE MD WV OH))       ; PA = Pennsylvania
                    (RI (CT MA))                   ; RI = Rhode Island
                    (SC (GA NC))                   ; SC = South Carolina
                    (SD (WY MT ND MN IA NE))       ; SD = South Dakota
                    (TN (AR MO KY VA NC GA AL MS)) ; TN = Tennessee
                    (TX (NM OK AR LA))             ; TX = Texas
                    (UT (CO NM AZ NV ID WY))       ; UT = Utah
                    (VT (NY MA NH))                ; VT = Vermont
                    (VA (NC TN KY WV MD DC))       ; VA = Virginia
                    (WA (ID OR))                   ; WA = Washington
                    (WV (KY OH PA MD VA))          ; WV = West Virginia
                    (WI (MN IA  IL MI))            ; WI = Wisconsin
                    (WY (ID MT SD NE CO UT))))     ; WY = Wyoming
                    
;**********************************************************************     
;Zhiwen Luo's part:

;A function to remove an symbol from everywhere in the list
;lst - the list of associations our target nodeSymbol is associated with
;nodeSymbol the nodeSymbol to remove from the nodes
(defun remove-symbol (map lst nodeSymbol)
  (let ((temp))
    (progn
      (loop for element1 in lst do
        (setq temp element1)
        ;find the list with element1 inside
        (loop for element2 in map do
          (if (eq (first element2) temp)        
            ;the second element of element2 is the list with element1 inside
            (loop for element3 in (second element2) do
              (if (eq element3 nodeSymbol)
                (progn
                  (format t "Remove ~a in ~a~%" element3 element2)
                  (setf (second element2) (remove element3 (second element2))))))))))))

;Remove the zero-one elements from the lst
(defun remove_01 (lst)
  (format t "List: ~a~%" lst)
  (setq lst (sort lst #'(lambda(x y)(< (length (second x)) (length (second y))))))
  (loop while (and (not (null lst)) (or (eq 0 (length (second (first lst)))) 
    (eq 1 (length (second (first lst)))))) do
    (remove-symbol lst (second (first lst)) (first (first lst)))
    (setf lst (delete (first lst) lst))
    (setf lst (sort lst #'(lambda(x y)(< (length (second x)) (length (second y))))))
    (format t "List after removing ~a~%" lst)
  )
  lst
)

(let ((explored_nodes '()))
    (defun cycle_loop (map parent)
    ;set children to the list of nodes connected to the parent
    (let ((children (second (find-if #'(lambda (x) (equal (first x) parent)) map))))
      ;add parent to the list of explored nodes
      (setf explored_nodes (cons parent explored_nodes))
      ;the condition that we already explored everything
      (cond ((null map) nil) 
        (t (let ((cycle-found nil))
            ;do cycle_loop to every child until reaching a node that is already explored
            (dolist (ele children cycle-found)
              (if (and (not (equal parent ele)) (find ele explored_nodes))
                (setf cycle-found t)
                (cycle_loop map ele))))))))

 
  ;helper function of cycle_loop
  (defun detect-cycle (map)
    (let ((return_val (cycle_loop map (car (car map)))))
      (setf explored_nodes '())
      return_val))
)

;End of Zhiwen Luo's part
;*****************************************************************
;Haitong Yu's part:

;The function to get the cutset after modified greedy algorithm
(defun modified-ga (maplist) 
  (let ((cutset) (graphi) (largest) (copy ) (temp1) (temp2) (final))
    ;make a copy of the list to prevent changes to the origional list
    (loop for x in maplist do
      (setq temp1 (first x))
      (setq temp2 (copy-list (second x)))
      (setf copy (append copy (list (list temp1 temp2)))))

    (setf copy (remove_01 copy))
    (setq graphi copy)
    (loop while (detect-cycle graphi) do
      ;the node with the largest degree is at the end of the list
      (setq largest (last copy))
      (format t "Removing: ~a~%" largest)       
      (setq cutset (append cutset  largest))
      (format t "New cutset: ~a~%" cutset)
      (setf copy (delete (first (last copy)) copy))
      (remove-symbol copy (second (first largest)) (first (first largest)))
      (setf copy (remove_01 copy))
      (setq graphi copy)
    )

    ;convert cutset to a list of vertices only
    (dolist (ele cutset)
      (setf final (cons (car ele) final))
    )
    final
  )
)

;Use cutset to create the assoc list
(defun generate-cutset-list (cutset maplist)
  (let ((cutset-assoc) (copy))
    (setf copy (copy-tree maplist))
    (dolist (ele cutset)
      (setf cutset-assoc (cons (find ele copy :test #'equal :key #'car) 
        cutset-assoc))
    )
    ;remove edges that are not related to cutset
    (dolist (ele cutset-assoc)
      (dolist (x (cadr ele))
        (if (not (member x cutset :test #'equal))
          (setf (cadr ele) (remove x (cadr ele))))))
    (reverse cutset-assoc)
  )
)

(defun shuffle (lst)
  ;Set random state
  (setf *random-state* (make-random-state t))
  (let ((result '()) (len) (rnum) (ele ()))
    (setq len (length lst))
    (loop for x from 1 to len
      do
        ;Random for the position of number insead of random for number to improve the preformance
        (setq rnum (random (length lst)))
        (setf ele (nth rnum lst))
        (setq result (append (list ele) result))
        ;Shrink the list 
        (setq lst (remove ele lst))
    )
    ;Return the shuffled list
  result)
)


;End of Haitong Yu's part
;*************************************************************************
;Zhiwen Wu's part:

;The function applying greedy algorithmn, ignore states that dont
;have any legal color options
(defun color-greedy (assoc-list colorlist coloring)
  (let ((result) (shuffled))
    (setf result coloring)
    (setf shuffled (shuffle (copy-tree assoc-list)))
    (dolist (ele shuffled)
      ;Boolean values to check if colors are used
      (let ((bred) (bgreen) (bblue) (byellow))
        (dolist (x (cadr ele))
          (let ((judge))
            (setf judge (find x result :test #'equal :key #'car))
            (cond
              ((eql (cdr judge) (nth 0 colorlist)) 
                (setf bred t))
              ((eql (cdr judge) (nth 1 colorlist)) 
                (setf bgreen t))
              ((eql (cdr judge) (nth 2 colorlist)) 
                (setf bblue t))
              ((eql (cdr judge) (nth 3 colorlist)) 
                (setf byellow t))
            )
          )
        )
        ;coloring based on the boolean result
        (if (not (find (car ele) result :test #'equal :key #'car))
          (cond 
            ((not bred)
              (setf result (cons (cons (car ele) (nth 0 colorlist)) result)))
            ((not bgreen)
              (setf result (cons (cons (car ele) (nth 1 colorlist)) result)))
            ((not bblue)
              (setf result (cons (cons (car ele) (nth 2 colorlist)) result)))
            ((not byellow)
              (setf result (cons (cons (car ele) (nth 3 colorlist)) result)))
          )
        )        
      )
    )
    result
  )
)

;Function to solve map-coloring problem for a specific map step by step
(defun map-coloring (maplist colorlist)
  (let ((cutset) (cutset-assoc) (cutset-coloring) (tree-coloring))
    (setf cutset (modified-ga maplist))
    (setf cutset-assoc (generate-cutset-list cutset maplist))    
    (setf cutset-coloring (color-greedy cutset-assoc colorlist nil))
    (setf tree-coloring (color-greedy maplist colorlist cutset-coloring))    
    (do ()
      ((= (length tree-coloring) (length maplist))
        (format t "~%Final result:" ))
      (setf cutset-coloring (color-greedy cutset-assoc colorlist nil))
      (setf tree-coloring (color-greedy maplist colorlist cutset-coloring))
    )
    tree-coloring
  )
)

(defun project-start ()
  ;For the first map, uncomment the following codes to get result
  (format t "~%First map:~%")
  (format t "~a~%" (map-coloring *map1* '(R G B Y)))

  ;For the 50-states problem, uncomment the following codes to get result
  (format t "~%50-states:~%")
  (format t "~a~%" (map-coloring *50-states* '(R G B Y)))
)

;End of Zhiwen Wu'part

;Example use of the program:
(project-start)
