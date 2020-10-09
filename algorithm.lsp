;; ------------------------------
;;     USEFUL FUNCTIONS
;; ------------------------------

(defun is_state_in_list (state list)
	(loop for l in list do
		(when (equal l state)
			(return T))
	)
)

(defun delete-nth (n list)
	(if (zerop n)
		(cdr list)
		(let ((cons (nthcdr (1- n) list)))
			(when cons (setf (cdr cons) (cddr cons))) list)))

;; ------------------------------
;;     ALGORITHM FUNCTIONS
;; ------------------------------

;; RETURN THE FINAL STATE
(defun get_final_state ()
	(list (list 'M 'M 'M 'C 'C 'C) 'L (list ) (list )))


;; RETURN THE INITIAL STATE
(defun get_initial_state ()
	(list (list ) 'R (list ) (list 'M 'M 'M 'C 'C 'C)))


;; RETURN IF IT'S THE CORRECT STATE
(defun is_final_state(state)
	(AND (= (length (remove M (nth 0 state))) 3) (= (length (remove C (nth 0 state))) 3) 
	(eq (nth 1 state) L))
)

(defun operator_move_the_boat (state)
	(let ((copy (copy-tree state)))
		; Se mueve la barca
		(if (eq (nth 1 copy) 'R)
			(setf (nth 1 copy) 'L)
			(setf (nth 1 copy) 'R)
		)
		(list copy)
	)
)

(defun operator_get_off_the_boat (state)
	(let ((temp (list )))
		(loop for i from 0 to (- (length (nth 2 state)) 1) do
			(let ((copy (copy-tree state)) (element (nth i (nth 2 state))))
				; Se quita un sólo personaje de la barca
				(setf (nth 2 copy) (delete-nth i (nth 2 copy)))
				; Se pone a su lado correspondiente
				(if (eq (nth 1 copy) 'R)
					(setf (nth 3 copy) (append (nth 3 copy) (list element)))
					(setf (nth 0 copy) (append (nth 0 copy) (list element)))
				)
				; Se agrega el nuevo estado
				(setq temp (append temp (list copy)))
			)
		)
		temp
	)
)

(defun operator_get_on_the_boat (state)
	(let ((temp (list )))
		(if (< (length (nth 2 state)) 2)
			(let ((zone (if (eq (nth 1 state) 'R) 3 0)))
				(loop for i from 0 to (- (length (nth zone state)) 1) do
					(let ((copy (copy-tree state)) (element (nth i (nth zone state))))
						; Se quita de la zona
						(setf (nth zone copy) (delete-nth i (nth zone copy)))
						; Se agrega a la barca
						(setf (nth 2 copy) (append (nth 2 copy) (list element)))

						; Se agrega el nuevo estado
						(setq temp (append temp (list copy)))
					)
				)
			)
		)
		temp
	)
)

(defun has_ended (state)
	(return)
	(let ((zone (if (eq (nth 1 state) 'R) 3 0)))
		(let ((n_missionaries (+ (length (remove 'C (nth zone state))) 
							(length (remove 'C (nth 2 state))))) 
			 (n_cannibals 	  (+ (length (remove 'M (nth zone state))) 
							(length (remove 'M (nth 2 state))))))
			(if (> n_cannibals n_missionaries)
				T
			)
		)
	)
)


;; GENERATE CHILD STATES 
(defun operator (state)
	(if (not (has_ended state))
		(let ((states_list (list )))

			;; MOVER LA BARCA
			(setq states_list (append states_list (operator_move_the_boat state)))
			
			;; BAJAR DE LA BARCA
			(setq states_list (append states_list (operator_get_off_the_boat state)))

			;; SUBIR DE LA BARCA
			(setq states_list (append states_list (operator_get_on_the_boat state)))

			states_list
		)
	)
)


;; Breadth First Search
(defun BFS (initial final)
	;; VARIABLES
	(setq opened (list initial))
	(setq closed (list))
	(setq result NIL)

	;; MAIN LOOP
	(loop 
		(when (or (eq result T) (= (length opened) 0)) (return result)) ;; RETURN CONDITION

		(setq current (car opened))
		(setq opened (cdr opened))
		
		(if (equal current final)
			(setq result T)
			(progn
				(setq closed (append closed (list current)))
				(setq new_states (operator current))
				(loop for new in new_states do
					(if (and 	(not (is_state_in_list new opened)) 
							(not (is_state_in_list new closed)))
						(setq opened (append opened (list new))))
				)
			)
		)

		(print '(-------------))
		(print '(ACTUAL))
		(print current)
		(print '(ABIERTOS))
		(print opened)
		(print '(CERRADOS))
		(print closed)
	)
)

;; ------------------------------
;;     MAIN FUNCTION
;; ------------------------------

(defun main ()
	(setq initial_state (get_initial_state))
	(setq final_state (get_final_state))  
	(print (BFS initial_state final_state))
	(print (length closed))
)

(main)
