;; ------------------------------
;;     USEFUL FUNCTIONS
;; ------------------------------

(defun is_state_in_list (state list)
	(loop for l in list do
		(when (equal l state)
			(return T))
	)
)

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
	(equal state (get_final_state))
)


;; GENERATE CHILD STATES 
(defun operator (state)
	(let ((states_list (list )))

		;; MOVER LA BARCA
		(let ((copy (copy-list state)))
			
			(if (eq (nth 1 copy) 'R)
				(setf (nth 1 copy) 'L)
				(setf (nth 1 copy) 'R)
			)
			(setq states_list (append states_list (list copy)))
		)

		;; BAJAR DE LA BARCA
		(loop for i from 0 to (- (length (nth 2 copy)) 1) do
			(let ((copy (copy-list state)))
				(setf (nth i (nth 2 state)))
				(if (eq (nth 1 copy) 'R)
					(setf (nth 3 copy) 'L)
					(setf (nth 0 copy) 'R)
				)
			)
		)


		states_list
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

		(print '(----------))
		(print opened)
		(print closed)

		(setq current (car opened))
		(setq opened (cdr opened))

		(print current)
		
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
	)
)

;; ------------------------------
;;     MAIN FUNCTION
;; ------------------------------

(defun main ()
	(setq initial_state (get_initial_state))
	(setq final_state (get_final_state))  
	;; (print (BFS initial_state final_state))
	(print initial_state)
	(print final_state)
)

(main)
