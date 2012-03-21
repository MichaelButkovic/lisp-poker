;;;code for ai opponent

;;;code to make the "ai" decide on what to do
(defun ai-decision ()
  (cond
   ;;;if ai has any hand besides high card, raise
   ((not (zerop (eval-hand (append *ai-hand* *board*)))) '0)
   ;;;checks if the values in the ai hand are less than or equal to 4 (possibility of a straight)
   ((<= 4 (abs (- (car (collect-values *ai-hand*)) (cadr (collect-values *ai-hand*))))) '0)
   ;;;randomly pick to call or fold (probably bad ai, but it's pretty much what i do in real life)
   ((<= 4 (random 10)) '1)
   (t '2)))


;;;for when the ai decides to rasie
(defun ai-raise ()
  (cond
   ;;;if previous raise is 0, raise by some amount below the blinds
   ((zerop *previous-raise*)
    (setf ai-raise-amount (random *blinds*))
    (setf *previous-raise* ai-raise-amount)
    (incf *pot* ai-raise-amount)
    (decf *ai-chips* ai-raise-amount))
   ;;;otherwise, raise by some amount below previous raise
   (t
    (setf ai-raise-amount (random *previous-raise*))
    (incf *pot* ai-raise-amount)
    (decf *ai-chips* ai-raise-amount)
    (setf *previous-raise* ai-raise-amount)))
  (format t "~%Your opponent raises by ~:D chips." ai-raise-amount))

;;;when the ai decides to fold
(defun ai-fold ()
  (format t "~%Your opponent folds. You win this hand!~%")
  (incf *user-chips* *pot*)
  (hand-cleanup))

;;;when the ai decides to call
(defun ai-call ()
  (format t "~%Your opponent calls.~%")
  (incf *pot* *previous-raise*)
  (decf *ai-chips* *previous-raise*)
  (setf *previous-raise* 0))


;;;game playing functions (could probably use some refactoring)

(defun ai-action ()
  (setf *ai-hand* (deal *deck* 2))
  (ai-action-sub))

(defun ai-action-sub ()
  (let ((action (ai-decision)))
     (cond
     ((equal action '0)(ai-raise)(user-action))
     ((equal action '1)(ai-fold))
     ((equal action '2)(ai-call))
      (t nil))))

(defun ai-flop ()
  (let ((action (ai-decision)))
    (cond
    ((equal action '0)(ai-raise)(user-flop))
    ((equal action '1)(ai-fold))
    ((equal action '2)(ai-call))
     (t nil))))

(defun ai-turn ()
  (let ((action (ai-decision)))
    (cond
    ((equal action '0)(ai-raise)(user-turn))
    ((equal action '1)(ai-fold))
    ((equal action '2)(ai-call))
     (t nil))))

(defun ai-river ()
  (let ((action (ai-decision)))
    (cond
    ((equal action '0)(ai-raise)(user-river))
    ((equal action '1)(ai-fold))
    ((equal action '2)(ai-call))
     (t nil))))

(defun ai-final-bets ()
  (let ((action (ai-decision)))
    (cond
    ((equal action '0)(ai-raise)(user-final-bets))
    ((equal action '1)(ai-fold))
    ((equal action '2)(ai-call))
     (t nil))))