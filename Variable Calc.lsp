; Loc Huynh
; Maxwell Nguyen
; 4/17/2024

(defun isAddable (term1 term2)
  (and (equal (cadr term1) (cadr term2))
       (equal (caddr term1) (caddr term2))))

(defun plus (term1 term2)
  (cons (+ (car term1) (car term2)) (cdr term1)))

(defun minus (term1 term2)
  (cons (- (car term1) (car term2)) (cdr term1)))


(defun mult (term1 term2)
  (let ((coeff1 (car term1))
        (coeff2 (car term2))
        (var1 (cadr term1))
        (var2 (cadr term2))
        (exp1 (caddr term1))
        (exp2 (caddr term2)))
    (cond
      ((and (null var1) (null var2))  
       (list (* coeff1 coeff2) "" 1))  
      ((and (equal var1 var2) (not (null var1)) (not (equal var1 ""))) 
       (list (* coeff1 coeff2) var1 (+ exp1 exp2)))  
      (t 
       (list (* coeff1 coeff2) (if (null var1) var2 var1) (if (null var1) exp2 exp1))))))




(defun div (term1 term2)
  (let ((coeff1 (car term1))
        (coeff2 (car term2))
        (var1 (cadr term1))
        (var2 (cadr term2))
        (exp1 (caddr term1))
        (exp2 (caddr term2)))
    (if (zerop coeff2)
        (error "Division by zero")
        (cond
         ((null var2) 
          (list (/ coeff1 coeff2) var1 exp1))
         ((and (string= var2 "") (= exp2 1)) 
          (list (/ coeff1 coeff2) var1 exp1))
         ((equal var1 var2) 
          (list (/ coeff1 coeff2) var1 (- exp1 exp2)))
         (t 
          (error "Variable mismatch"))))))




(defun parseTerm (term-str)
  (let ((coefficient 0.0)
        (variable "")
        (exponent 1)
        (index 0)
        (len (length term-str))
        (coeff-sign 1.0)
        (decimal-point-seen nil)
        (decimal-place 0.1))
    (when (char= (char term-str 0) #\-)
      (setf coeff-sign -1.0)
      (incf index))
    (when (char= (char term-str 0) #\+)
      (incf index))
    (loop while (< index len) do
      (let ((char (char term-str index)))
        (cond
         ((digit-char-p char)
          (if decimal-point-seen
              (progn
                (setf coefficient (+ coefficient (* decimal-place (- (char-code char) (char-code #\0)))))
                (setf decimal-place (/ decimal-place 10)))
              (setf coefficient (+ (* coefficient 10) (- (char-code char) (char-code #\0))))))
         ((char= char #\.)
          (setf decimal-point-seen t))
         ((alpha-char-p char)
          (setf variable (string char))
          (incf index)
          (when (and (< index len) (char= (char term-str index) #\^))
            (incf index)
            (let ((exp-start index))
              (loop while (and (< index len) (digit-char-p (char term-str index))) do
                (incf index))
              (setf exponent (parse-integer (subseq term-str exp-start index)))))
          (return-from parseTerm (list (* coeff-sign (if (zerop coefficient) 1.0 coefficient)) variable exponent)))
         ((char= char #\^)
          (incf index)
          (let ((exp-start index))
            (loop while (and (< index len) (digit-char-p (char term-str index))) do
              (incf index))
            (setf exponent (parse-integer (subseq term-str exp-start index))))
          (setf index len))))
      (incf index))
    (let ((final-coefficient (* coeff-sign (if (zerop coefficient) 1.0 coefficient))))
      (list final-coefficient variable exponent))))





(defun formatTerm (term)
  (let ((coeff (princ-to-string (car term)))
        (var (cadr term))
        (exp (caddr term)))
    (cond ((and (equal var "") (equal exp 1)) (format nil "~A" coeff))
          ((and (string= coeff "1") (= exp 1)) (format nil "~A" var))
          ((and (string= coeff "1") (> exp 1)) (format nil "~A^~A" var exp))
          ((> exp 1) (format nil "~A~A^~A" coeff var exp))
          ((= exp 1) (format nil "~A~A" coeff var))
          (t (format nil "~A" coeff)))))

(defun parseExpression (expr)
  (let ((terms '())
        (start 0)
        (end 0)
        (n (length expr)))
    (loop while (< end n) do
      (let ((char (char expr end)))
        (cond
         ((or (char= char #\+)
              (char= char #\-))
          (when (> end start)
            (let ((term (parseTerm (subseq expr start end))))
              (push term terms)))
          (push (string char) terms)
          (setq start (1+ end))
          (incf end))
         ((char= char #\*)
          (when (> end start)
            (let ((term1 (parseTerm (subseq expr start end))))
              (incf end)
              (setq start end)
              (loop while (and (< end n) (not (member (char expr end) '(#\+ #\- #\* #\/)))) do
                (incf end))
              (when (> end start)
                (let ((term2 (parseTerm (subseq expr start end))))
                  (let ((result (mult term1 term2)))
                    (push result terms)
                    (setq start end))))))
          (incf end))
         ((char= char #\/)
          (when (> end start)
            (let ((term1 (parseTerm (subseq expr start end))))
              (incf end)
              (setq start end)
              (loop while (and (< end n) (not (member (char expr end) '(#\+ #\- #\* #\/)))) do
                (incf end))
              (when (> end start)
                (let ((term2 (parseTerm (subseq expr start end))))
                  (let ((result (div term1 term2)))
                    (push result terms))
                  (setq start end)))))
          (incf end))
         (t
          (incf end))))
      (when (= end n)
        (when (> end start)
          (let ((term (parseTerm (subseq expr start end))))
            (push term terms)))))
    (reverse terms)))





(defun simplifyExpression (terms)
  (let ((simplified (make-hash-table :test 'equal))
        (current-operator '+))
    (dolist (term terms)
      (if (stringp term)
          (setq current-operator (if (char= (char term 0) #\+) '+ '-))
          (let* ((coeff (car term))
                 (var (cadr term))
                 (exp (caddr term))
                 (key (if (string= var "") "constant" (format nil "~A^~A" var exp))))
            (let ((existing (gethash key simplified)))
              (if existing
                  (setf (gethash key simplified) (list (funcall current-operator (car existing) coeff) var exp))
                  (setf (gethash key simplified) (list (funcall current-operator 0 coeff) var exp)))))))
    (let ((result '()))
      (maphash (lambda (k v) (push v result)) simplified)
      (sort result (lambda (a b) (or (> (caddr a) (caddr b)) (and (= (caddr a) (caddr b)) (string< (cadr a) (cadr b)))))))))




(defun formatSimplified (simplified)
  (let ((sorted-terms (sort simplified 
                            (lambda (a b) 
                              (or (> (caddr a) (caddr b))
                                  (and (= (caddr a) (caddr b))
                                       (string< (cadr a) (cadr b))))))))
    (format nil "~{~A~^ + ~}" (mapcar #'formatTerm sorted-terms))))



(defun evalexpression (simplified)
  (let ((values (make-hash-table :test 'equal))
        (has-variables nil))
    (dolist (term simplified)
      (let ((var (cadr term))
            (exp (caddr term)))
        (when (not (string= var ""))
          (setf has-variables t)
          (unless (gethash var values) 
            (format t "~A: " var)
            (finish-output)
            (let ((value (read)))
              (setf (gethash var values) value))))))

    (let ((result 0.0))
      (dolist (term simplified)
        (let* ((coeff (car term))
               (var (cadr term))
               (exp (caddr term))
               (term-value (* coeff (if (string= var "") 1 (expt (gethash var values) exp)))))
          (incf result term-value)))
      (if has-variables
          (format t "Expression value: ~A~%" result)
          (format t "Constant expression value: ~A~%" result)))))


(defun exeval ()
  (loop
    (finish-output)
    (format t "Enter arithmetic expression: ")
    (finish-output)
    (let ((expr (read-line)))
      (if (equal expr "quit")
          (return (progn (write-line "Good Bye!") (finish-output)))
        (progn
          (let* ((parsed (parseExpression expr))
                 (simplified (simplifyExpression parsed)))
            (format t "Simplification: ~A~%" (formatSimplified simplified))
            (finish-output)
            (format t "Evaluate? ")
            (finish-output)
            (let ((response (read-line)))
              (when (string-equal response "y")
                (evalexpression simplified))
              (write-line "***")
              (finish-output))))))))

(exeval)
