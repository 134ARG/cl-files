(defun most (fn lst)
  (if (null lst) (values 0 nil))
  (let* ((wins (car lst))
	 (max (funcall fn wins)))
    (dolist (x (cdr lst))                     ;you needn't. just reserve the car and cdr.
      (let ((score (funcall fn x)))
	(if (> score max)
	    (setf wins x
		  max score))))
    (values wins max)))

(defun before (x y lst &key (test #'equal))
  (let ((first (car lst)))
    (and lst
	 (cond ((funcall test x first) lst)      ;you'd better choose to return sth useful.
	       ((funcall test y first) nil)
	       (t (before x y (cdr lst)))))))

(defun after (x y lst &key (test #'equal))       ;you should be aware that you just need to check the returned value by before
  (let (rest (before y x lst :test test))
    (and (not rest) (member x lst :test test))))     ;fuck. too stupid. exchange args' position!


