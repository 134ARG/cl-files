;; general variables 
(defparameter +operators+ (vector '+ '- '* '/ '^))
(defparameter *op-predicts* nil)


;; predicts about operator
(defun operatorp (op &key (start 0) end)
  (find op +operators+ :start start :end end))

(defmacro defoperator-p (name &key (start 0) end)
  `(push
    (defun ,name (obj)
      (operatorp obj :start ,start :end ,end))
    *op-predicts*))

(defoperator-p junior-operator-p :start 0 :end 2)
(defoperator-p midddle-operator-p :start 2 :end 4)
(defoperator-p senior-operator-p :start 4 :end 5)


;; supply functions
(defun translated-form-p (expr)
  (operatorp (car expr)))

(defun single-elt-p (expr)
  (eql (cdr expr) nil))

(defun make-sure-list (obj)
  (if (listp obj) obj (list obj)))


;; extra mathematics function
(defun ^ (base power)
  (expt base power))


;; main convert function
(defun loop-per-operator (form fn)
  (labels ((convert-form (form)
             (cond ((or (atom form)
                        (translated-form-p form))
                    form)
                   ((single-elt-p form) (convert-form (car form)))
                   (t (destructuring-bind (fst operator sec &rest rest) form
                        (if (funcall fn operator)
                            (convert-form `((,operator
                                             ,(convert-form fst)
                                             ,(convert-form sec))
                                            ,@rest))
                            `(,(convert-form fst)
                              ,operator
                              ,@(make-sure-list (convert-form `(,sec ,@rest))))))))))
    (convert-form form)))

;; front end
(defmacro eval-expr (form)
  (labels ((iter (form predicts-lst)
             (if (null predicts-lst)
                 form
                 (iter (loop-per-operator form (car predicts-lst)) (cdr predicts-lst)))))
    (iter form *op-predicts*)))
