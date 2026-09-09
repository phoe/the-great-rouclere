(in-package #:the-great-rouclere)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WITH macro

(defun check-with-keyword (key)
  (let* ((methods (append (m:generic-function-methods #'add-to-answer)
                          (m:generic-function-methods #'add-to-expectation)))
         (keywords (mapcar (a:compose #'m:eql-specializer-object
                                      #'first
                                      #'m:method-specializers)
                           methods)))
    (unless (member key keywords)
      (let ((*package* (find-package :keyword)))
        (error "The Great Rouclere does not recognize the WITH keyword ~S!~%(Only ~{~S~^, ~}.)"
               key keywords)))))

(defmacro with (key &rest data)
  `(progn
     (check-with-keyword ,key)
     (cond ((boundp '*answer*)
            (setf *answer* (add-to-answer ,key (list ,@data) *answer*)))
           ((boundp '*expectation*)
            (setf *expectation* (add-to-expectation ,key (list ,@data) *expectation*)))
           (t
            (error "The Great Rouclere has no context of the WITH!")))))
