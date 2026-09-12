(in-package #:the-great-rouclere)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Acceptor

;;; We claim HTTP status code 444 to denote a surprise.
(h::def-http-return-code +http-unexpected-request+ 444 "Unexpected Request")

(defclass magic-acceptor (h:acceptor)
  ((context :reader magic-acceptor-context :initarg :context :initform '())))

(defmethod h:acceptor-log-access ((acceptor magic-acceptor) &key &allow-other-keys))

(defmethod h:acceptor-log-message ((acceptor magic-acceptor) level control &rest args)
  (declare (ignore args))
  (call-next-method))

(defmethod h:acceptor-status-message ((acceptor magic-acceptor) code &key)
  (ecase (truncate code 100)
    (1 "Magic is about to come...")
    (2 "Magic is in the air!")
    (3 "Magic is somewhere else.")
    (4 "Magic needs you to believe in it.")
    (5 "Magic doesn't exist.")))

(defun unmake-request (stream &optional data)
  (format stream "~A ~A ~A~%" (h:request-method*) (h:request-uri*) (h:server-protocol*))
  (loop for (key . value) in (h:headers-in*) do
    (format stream "~:(~A~): ~A~%" key value))
  (terpri stream)
  (when data (format stream "~A~%~%" data)))

(defun magic-acceptor-dispatch-request (acceptor request)
  (let ((port (h:acceptor-port acceptor)))
    (flet ((fail ()
             (push (list request (copy-tree (expectations port))) (surprises port))
             (setf (h:return-code*) +http-unexpected-request+
                   (h:content-type*) "text/plain")
             (h:abort-request-handler
              (with-output-to-string (stream)
                (format stream ";; The Great Rouclere is surprised by this request!~%~%")
                (unmake-request stream (h:raw-post-data :external-format :utf-8))
                (report-magic-failures (expectations port) nil
                                       "The Great Rouclere has had ~D expectations at the time."
                                       stream)))))
      (bt:with-lock-held (*expectations-lock*)
        (loop for expectation in (expectations port)
              for *expectation* = expectation
              for match = (match-expectation request expectation)
              when match
                do (cond ((eq t (getf expectation :times)))
                         ((= 0 (decf (getf expectation :times)))
                          (a:deletef (expectations port) expectation :count 1)))
                   (return (when (consp match)
                             (create-answer request match)))
              finally (fail))))))

(defun magic-acceptor-wrap-request (acceptor request context)
  (destructuring-bind (wrapper . context) context
    (flet ((wrapped-acceptor-function ()
             (if (null context)
                 (magic-acceptor-dispatch-request acceptor request)
                 (magic-acceptor-wrap-request acceptor request context))))
      (funcall wrapper #'wrapped-acceptor-function))))

(defmethod h:acceptor-dispatch-request ((acceptor magic-acceptor) request)
  (a:if-let ((context (magic-acceptor-context acceptor)))
    (magic-acceptor-wrap-request acceptor request context)
    (magic-acceptor-dispatch-request acceptor request)))

