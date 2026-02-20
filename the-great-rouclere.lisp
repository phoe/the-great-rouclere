(uiop:define-package #:the-great-rouclere
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:h #:hunchentoot)
                    (#:s #:split-sequence))
  (:export #:with-magic #:expect #:with #:answer
           #:expectations))

(in-package #:the-great-rouclere)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities

(defun octets-to-string (octets)
  (babel:octets-to-string octets :encoding (babel:make-external-format :utf-8)))

(defun base64-decode (string)
  (octets-to-string (base64:base64-stream-to-usb8-array string)))

(defun base64-encode (string)
  (base64:usb8-array-to-base64-string
   (babel:string-to-octets string :encoding (babel:make-external-format :utf-8))))

;;; We claim HTTP status code 555 to denote an expectation failure.
(unless (nth-value 1 (gethash 555 h::*http-reason-phrase-map*))
  (setf (gethash 555 h::*http-reason-phrase-map*) "Magic Is Gone"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Acceptor

(defclass magic-acceptor (h:acceptor)
  ((surprises :accessor surprises :initform '())))

(defmethod h:acceptor-log-access ((acceptor magic-acceptor) &key &allow-other-keys))

(defmethod h:acceptor-log-message ((acceptor magic-acceptor) level control &rest args)
  (declare (ignore args))
  (call-next-method))

(defmethod h:acceptor-status-message ((acceptor magic-acceptor) code &key)
  (ecase (truncate code 100)
    (1 "Magic is about to come...")
    (2 "Magic is in the air!")
    (3 "Magic is somewhere else.")
    (4 "Magic needs you to believe in.")
    (5 "Magic doesn't exist.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Queue

;;; Implementing Queues in Lisp, Norvig, Waters, 1991. Figure 4.
;;; TODO actually use them.

#+(or) #+(or) #+(or) #+(or) #+(or) #+(or)
(defun make-queue () (let ((q (list nil))) (cons q q)))
(defun queue-elements (q) (cdar q))

(defun empty-queue-p (q) (null (cdar q)))
(defun queue-front (q) (cadar q))
(defun dequeue (q) (car (setf (car q) (cdar q))))
(defun enqueue (q item) (setf (cdr q) (setf (cddr q) (list item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core

(defvar *magic*)

(defparameter *expectations* (make-hash-table))

(defun expectations ()
  (gethash (h:acceptor-port *magic*) *expectations*))

(defun (setf expectations) (newval)
  (setf (gethash (h:acceptor-port *magic*) *expectations*) newval))

(defun delete-expectations ()
  (remhash (h:acceptor-port *magic*) *expectations*))

(defun report-magic-failure (failures on-failure report-string)
  (when failures
    (when on-failure (funcall on-failure failures))
    (format *debug-io* "~&;; ")
    (format *debug-io* report-string (length failures))
    (loop for list in failures
          for i from 1
          do (format *debug-io* "~&~3D: ~S~%" i list))))

(defun call-with-magic (thunk on-letdown on-surprise)
  (let ((*magic* (make-instance 'magic-acceptor :port 0)))
    (h:start *magic*)
    (unwind-protect
         (let ((port (h:acceptor-port *magic*)))
           (funcall thunk port)
           (report-magic-failure (surprises *magic*) on-surprise
                                 "The Great Rouclere has been surprised ~D times!")
           (report-magic-failure (expectations) on-letdown
                                 "The Great Rouclere still has has ~D unmet expectations!"))
      (delete-expectations)
      (h:stop *magic*))))

(defmacro with-magic ((port-var &key on-letdown on-surprise) &body body)
  (a:with-gensyms (thunk)
    `(flet ((,thunk (,port-var)
              (declare (ignorable ,port-var))
              ,@body))
       (call-with-magic #',thunk ,on-letdown ,on-surprise))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expectation building

;;; Held EITHER around the whole EXPECT block OR around a whole
;;; ACCEPTOR-DISPATCH-REQUEST. The two blocks must execute separately.
(defvar *expectations-lock* (bt:make-lock "The Great Rouclere expectations lock"))

;;; TODO we need to be able to handle multiple expectations for multiple magicks.
(defvar *expectation*)

(defmacro expect ((method url &key (times 1)) &body body)
  `(if (boundp '*expectation*)
       (error "The Great Rouclere is already listening to your expectation! ~S"
              *expectation*)
       (let ((*expectation* (list :method ,method :url ,url :times ,times)))
         (bt:with-lock-held (*expectations-lock*)
           (multiple-value-prog1 ,@body
             ;; TODO: use a proper queue instead of a O(n²) nconcf.
             (a:nconcf (expectations) (list *expectation*)))))))

(defvar *answer*)

(defmacro answer ((code) &body body)
  `(cond ((not (boundp '*expectation*))
          (error "The Great Rouclere cannot answer if there is no expectation!"))
         ((boundp '*answer*)
          (error "The Great Rouclere is already preparing an answer! ~S"
                 *answer*))
         (t
          (let ((*answer* (list :code ,code)))
            (multiple-value-prog1 ,@body
              ;; TODO: use a proper queue instead of a O(n²) nconcf.
              (a:nconcf (getf *expectation* :response) *answer*))))))

(defgeneric add-to-expectation (key data expectation)
  (:method ((key (eql :basic-authorization)) data expectation)
    (destructuring-bind (username password) data
      (a:when-let ((actual (a:assoc-value (getf expectation :headers) "Authorization"
                                          :test #'equal)))
        (error "The Great Rouclere will already expect header ~S as ~S!"
               "Authorization" actual))
      (let ((value (base64-encode (format nil "~A:~A" username password))))
        (push (cons "Authorization" (format nil "Basic ~A" value)) (getf expectation :headers)))
      expectation))
  (:method ((key (eql :header)) data expectation)
    (destructuring-bind (header value) data
      (a:when-let ((actual (a:assoc-value (getf expectation :headers) key :test #'equal)))
        (error "The Great Rouclere will already expect header ~S as ~S!"
               key actual))
      (push (cons header value) (getf expectation :headers))
      expectation))
  (:method ((key (eql :accept)) data expectation)
    (destructuring-bind (value) data
      (a:when-let ((actual (a:assoc-value (getf expectation :headers) "Accept"
                                          :test #'equal)))
        (error "The Great Rouclere will already expect header ~S as ~S!"
               "Accept" actual))
      (push (cons "Accept" value) (getf expectation :headers))
      expectation))
  (:method ((key (eql :body)) data expectation)
    (destructuring-bind (value) data
      (a:when-let ((actual (getf expectation :body)))
        (error "The Great Rouclere will already expect body ~S!" actual))
      (setf (getf expectation :body) value)
      expectation)))

(defgeneric add-to-response (key data expectation)
  (:method ((key (eql :header)) data expectation)
    (destructuring-bind (header value) data
      (a:when-let ((actual (a:assoc-value (getf expectation :headers) key :test #'equal)))
        (error "The Great Rouclere will already respond with header ~S as ~S!"
               key actual))
      (push (cons header value) (getf expectation :headers))
      expectation))
  (:method ((key (eql :content-type)) data expectation)
    (destructuring-bind (value) data
      (a:when-let ((actual (a:assoc-value (getf expectation :headers) "Content-Type"
                                          :test #'equal)))
        (error "The Great Rouclere will already respond with header ~S as ~S!"
               "Content-Type" actual))
      (push (cons "Content-Type" value) (getf expectation :headers))
      expectation))
  (:method ((key (eql :body)) data expectation)
    (destructuring-bind (value) data
      (a:when-let ((actual (getf expectation :body)))
        (error "The Great Rouclere will already respond with body ~S!" actual))
      (setf (getf expectation :body) value)
      expectation)))

(defmacro with (key &rest data)
  `(cond ((boundp '*answer*)
          (setf *answer*
                (add-to-response ,key (list ,@data) *answer*)))
         ((boundp '*expectation*)
          (setf *expectation*
                (add-to-expectation ,key (list ,@data) *expectation*)))
         (t
          (error "The Great Rouclere has no context of the WITH!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expectation matching

(defgeneric match (key value request)
  (:method :around (key (value function) request)
    (call-next-method key (funcall value request) request))
  (:method ((key (eql :method)) value request)
    (eq value (h:request-method request)))
  (:method ((key (eql :url)) value request)
    (string= value (h:request-uri request)))
  (:method ((key (eql :body)) value request)
    (string= value (h:raw-post-data :request request :external-format :utf-8)))
  (:method ((key (eql :headers)) value request)
    (loop for (expected-header . expected-value) in value
          always (string= expected-value (h:header-in expected-header request))))
  (:method ((key (eql :times)) value request)
    ;; Virtual match, handled in ACCEPTOR-DISPATCH-REQUEST.
    t)
  (:method ((key (eql :response)) value request)
    ;; Virtual match, handled in MATCH-EXPECTATION.
    t))

(defun match-expectation (request expectation)
  (loop with response = nil
        for (key value) on expectation by #'cddr
        when (eq key :response)
          do (setf response (if (functionp value)
                                (funcall value request)
                                value))
        always (match key value request)
        finally (return (or response t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Response construction

(defgeneric respond (key value request)
  (:method :around (key (value function) request)
    (call-next-method key (funcall value request) request))
  (:method ((key (eql :code)) value request)
    (setf (h:return-code*) value))
  (:method ((key (eql :headers)) value request)
    (loop for (expected-header . expected-value) in value
          do (setf (h:header-out expected-header) expected-value)))
  (:method ((key (eql :body)) value request)
    ;; Virtual call, handled in CREATE-RESPONSE.
    ))

(defun create-response (request response)
  (loop with body = nil
        for (key value) on response by #'cddr
        when (eq key :body)
          ;; TODO do we even use that? is it even usable?
          do (setf body (if (functionp value)
                            (funcall value)
                            value))
        do (respond key value request)
        finally (return body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handling HTTP requests

(defmethod h:acceptor-dispatch-request ((acceptor magic-acceptor) request)
  (flet ((fail ()
           (setf (h:return-code*) 555)
           (setf (h:content-type*) "text/plain")
           (let ((response "The Great Rouclere is surprised by this request!"))
             (push (list request (copy-tree (expectations))) (surprises acceptor))
             (h:abort-request-handler response))))
    (bt:with-lock-held (*expectations-lock*)
      (loop with *magic* = acceptor
            for expectation in (expectations)
            for match = (match-expectation request expectation)
            when match
              do (cond ((eq t (getf expectation :times)))
                       ((plusp (getf expectation :times))
                        (when (= 0 (decf (getf expectation :times)))
                          (a:deletef (expectations) expectation :count 1))))
                 (return (when (consp match)
                           (create-response request match)))
            finally (fail)))))
