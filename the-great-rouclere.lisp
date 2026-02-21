(uiop:define-package #:the-great-rouclere
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:h #:hunchentoot)
                    (#:s #:split-sequence))
  (:export  #:+http-magic-is-gone+
            #:with-magic-show #:with-wand-pointed-at
            #:expect #:answer #:with #:var
            #:expectations #:surprises))

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
(h::def-http-return-code +http-magic-is-gone+ 555 "Magic Is Gone")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Acceptor

(defclass magic-acceptor (h:acceptor) ())

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core

(defparameter *expectations* (make-hash-table))
(defun expectations (port) (gethash port *expectations*))
(defun (setf expectations) (newval port) (setf (gethash port *expectations*) newval))
(defun delete-expectations (port) (remhash port *expectations*))

(defparameter *surprises* (make-hash-table))
(defun surprises (port) (gethash port *surprises*))
(defun (setf surprises) (newval port) (setf (gethash port *surprises*) newval))
(defun delete-surprises (port) (remhash port *surprises*))

(defun report-magic-failures (failures on-failure report-string &optional (stream *debug-io*))
  (when failures
    (when on-failure (funcall on-failure failures))
    (when report-string
      (format stream "~&;; ")
      (format stream report-string (length failures)))
    (loop for list in failures
          for i from 1
          do (format stream "~&~3D: ~S~%" i list))))

(defun collect-failures (acceptors)
  (loop for acceptor in acceptors
        for port = (h:acceptor-port acceptor)
        nconc (loop for surprise in (surprises port)
                    collect (list* port surprise))
          into surprises
        nconc (loop for letdown in (remove t (expectations port)
                                           :key (a:rcurry #'getf :times))
                    collect (list* :port port letdown))
          into letdowns
        finally (return (values surprises letdowns))))

(defun call-with-magic-show (thunk nports on-letdowns on-surprises)
  (let ((acceptors (loop repeat nports collect (make-instance 'magic-acceptor :port 0)))
        ports)
    (unwind-protect
         (progn
           (mapc #'h:start acceptors)
           (setf ports (mapcar #'h:acceptor-port acceptors))
           (apply thunk ports)
           (multiple-value-bind (surprises letdowns) (collect-failures acceptors)
             (report-magic-failures surprises on-surprises
                                    "The Great Rouclere has been surprised ~D times!")
             (report-magic-failures letdowns on-letdowns
                                    "The Great Rouclere still has ~D unmet expectations!"))))
    (mapc #'delete-expectations ports)
    (mapc #'delete-surprises ports)
    (mapc #'h:stop acceptors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core

;; Assures that all EXPECT blocks and all request handlers don't conflict.
(defvar *expectations-lock* (bt:make-lock "The Great Rouclere expectations lock"))

(defvar *port*)

(defmacro with-wand-pointed-at ((port-var) &body body)
  `(let ((*port* ,port-var)) ,@body))

(defmacro with-magic-show ((port-var-or-vars &key on-letdowns on-surprises) &body body)
  (let ((port-vars (a:ensure-list port-var-or-vars)))
    (a:with-gensyms (thunk)
      `(flet ((,thunk (,@port-vars) (declare (ignorable ,@(rest port-vars)))
                (with-wand-pointed-at (,(first port-vars)) ,@body)))
         (call-with-magic-show #',thunk ,(length port-vars) ,on-letdowns ,on-surprises)))))

(defvar *expectation*)

(defmacro expect ((method url &key (times 1)) &body body)
  `(if (boundp '*expectation*)
       (error "The Great Rouclere is already listening to your expectation! ~S"
              *expectation*)
       (let ((*expectation* (list :method ,method :url ,url :times ,times)))
         (bt:with-lock-held (*expectations-lock*)
           (multiple-value-prog1 ,@body
             (a:nconcf (expectations *port*) (list *expectation*)))))))

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
              (setf *expectation* (list* :answer *answer* *expectation*)))))))

(defgeneric add-to-expectation (key data expectation)
  (:method ((key (eql :header)) data expectation)
    (destructuring-bind (header value) data
      (a:when-let ((actual (a:assoc-value (getf expectation :headers) key :test #'equal)))
        (error "The Great Rouclere will already expect header ~S as ~S!"
               key actual))
      (push (cons header value) (getf expectation :headers))
      expectation))
  (:method ((key (eql :basic-authorization)) data expectation)
    (destructuring-bind (username password) data
      (let ((value (base64-encode (format nil "~A:~A" username password))))
        (add-to-expectation :header (list "Authorization" (format nil "Basic ~A" value))
                            expectation))))
  (:method ((key (eql :accept)) data expectation)
    (add-to-expectation :header (cons "Accept" data) expectation))
  (:method ((key (eql :predicate)) data expectation)
    (destructuring-bind (function) data
      (push function (getf expectation :predicates))
      expectation))
  (:method ((key (eql :body)) data expectation)
    (destructuring-bind (value) data
      (a:when-let ((actual (getf expectation :body)))
        (error "The Great Rouclere will already expect body ~S!" actual))
      (setf (getf expectation :body) value)
      expectation)))

(defgeneric add-to-answer (key data expectation)
  (:method ((key (eql :header)) data expectation)
    (destructuring-bind (header value) data
      (a:when-let ((actual (a:assoc-value (getf expectation :headers) key :test #'equal)))
        (error "The Great Rouclere will already respond with header ~S as ~S!"
               key actual))
      (push (cons header value) (getf expectation :headers))
      expectation))
  (:method ((key (eql :content-type)) data expectation)
    (destructuring-bind (value) data
      (add-to-answer :header (list "Content-Type" value) expectation)))
  (:method ((key (eql :side-effects)) data expectation)
    (destructuring-bind (function) data
      (push function (getf expectation :side-effects))
      expectation))
  (:method ((key (eql :body)) data expectation)
    (destructuring-bind (value) data
      (a:when-let ((actual (getf expectation :body)))
        (error "The Great Rouclere will already respond with body ~S!" actual))
      (setf (getf expectation :body) value)
      expectation)))

(defmacro with (key &rest data)
  `(cond ((boundp '*answer*)
          (setf *answer* (add-to-answer ,key (list ,@data) *answer*)))
         ((boundp '*expectation*)
          (setf *expectation* (add-to-expectation ,key (list ,@data) *expectation*)))
         (t
          (error "The Great Rouclere has no context of the WITH!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extracting URL portions

(defun var (name &optional (template (getf *expectation* :url)) (script (h:script-name*)))
  (let ((template (s:split-sequence #\/ template))
        (script (s:split-sequence #\/ script))
        (indicator (format nil ":~A" name)))
    (a:if-let ((position (position indicator template :test #'equalp)))
      (nth position script)
      (error "The Great Rouclere is not aware of a variable named :~(~A~)!" name))))

(defun check-duplicate-variables (template)
  (loop with hash-table = (make-hash-table :test #'equal)
        for fragment in template
        for variablep = (eql 0 (position #\: fragment))
        do (cond ((null variablep))
                 ((gethash fragment hash-table)
                  (error "The Great Rouclere has found duplicate variable ~S!" fragment))
                 (t
                  (setf (gethash fragment hash-table) t))) ))

(defun url-match (&optional (template (getf *expectation* :url)) (script (h:script-name*)))
  (let ((template (s:split-sequence #\/ template))
        (script (s:split-sequence #\/ script)))
    (check-duplicate-variables template)
    (and (= (length template) (length script))
         (loop for template-fragment in template
               for script-fragment in script
               always (or (eql 0 (position #\: template-fragment))
                          (string= template-fragment script-fragment))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expectation matching

(defgeneric match (key value request)
  ;; TODO ignore the request everywhere, use star variants of functions
  (:method :around (key (value function) request)
    (call-next-method key (funcall value request) request))
  (:method ((key (eql :method)) value request)
    (eq value (h:request-method request)))
  (:method ((key (eql :url)) value request)
    (url-match value (h:script-name request)))
  (:method ((key (eql :body)) value request)
    (string= value (h:raw-post-data :request request :external-format :utf-8)))
  (:method ((key (eql :headers)) value request)
    (loop for (expected-header . expected-value) in value
          always (string= expected-value (h:header-in expected-header request))))
  (:method ((key (eql :predicates)) value request)
    (every #'funcall value))
  (:method ((key (eql :times)) value request)
    ;; Virtual match, handled in ACCEPTOR-DISPATCH-REQUEST.
    t)
  (:method ((key (eql :answer)) value request)
    ;; Virtual match, handled in MATCH-EXPECTATION.
    t))

(defun match-expectation (request expectation)
  (loop with answer = nil
        for (key value) on expectation by #'cddr
        when (eq key :answer)
          do (setf answer value)
        always (match key value request)
        finally (return (or answer t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Answer construction

(defgeneric respond (key value request)
  (:method :around (key (value function) request)
    (call-next-method key (funcall value request) request))
  (:method ((key (eql :code)) value request)
    (setf (h:return-code*) value))
  (:method ((key (eql :headers)) value request)
    (loop for (expected-header . expected-value) in value
          do (setf (h:header-out expected-header) expected-value)))
  (:method ((key (eql :side-effects)) value request)
    (mapc #'funcall value))
  (:method ((key (eql :body)) value request)
    ;; Virtual call, handled in CREATE-ANSWER.
    ))

(defun create-answer (request answer)
  (loop with body = nil
        for (key value) on answer by #'cddr
        when (eq key :body)
          do (setf body value)
        do (respond key value request)
        finally (return body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handling HTTP requests

(defun unmake-request (request stream &optional data)
  (format stream "~A ~A ~A~%"
          (h:request-method request) (h:request-uri request) (h:server-protocol request))
  (loop for (key . value) in (h:headers-in request) do
    (format stream "~:(~A~): ~A~%" key value))
  (terpri stream)
  (when data (format stream "~A~%~%" data)))

(defmethod h:acceptor-dispatch-request ((acceptor magic-acceptor) request)
  (let ((port (h:acceptor-port acceptor)))
    (flet ((fail ()
             (push (list request (copy-tree (expectations port))) (surprises port))
             (setf (h:return-code*) 555
                   (h:content-type*) "text/plain")
             (h:abort-request-handler
              (with-output-to-string (stream)
                (format stream ";; The Great Rouclere is surprised by this request!~%~%")
                (unmake-request request stream (h:raw-post-data :external-format :utf-8))
                (report-magic-failures (expectations port) nil
                                       "The Great Rouclere has had ~D expectations at the time."
                                       stream)))))
      (bt:with-lock-held (*expectations-lock*)
        (loop for expectation in (expectations port)
              for *expectation* = expectation
              for match = (match-expectation request expectation)
              when match
                do (cond ((eq t (getf expectation :times)))
                         ((plusp (getf expectation :times))
                          (when (= 0 (decf (getf expectation :times)))
                            (a:deletef (expectations port) expectation :count 1))))
                   (return (when (consp match)
                             (create-answer request match)))
              finally (fail))))))
