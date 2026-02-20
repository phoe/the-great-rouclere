(uiop:define-package #:the-great-rouclere
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:h #:hunchentoot)
                    (#:s #:split-sequence))
  (:export #:with-magic #:expect #:with #:answer
           #:expectations))

(in-package #:the-great-rouclere)

;; We claim HTTP status code 555 to denote an expectation failure.
(unless (nth-value 1 (gethash 555 h::*http-reason-phrase-map*))
  (setf (gethash 555 h::*http-reason-phrase-map*) "Magic Is Gone"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Acceptor

(defclass magic-acceptor (h:acceptor) ())

(defmethod h:acceptor-log-access ((acceptor magic-acceptor) &key &allow-other-keys))

(defmethod h:acceptor-log-message ((acceptor magic-acceptor) level control &rest args)
  (declare (ignore args))
  (call-next-method))

(defmethod h:acceptor-status-message ((acceptor magic-acceptor) code &key)
  (if (= 2 (truncate code 100))
      "The magic is in the air!"
      "The magic is gone."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core

(defvar *magic*)

(defparameter *expectations* (make-hash-table))

(defvar *expectations-lock* (bt:make-lock "The Great Rouclere expectations lock"))

(defun expectations ()
  (bt:with-lock-held (*expectations-lock*)
    (gethash (h:acceptor-port *magic*) *expectations*)))

(defun (setf expectations) (newval)
  (bt:with-lock-held (*expectations-lock*)
    (setf (gethash (h:acceptor-port *magic*) *expectations*) newval)))

(defun delete-expectations ()
  (bt:with-lock-held (*expectations-lock*)
    (remhash (h:acceptor-port *magic*) *expectations*)))

(defun call-with-magic (thunk on-failure)
  (let ((*magic* (make-instance 'magic-acceptor :port 0)))
    (h:start *magic*)
    (unwind-protect
         (let ((port (h:acceptor-port *magic*)))
           (funcall thunk port)
           (when (expectations)
             (when on-failure
               (funcall on-failure (expectations)))
             (format *debug-io* "~&;; The Great Rouclere still has has ~D unmet expectations!"
                     (length (expectations)))
             (loop for list in (expectations)
                   for i from 1
                   do (format *debug-io* "~&~3D: ~S~%" i list))))
      (delete-expectations)
      (h:stop *magic*))))

(defmacro with-magic ((port-var &key on-failure) &body body)
  (a:with-gensyms (thunk)
    `(flet ((,thunk (,port-var)
              (declare (ignorable ,port-var))
              ,@body))
       (call-with-magic #',thunk ,on-failure))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expectation building

(defmacro expect ((method url &key (times 1)) &body body)
  ;; TODO error if already in an expectation
  `(progn
     ;; TODO: use a proper queue instead of a O(n²) nconcf.
     ;; Or maybe build the expectation somewhere else, and only then push it?
     (a:nconcf (expectations) (list (list :method ,method :url ,url :times ,times)))
     ,@body))

(defun base64-decode (string)
  (babel:octets-to-string
   (base64:base64-stream-to-usb8-array string)
   :encoding (babel:make-external-format :utf-8)))

(defun base64-encode (string)
  (base64:usb8-array-to-base64-string
   (babel:string-to-octets string :encoding (babel:make-external-format :utf-8))))

;; TODO move expectation to be the third arg
(defgeneric add-to-expectation (expectation key data)
  (:method (expectation (key (eql :basic-authorization)) data)
    (destructuring-bind (username password) data
      (let ((value (base64-encode (format nil "~A:~A" username password))))
        (push (cons "Authorization" (format nil "Basic ~A" value)) (getf expectation :headers)))
      expectation))
  (:method (expectation (key (eql :header)) data)
    (destructuring-bind (header value) data
      (push (cons header value) (getf expectation :headers))
      expectation))
  (:method (expectation (key (eql :accept)) data)
    (destructuring-bind (value) data
      (push (cons "Accept" value) (getf expectation :headers))
      expectation))
  (:method (expectation (key (eql :body)) data)
    (destructuring-bind (value) data
      (setf (getf expectation :body) value)
      expectation)))

(defgeneric add-to-response (expectation key data)
  (:method (expectation (key (eql :header)) data)
    (destructuring-bind (header value) data
      (push (cons header value) (getf expectation :headers))
      expectation))
  (:method (expectation (key (eql :content-type)) data)
    (destructuring-bind (value) data
      (push (cons "Content-Type" value) (getf expectation :headers))
      expectation))
  (:method (expectation (key (eql :body)) data)
    (destructuring-bind (value) data
      (setf (getf expectation :body) value)
      expectation)))

(defmacro with (key &rest data)
  `(if (getf (a:lastcar (expectations)) :response)
       (setf (getf (a:lastcar (expectations)) :response)
             (add-to-response (getf (a:lastcar (expectations)) :response) ,key (list ,@data)))
       (setf (a:lastcar (expectations))
             (add-to-expectation (a:lastcar (expectations)) ,key (list ,@data)))))

(defmacro answer ((code) &body body)
  `(if (getf (a:lastcar (expectations)) :response)
       (error "The Great Rouclere already expects a response! ~S"
              (getf (a:lastcar (expectations)) :response))
       (progn (a:nconcf (getf (a:lastcar (expectations)) :response) (list :code ,code))
              ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expectation matching and response construction

(defgeneric match (key value request)
  ;; TODO handle duplicates
  (:method :around (key (value function) request)
    (call-next-method key (funcall value request) request))
  (:method ((key (eql :method)) value request)
    (eq value (h:request-method request)))
  (:method ((key (eql :url)) value request)
    (string= value (h:request-uri request)))
  (:method ((key (eql :body)) value request)
    (string= value (babel:octets-to-string (h:raw-post-data :request request)
                                           :encoding (babel:make-external-format :utf-8))))
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

(defgeneric respond (key value request)
  ;; TODO what happens if there is no RESPOND?
  ;; TODO handle duplicates
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
          do (setf body (if (functionp value)
                            (funcall value)
                            value))
        do (respond key value request)
        finally (return body)))

(defmethod h:acceptor-dispatch-request ((acceptor magic-acceptor) request)
  (flet ((fail ()
           (setf (h:return-code*) 555)
           (setf (h:content-type*) "text/plain")
           (h:abort-request-handler
            ;; TODO solve \n
            ;; TODO signal an error in the testing thread, somehow?
            (format nil "The Great Rouclere is surprised by this request!~%~A"
                    (when (h:raw-post-data)
                      (babel:octets-to-string (h:raw-post-data)))))))
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
          finally (fail))))
