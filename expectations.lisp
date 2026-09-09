(in-package #:the-great-rouclere)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expectations

;; Assures that all EXPECT blocks and all request handlers don't conflict.
;; No need for a separate answer lock, since all answers within an expectation.
(defvar *expectations-lock* (bt:make-lock "The Great Rouclere expectations lock"))

(defvar *expectation*)

(defmacro expect ((method url &key (times 1)) &body body)
  `(if (boundp '*expectation*)
       (error "The Great Rouclere is already listening to your expectation! ~S"
              *expectation*)
       (let ((*expectation* (list :method ,method :url ,url :times ,times)))
         (bt:with-lock-held (*expectations-lock*)
           (multiple-value-prog1 ,@body
             (a:nconcf (expectations *port*) (list *expectation*)))))))

(defun base64-encode (string)
  (base64:usb8-array-to-base64-string
   (babel:string-to-octets string :encoding (babel:make-external-format :utf-8))))

(defgeneric add-to-expectation (key data expectation)
  ;; :HEADER
  (:method ((key (eql :header)) data expectation)
    (destructuring-bind (header value) data
      (a:when-let ((actual (a:assoc-value (getf expectation :headers) header :test #'equal)))
        (error "The Great Rouclere will already expect header ~S as ~S!"
               header actual))
      (push (cons header value) (getf expectation :headers))
      expectation))
  ;; :BASIC-AUTHORIZATION
  (:method ((key (eql :basic-authorization)) data expectation)
    (let ((value (if (functionp (first data))
                     (destructuring-bind (thunk) data
                       (flet ((encode-basic-authorization ()
                                (format nil "Basic ~A"
                                        (base64-encode (apply #'format nil "~A:~A" (funcall thunk))))))
                         #'encode-basic-authorization))
                     (destructuring-bind (username password) data
                       (format nil "Basic ~A" (base64-encode (format nil "~A:~A" username password)))))))
      (add-to-expectation :header (list "Authorization" value) expectation)))
  ;; :ACCEPT
  (:method ((key (eql :accept)) data expectation)
    (add-to-expectation :header (cons "Accept" data) expectation))
  ;; :SIDE-EFFECTS
  (:method ((key (eql :side-effects)) data expectation)
    (destructuring-bind (function) data
      (push function (getf expectation :side-effects))
      expectation))
  ;; :PREDICATE
  (:method ((key (eql :predicate)) data expectation)
    (destructuring-bind (function) data
      (push function (getf expectation :predicates))
      expectation))
  ;; :BODY
  (:method ((key (eql :body)) data expectation)
    (destructuring-bind (value) data
      (a:when-let ((actual (getf expectation :body)))
        (error "The Great Rouclere will already expect body ~S!" actual))
      (setf (getf expectation :body) value)
      expectation)))
