(in-package #:the-great-rouclere)

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

(defvar *failure-stream* *debug-io*)

(defun report-magic-failures (failures on-failure report-string &optional (stream *failure-stream*))
  (when (and failures stream)
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

(defvar *default-acceptor-class* 'magic-acceptor)

(defun call-with-magic-show (thunk nports on-letdowns on-surprises context)
  (let ((acceptors (loop repeat nports collect (make-instance *default-acceptor-class*
                                                              :port 0
                                                              :context context)))
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
                                    "The Great Rouclere still has ~D unmet expectations!")))
      (mapc #'delete-expectations ports)
      (mapc #'delete-surprises ports)
      (mapc #'h:stop acceptors))))

(defvar *port*)

(defmacro with-wand-pointed-at ((port-var) &body body)
  `(let ((*port* ,port-var)) ,@body))

(defmacro with-magic-show ((port-var-or-vars &key on-letdowns on-surprises context) &body body)
  (let ((port-vars (a:ensure-list port-var-or-vars)))
    (a:with-gensyms (thunk)
      `(flet ((,thunk (,@port-vars) (declare (ignorable ,@(rest port-vars)))
                (with-wand-pointed-at (,(first port-vars)) ,@body)))
         (call-with-magic-show #',thunk ,(length port-vars)
                               ,on-letdowns ,on-surprises (list ,@context))))))
