(in-package #:the-great-rouclere)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; URL script parsing

(defun check-duplicate-variables (template)
  (loop with hash-table = (make-hash-table :test #'equal)
        for fragment in template
        for variablep = (eql 0 (position #\: fragment))
        do (cond ((null variablep))
                 ((gethash fragment hash-table)
                  (error "The Great Rouclere has found duplicate variable ~S!" fragment))
                 (t
                  (setf (gethash fragment hash-table) t))) ))

(defun var (name &optional (template (getf *expectation* :url)) (script (h:script-name*)))
  (let ((template (s:split-sequence #\/ template))
        (script (s:split-sequence #\/ script))
        (indicator (format nil ":~A" name)))
    (check-duplicate-variables template)
    (a:if-let ((position (position indicator template :test #'equalp)))
      (nth position script)
      (error "The Great Rouclere is not aware of a variable named :~(~A~)!" name))))

(defun url-match (template script)
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
  ;; :METHOD
  (:method ((key (eql :method)) (value function) request)
    (call-next-method key (funcall value) request))
  (:method ((key (eql :method)) value request)
    (eq value (h:request-method*)))
  ;; :URL
  (:method ((key (eql :url)) (value function) request)
    (call-next-method key (funcall value) request))
  (:method ((key (eql :url)) value request)
    (url-match value (h:script-name*)))
  ;; :BODY
  (:method ((key (eql :body)) (value function) request)
    (call-next-method key (funcall value) request))
  (:method ((key (eql :body)) value request)
    (string= value (h:raw-post-data :request request :external-format :utf-8)))
  ;; :HEADERS
  (:method ((key (eql :headers)) value request)
    (loop for (expected-header . maybe-expected-value) in value
          for expected-value = (if (functionp maybe-expected-value)
                                   (funcall maybe-expected-value)
                                   maybe-expected-value)
          always (string= expected-value (h:header-in* expected-header))))
  ;; :PREDICATES
  (:method ((key (eql :predicates)) value request)
    (every #'funcall value))
  ;; :SIDE-EFFECTS
  (:method ((key (eql :side-effects)) value request)
    (mapc #'funcall value)
    t)
  ;; :TIMES
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
  ;; :CODE
  (:method ((key (eql :code)) (value function) request)
    (call-next-method key (funcall value) request))
  (:method ((key (eql :code)) value request)
    (setf (h:return-code*) value))
  ;; :HEADERS
  (:method ((key (eql :headers)) value request)
    (loop for (expected-header . maybe-expected-value) in value
          for expected-value = (if (functionp maybe-expected-value)
                                   (funcall maybe-expected-value)
                                   maybe-expected-value)
          do (setf (h:header-out expected-header) expected-value)))
  ;; :SIDE-EFFECTS
  (:method ((key (eql :side-effects)) value request)
    (mapc #'funcall value))
  ;; :BODY
  (:method ((key (eql :body)) value request)
    ;; Virtual call, handled in CREATE-ANSWER.
    ))

(defun create-answer (request answer)
  (loop with body = nil
        for (key value) on answer by #'cddr
        when (eq key :body)
          do (setf body (if (functionp value)
                            (funcall value)
                            value))
        do (respond key value request)
        finally (return body)))
