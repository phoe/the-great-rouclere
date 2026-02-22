(uiop:define-package #:the-great-rouclere/tests
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:d #:drakma)
                    (#:h #:hunchentoot)
                    (#:r #:the-great-rouclere))
  (:export #:*debugp* #:magic!))

(in-package #:the-great-rouclere/tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Framework

(5am:def-suite the-great-rouclere)

(5am:in-suite the-great-rouclere)

(defvar *debugp* nil)

(defun magic! (&optional (debugp *debugp*))
  (let ((5am:*on-error* (if debugp :debug nil))
        (5am:*on-failure* (if debugp :debug nil)))
    (5am:run! 'the-great-rouclere)))

(defun fail (&rest args)
  (5am:fail "Test failure: ~S" args))

(defun make-url (port script)
  (format nil "http://localhost:~A~A" port script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests

(defun arrange-basics (&optional (url "/ping"))
  (r:expect (:post url)
    (r:with :body "I would like one magic, please!")
    (r:with :header "Magic-Dust" "Imagination")
    (r:with :accept "application/magic-show")
    (r:answer (h:+http-ok+)
      (r:with :body "That's perfect!!!")
      (r:with :content-type "text/magical")
      (r:with :header "Magic-Dust" "Prestidigitation"))))

(5am:test basics
  (r:with-magic-show (port)
    ;; Arrange
    (arrange-basics)
    ;; Act
    (multiple-value-bind (body status-code headers)
        (d:http-request (make-url port "/ping")
                        :method :post
                        :accept "application/magic-show"
                        :additional-headers '(("Magic-Dust" . "Imagination"))
                        :content "I would like one magic, please!")
      ;; Assert
      (5am:is (equal "That's perfect!!!" body))
      (5am:is (= h:+http-ok+ status-code))
      (5am:is (eql 0 (search "text/magical" (a:assoc-value headers :content-type))))
      (5am:is (equal "Prestidigitation" (a:assoc-value headers :magic-dust))))))

(5am:test expectations-single
  (r:with-magic-show (port :on-letdowns #'fail :on-surprises #'fail)
    ;; Arrange
    (arrange-basics)
    ;; Assert
    (5am:is (= 1 (length (r:expectations port))))
    (let ((expectation (pop (r:expectations port))))
      (5am:is (eq :post (getf expectation :method)))
      (5am:is (string= "I would like one magic, please!"
                       (getf expectation :body)))
      (5am:is (= 1 (getf expectation :times)))
      (5am:is (equal "/ping" (getf expectation :url)))
      (5am:is (a:set-equal '(("Magic-Dust" . "Imagination")
                             ("Accept" . "application/magic-show"))
                           (getf expectation :headers)
                           :test #'equal))
      (5am:is (equal '() (getf expectation :accept)))
      (let ((answer (getf expectation :answer)))
        (5am:is (= h:+http-ok+ (getf answer :code)))
        (5am:is (equal "That's perfect!!!" (getf answer :body)))
        (5am:is (a:set-equal '(("Magic-Dust" . "Prestidigitation")
                               ("Content-Type" . "text/magical"))
                             (getf answer :headers)
                             :test #'equal))))
    (5am:is (= 0 (length (r:expectations port))))))

(5am:test expectations-multiple
  (flet ((test (calls)
           (r:with-magic-show (port :on-letdowns #'fail :on-surprises #'fail)
             ;; Arrange
             (r:expect (:get "/ping")
               (r:answer (h:+http-switching-protocols+)))
             (r:expect (:get "/pong")
               (r:answer (h:+http-moved-permanently+)))
             (r:expect (:get "/pung")
               (r:answer (h:+http-unavailable-for-legal-reasons+)))
             ;; Assert
             (5am:is (= 3 (length (r:expectations port))))
             (let ((expectation (first (r:expectations port))))
               (5am:is (equal "/ping" (getf expectation :url))))
             (let ((expectation (second (r:expectations port))))
               (5am:is (equal "/pong" (getf expectation :url))))
             (let ((expectation (third (r:expectations port))))
               (5am:is (equal "/pung" (getf expectation :url))))
             (flet ((do-test (data)
                      (destructuring-bind (path status) data
                        (let* ((url (make-url port path))
                               (status-code (nth-value 1 (d:http-request url))))
                          (5am:is (= status status-code))))))
               (mapc #'do-test calls))
             (5am:is (= 0 (length (r:expectations port)))))))
    (let ((calls `(("/ping" ,h:+http-switching-protocols+)
                   ("/pong" ,h:+http-moved-permanently+)
                   ("/pung" ,h:+http-unavailable-for-legal-reasons+))))
      (test calls)
      (test (reverse calls)))))

(5am:test basic-authorization
  (r:with-magic-show (port :on-letdowns #'fail :on-surprises #'fail)
    (r:expect (:get "/")
      (r:with :basic-authorization "user" "pass")
      (r:answer (h:+http-ok+)))
    (let ((status (nth-value 1 (d:http-request (make-url port "/")
                                               :basic-authorization '("user" "pass")))))
      (5am:is (= status h:+http-ok+)))))

(5am:test letdown
  (let ((flag nil))
    (flet ((on-letdowns (expectations)
             (setf flag t)
             (5am:is (= 1 (length expectations)))
             (let ((expectation (first expectations)))
               (5am:is (eq :get (getf expectation :method)))
               (5am:is (= 1 (getf expectation :times)))
               (5am:is (equal "/nowhere" (getf expectation :url))))))
      (let ((string (with-output-to-string (*debug-io*)
                      (r:with-magic-show (port :on-letdowns #'on-letdowns :on-surprises #'fail)
                        (r:expect (:get "/nowhere"))))))
        (5am:is-true flag)
        (5am:is (eql 0 (search ";; The Great Rouclere still has 1 unmet expectations!" string)))))))

(5am:test surprise
  (let ((flag nil)
        expected-port)
    (flet ((on-surprises (surprises)
             (setf flag t)
             (5am:is (= 1 (length surprises)))
             (destructuring-bind (actual-port request expectations) (first surprises)
               (5am:is (eql expected-port actual-port))
               (5am:is (eq :post (h:request-method request)))
               (5am:is (string= "/nowhere" (h:request-uri request)))
               (5am:is (string= "Surprise!!!1" (h:raw-post-data :request request
                                                                :external-format :utf-8)))
               (5am:is (null expectations)))))
      (let ((string (with-output-to-string (*debug-io*)
                      (r:with-magic-show (port :on-surprises #'on-surprises :on-letdowns #'fail)
                        (setf expected-port port)
                        (let* ((url (make-url port "/nowhere")))
                          (multiple-value-bind (body status-code headers uri stream must-close reason)
                              (d:http-request url :method :post :content "Surprise!!!1")
                            (declare (ignore headers uri stream must-close))
                            (5am:is (= r:+http-magic-is-gone+ status-code))
                            (5am:is (string= "Magic Is Gone" reason))
                            (5am:is (eql 0 (search ";; The Great Rouclere is surprised by this request!" body)))
                            (5am:is (search "Surprise!!!1" body))
                            (5am:is (= 1 (length (r:surprises port))))
                            (let ((surprise (first (r:surprises port))))
                              (destructuring-bind (request expectations) surprise
                                (5am:is (eq :post (h:request-method request)))
                                (5am:is (string= "/nowhere" (h:request-uri request)))
                                (5am:is (string= "Surprise!!!1" (h:raw-post-data :request request
                                                                                 :external-format :utf-8)))
                                (5am:is (null expectations))))))))))
        (5am:is-true flag)
        (5am:is (eql 0 (search ";; The Great Rouclere has been surprised 1 times!" string)))))))

(5am:test permanent-expectations
  (let ((flag nil))
    (flet ((on-letdowns (expectations)
             (declare (ignore expectations))
             (setf flag t)))
      (r:with-magic-show (port :on-letdowns #'on-letdowns :on-surprises #'fail)
        (r:expect (:get "/always" :times t)
          (r:answer (h:+http-accepted+)))
        (5am:is (= 1 (length (r:expectations port))))
        (5am:is (loop with url = (make-url port "/always")
                      repeat 100
                      for status-code = (nth-value 1 (d:http-request url))
                      always (= status-code h:+http-accepted+)))
        (5am:is (= 1 (length (r:expectations port)))))
      (5am:is (null flag)))))

(5am:test multimagic
  (r:with-magic-show ((port-1 port-2 port-3) :on-letdowns #'fail :on-surprises #'fail)
    (flet ((spell (port url code)
             (r:with-wand-pointed-at (port)
               (r:expect (:get url)
                 (r:answer (code)))))
           (test (port script code)
             (let* ((url (make-url port script))
                    (status-code (nth-value 1 (d:http-request url))))
               (5am:is (= code status-code)))))
      (let ((set-1 (list port-1 "/abra" h:+http-accepted+))
            (set-2 (list port-2 "/kadabra" h:+http-gone+))
            (set-3 (list port-3 "/alakazam" h:+http-bad-gateway+)))
        (apply #'spell set-1)
        (apply #'spell set-2)
        (apply #'spell set-3)
        (apply #'test set-2)
        (apply #'test set-3)
        (apply #'test set-1)))))


(5am:test variables-predicates-side-effects
  (let (result)
    (r:with-magic-show (port :on-letdowns #'fail :on-surprises #'fail)
      (r:expect (:get "/foo/:bar/baz/:quux/:frob")
        (r:with :predicate (lambda () (string= "123" (r:var "bar"))))
        (r:answer (h:+http-ok+)
          (r:with :side-effects
                  (lambda () (setf result (list (r:var "quux") (r:var "frob")))))))
      (let* ((url (make-url port "/foo/123/baz/456/789"))
             (status (nth-value 1 (d:http-request url))))
        (5am:is (= h:+http-ok+ status))
        (5am:is (equal '("456" "789") result))))))

(5am:test variables-multiple-lengths
  (r:with-magic-show (port :on-letdowns #'fail :on-surprises #'fail)
    (r:expect (:get "/:foo")
      (r:with :predicate (lambda () (string= "123" (r:var "foo")))))
    (r:expect (:get "/:foo/:bar")
      (r:with :predicate (lambda () (and (string= "123" (r:var "foo"))
                                         (string= "456" (r:var "bar"))))))
    (r:expect (:get "/:foo/:bar/:baz")
      (r:with :predicate (lambda () (and (string= "123" (r:var "foo"))
                                         (string= "456" (r:var "bar"))
                                         (string= "789" (r:var "baz"))))))
    (flet ((test (script)
             (5am:is (= h:+http-ok+ (nth-value 1 (d:http-request (make-url port script)))))))
      (test "/123/456/789")
      (test "/123")
      (test "/123/456"))))

(5am:test multiple-headers
  (r:with-magic-show (port :on-letdowns #'fail :on-surprises #'fail)
    (r:expect (:get "/")
      (r:with :header "Foo" "Bar"))
    (r:expect (:get "/")
      (r:with :header "Foo" "Baz"))
    (r:expect (:get "/")
      (r:with :header "Foo" "Ban"))
    (r:expect (:get "/")
      (r:with :header "Quux" "Fred"))
    (flet ((test (key value)
             (5am:is (= h:+http-ok+ (nth-value 1 (d:http-request (make-url port "/")
                                                                 :additional-headers
                                                                 (list (cons key value))))))))
      (test "Quux" "Fred")
      (test "Foo" "Baz")
      (test "Foo" "Bar")
      (test "Foo" "Ban"))))

(5am:test multiple-times
  (r:with-magic-show (port :on-letdowns #'fail :on-surprises #'fail)
    (r:expect (:get "/" :times 3))
    (flet ((test ()
             (5am:is (= h:+http-ok+ (nth-value 1 (d:http-request (make-url port "/")))))))
      (test)
      (test)
      (test))))

(defmacro signals* (report &body body)
  (a:with-gensyms (condition block)
    (a:once-only (report)
      `(block ,block
         (handler-bind ((condition (lambda (,condition)
                                     (when (eql 0 (search ,report (princ-to-string ,condition)))
                                       (5am:pass)
                                       (return-from ,block nil)))))
           ,@body)
         (5am:fail "Condition with report ~S not signaled." ,report)))))

(5am:test errors
  (macrolet ((without-expectations ((port-var) &body body)
               `(unwind-protect (progn ,@body)
                  (setf (r:expectations ,port-var) nil))))
    (r:with-magic-show (port :on-letdowns #'fail :on-surprises #'fail)
      (without-expectations (port)
        (r:expect (:get "/")
          (r:with :header "Foo" "Bar")
          (signals* "The Great Rouclere will already expect header \"Foo\" as \"Bar\"!"
            (r:with :header "Foo" "Baz"))))
      (without-expectations (port)
        (r:expect (:get "/")
          (r:answer (h:+http-ok+)
            (r:with :header "Foo" "Bar")
            (signals* "The Great Rouclere will already respond with header \"Foo\" as \"Bar\"!"
              (r:with :header "Foo" "Baz")))))
      (without-expectations (port)
        (r:expect (:post "/")
          (r:with :body "body")
          (signals* "The Great Rouclere will already expect body \"body\"!"
            (r:with :body "nobody"))))
      (without-expectations (port)
        (r:expect (:get "/")
          (r:answer (h:+http-ok+)
            (r:with :body "body")
            (signals* "The Great Rouclere will already respond with body \"body\"!"
              (r:with :body "nobody")))))
      (signals* "The Great Rouclere is not aware of a variable named :foo!"
        (r:var "foo" "/" "/"))
      (signals* "The Great Rouclere has found duplicate variable \":foo\"!"
        (r:var "foo" "/:foo/:foo" "/123/456")))))
