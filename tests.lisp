(defpackage #:the-great-rouclere/tests
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:d #:drakma)
                    (#:h #:hunchentoot)
                    (#:r #:the-great-rouclere))
  (:export #:magic!))

(in-package #:the-great-rouclere/tests)

(defun magic! ()
  (5am:run! 'the-great-rouclere))

(5am:def-suite the-great-rouclere)

(5am:in-suite the-great-rouclere)

(defun arrange-basics (&optional (url "/ping"))
  (r:expect (:get url)
    (r:with :header "Magic-Dust" "Imagination")
    (r:with :accept "application/magic-show")
    (r:answer (h:+http-ok+)
      (r:with :body "That's perfect!!!")
      (r:with :content-type "text/magical")
      (r:with :header "Magic-Dust" "Prestidigitation"))))

(5am:test basics
  (r:with-magic (port)
    ;; Arrange
    (arrange-basics)
    ;; Act
    (multiple-value-bind (body status-code headers)
        (d:http-request (format nil "http://localhost:~D/ping" port)
                        :accept "application/magic-show"
                        :additional-headers '(("Magic-Dust" . "Imagination")))
      ;; Assert
      (5am:is (equal "That's perfect!!!" body))
      (5am:is (=  200 status-code))
      (5am:is (= 0 (search "text/magical" (a:assoc-value headers :content-type))))
      (5am:is (equal "Prestidigitation" (a:assoc-value headers :magic-dust))))))

(5am:test expectations-single
  (r:with-magic (port)
    ;; Arrange
    (arrange-basics)
    ;; Assert
    (5am:is (= 1 (length (r:expectations))))
    (let ((expectation (pop (r:expectations))))
      (5am:is (eq :get (getf expectation :method)))
      (5am:is (= 1 (getf expectation :times)))
      (5am:is (equal "/ping" (getf expectation :url)))
      (5am:is (a:set-equal '(("Magic-Dust" . "Imagination")
                             ("Accept" . "application/magic-show"))
                           (getf expectation :headers)
                           :test #'equal))
      (5am:is (equal '() (getf expectation :accept)))
      (let ((answer (getf expectation :answer)))
        (5am:is (= 200 (getf answer :code)))
        (5am:is (equal "That's perfect!!!" (getf answer :body)))
        (5am:is (a:set-equal '(("Magic-Dust" . "Prestidigitation")
                               ("Content-Type" . "text/magical"))
                             (getf answer :headers)
                             :test #'equal))))
    (5am:is (= 0 (length (r:expectations))))))

(5am:test expectations-multiple
  (flet ((test (calls)
           (r:with-magic (port)
             ;; Arrange
             (r:expect (:get "/ping"))
             (r:expect (:get "/pong"))
             (r:expect (:get "/pung"))
             ;; Assert
             (5am:is (= 3 (length (r:expectations))))
             (let ((expectation (first (r:expectations))))
               (5am:is (equal "/ping" (getf expectation :url))))
             (let ((expectation (second (r:expectations))))
               (5am:is (equal "/pong" (getf expectation :url))))
             (let ((expectation (third (r:expectations))))
               (5am:is (equal "/pung" (getf expectation :url))))
             (flet ((test (path)
                      (let* ((url (format nil "http://localhost:~D~A" port path))
                             (status-code (nth-value 1 (d:http-request url))))
                        (5am:is (= 200 status-code)))))
               (mapc #'test calls))
             (5am:is (= 0 (length (r:expectations)))))))
    (let ((calls '("/ping" "/pong" "/pung")))
      (test calls)
      (test (reverse calls)))))

(5am:test letdown
  (let ((flag nil))
    (flet ((on-letdown (expectations)
             (setf flag t)
             (5am:is (= 1 (length expectations)))
             (let ((expectation (first expectations)))
               (5am:is (eq :get (getf expectation :method)))
               (5am:is (= 1 (getf expectation :times)))
               (5am:is (equal "/nowhere" (getf expectation :url))))))
      (let ((string (with-output-to-string (*debug-io*)
                      (r:with-magic (port :on-letdown #'on-letdown)
                        (r:expect (:get "/nowhere"))))))
        (5am:is-true flag)
        (5am:is (= 0 (search ";; The Great Rouclere still has has 1 unmet expectations!" string)))))))

(5am:test surprise
  (let ((flag nil))
    (flet ((on-surprise (surprises)
             (setf flag t)
             (5am:is (= 1 (length surprises)))
             (destructuring-bind (request expectations) (first surprises)
               (5am:is (eq :get (h:request-method request)))
               (5am:is (string= "/nowhere" (h:request-uri request)))
               (5am:is (null expectations)))))
      (let ((string (with-output-to-string (*debug-io*)
                      (r:with-magic (port :on-surprise #'on-surprise)
                        (let* ((url (format nil "http://localhost:~D/nowhere" port)))
                          (multiple-value-bind (body status-code) (d:http-request url)
                            (5am:is (= 555 status-code))
                            (= 0 (search ";; The Great Rouclere is surprised by this request!" body))))))))
        (5am:is-true flag)
        (5am:is (= 0 (search ";; The Great Rouclere has been surprised 1 times!" string)))))))
