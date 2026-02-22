(asdf:defsystem #:the-great-rouclere
  :description "HTTP mocking library"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:hunchentoot
               #:closer-mop)
  :serial t
  :components ((:file "the-great-rouclere"))
  :in-order-to ((test-op (test-op #:the-great-rouclere/tests))))

(asdf:defsystem #:the-great-rouclere/tests
  :description "HTTP mocking library testware"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :depends-on (#:drakma
               #:fiveam
               #:the-great-rouclere)
  :serial t
  :components ((:file "tests"))
  :perform (test-op (o s) (uiop:symbol-call '#:the-great-rouclere/tests '#:magic!)))
