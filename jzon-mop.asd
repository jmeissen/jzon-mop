#-asdf3.1 (error "system-name requires ASDF 3.1 or later.")
(asdf:defsystem #:jzon-mop
  :class :package-inferred-system
  :pathname #p"src/"
  :description "Direct JSON to instance with MOP and com.inuoe.jzon."
  :author "Jeffrey Meissen <jeffrey@meissen.email>"
  :license "MIT"
  :version "0.0.1"
  :depends-on (:log4cl
               :com.inuoe.jzon
               :closer-mop
               :symbol-munger
               :jzon-mop/jzon-mop)
  :in-order-to ((test-op (test-op "jzon-mop/test"))))

(asdf:defsystem #:jzon-mop/test
  :long-name "jzon-mop test system"
  :description "Test system for jzon-mop"
  :author "Jeffrey Meissen <jeffrey@meissen.email>"
  :license "MIT"
  :pathname #p"tests/"
  :serial t
  :version "0.0.1"
  :components ((:file "package")
               (:file "tests"))
  :depends-on (:parachute :jzon-mop)
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :jzon-mop-test)))
