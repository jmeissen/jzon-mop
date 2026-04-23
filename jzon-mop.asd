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
               :serapeum
               :symbol-munger
               :jzon-mop/jzon-mop))
