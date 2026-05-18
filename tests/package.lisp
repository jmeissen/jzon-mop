(in-package #:cl-user)

(uiop:define-package #:jzon-mop-test
  (:use #:cl #:parachute)
  (:import-from #:jzon-mop
                #:make-object-parser-for-class
                #:jzon-mop-model-class))
