(in-package #:cl-user)

(uiop:define-package #:jzon-mop-test
  (:use #:cl #:parachute)
  (:import-from #:jzon-mop
                #:parse
                #:jzon-mop-model-class))
