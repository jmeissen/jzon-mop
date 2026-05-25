(in-package #:jzon-mop-test)

(defclass record-item ()
  ((this-val
    :accessor record-item-this-val)
   (that-val
    :accessor record-item-that-val))
  (:metaclass jzon-mop-model-class))

(defclass record-wrapper ()
  ((data
    :accessor record-wrapper-data
    :type vector
    :subtype record-item)
   (primary-item
    :accessor record-wrapper-primary-item
    :type record-item))
  (:metaclass jzon-mop-model-class))

(defclass untyped-array-wrapper ()
  ((items
    :accessor untyped-array-wrapper-items))
  (:metaclass jzon-mop-model-class))

(defclass mixed-key-record ()
  ((underscored-name
    :accessor mixed-key-record-underscored-name)
   (from-camel-case
    :accessor mixed-key-record-from-camel-case))
  (:metaclass jzon-mop-model-class)
  (:key-parser mixed-key-parser))

(defun parse-json (class json)
  (parse json class))

(defun camelcase-key-parser (string package)
  (symbol-munger:camel-case->lisp-symbol string package))

(defun mixed-key-parser (string package)
  (if (find #\_ string)
      (symbol-munger:underscores->lisp-symbol string package)
      (camelcase-key-parser string package)))

(defclass camelcase-key-record ()
  ((from-camel-case
    :accessor camelcase-key-record-from-camel-case))
  (:metaclass jzon-mop-model-class)
  (:key-parser camelcase-key-parser))

(define-test parses-camelcase-key-parser
    (let ((record (parse-json 'camelcase-key-record
                              "{\"fromCamelCase\":\"ok\"}")))
      (is equal "ok" (camelcase-key-record-from-camel-case record))))

(define-test fails-to-parse-underscored-key-with-camelcase-key-parser
    (let ((record (parse-json 'camelcase-key-record
                              "{\"from_camel_case\":\"ok\"}")))
      (is equal "ok" (camelcase-key-record-from-camel-case record))))

(define-test fails-to-parse-mixed-camelcase-key-with-custom-key-parser
    (let ((record (parse-json 'mixed-key-record
                              "{\"fromCamelcase\":\"still-nope\"}")))
      (is eq t
          (handler-case
              (progn
                (mixed-key-record-from-camel-case record)
                nil)
            (unbound-slot () t)))))

(define-test parses-simple-object
  (let ((item (parse-json 'record-item "{\"this_val\":1,\"that_val\":2}")))
    (is eq t (typep item 'record-item))
    (is = 1 (record-item-this-val item))
    (is = 2 (record-item-that-val item))))

(define-test parses-top-level-array-into-vector
  (let ((items (parse-json 'record-item
                           "[{\"this_val\":1,\"that_val\":2},{\"this_val\":3,\"that_val\":4}]")))
    (is eq t (typep items 'simple-vector))
    (is = 2 (length items))
    (is = 1 (record-item-this-val (aref items 0)))
    (is = 4 (record-item-that-val (aref items 1)))))

(define-test parses-nested-object-and-array-slots
  (let ((wrapper (parse-json 'record-wrapper
                             "{\"data\":[{\"this_val\":1,\"that_val\":2}],\"primary_item\":{\"this_val\":3,\"that_val\":4}}")))
    (is eq t (typep wrapper 'record-wrapper))
    (is eq t (typep (record-wrapper-data wrapper) 'simple-vector))
    (is = 1 (length (record-wrapper-data wrapper)))
    (is eq t (typep (aref (record-wrapper-data wrapper) 0) 'record-item))
    (is = 1 (record-item-this-val (aref (record-wrapper-data wrapper) 0)))
    (is = 3 (record-item-this-val (record-wrapper-primary-item wrapper)))
    (is = 4 (record-item-that-val (record-wrapper-primary-item wrapper)))))

(define-test parses-empty-array-in-untyped-object-slot
  (let ((wrapper (parse-json 'untyped-array-wrapper
                             "{\"items\":[]}")))
    (is eq t (typep (untyped-array-wrapper-items wrapper) 'simple-vector))
    (is = 0 (length (untyped-array-wrapper-items wrapper)))))

(define-test warns-on-missing-primitive-slot-by-default
  (let ((warning nil))
    (handler-bind
        ((warning (lambda (c)
                    (setf warning c)
                    (muffle-warning))))
      (let ((item (parse-json 'record-item
                              "{\"this_val\":1,\"missing_val\":99,\"that_val\":2}")))
        (is eq t (typep warning 'warning))
        (is = 1 (record-item-this-val item))
        (is = 2 (record-item-that-val item))))))

(define-test warns-and-skips-missing-nested-slot-by-default
  (let ((warning nil))
    (handler-bind
        ((warning (lambda (c)
                    (setf warning c)
                    (muffle-warning))))
      (let ((wrapper (parse-json 'record-wrapper
                                 "{\"ignored_items\":[{\"this_val\":9,\"that_val\":10}],\"data\":[{\"this_val\":1,\"that_val\":2}],\"primary_item\":{\"this_val\":3,\"that_val\":4}}")))
        (is eq t (typep warning 'warning))
        (is eq t (typep (record-wrapper-data wrapper) 'simple-vector))
        (is = 1 (length (record-wrapper-data wrapper)))
        (is = 3 (record-item-this-val (record-wrapper-primary-item wrapper)))
        (is = 4 (record-item-that-val (record-wrapper-primary-item wrapper)))))))

(define-test errors-on-missing-slot-when-strict
  (let ((jzon-mop:*warn-on-missing-slots* nil))
    (declare (special jzon-mop:*warn-on-missing-slots*))
    (is eq t
        (handler-case
            (progn
              (parse-json 'record-item
                          "{\"this_val\":1,\"missing_val\":99,\"that_val\":2}")
              nil)
          (error () t)))))

(define-test parses-with-custom-key-parser
  (let ((record (parse-json 'mixed-key-record
                            "{\"underscored_name\":\"ok\",\"fromCamelCase\":\"still-ok\"}")))
    (is equal "ok" (mixed-key-record-underscored-name record))
    (is equal "still-ok" (mixed-key-record-from-camel-case record))))
