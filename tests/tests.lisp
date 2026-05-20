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

(defclass mixed-key-record ()
  ((underscored-name
    :accessor mixed-key-record-underscored-name)
   (from-camel-case
    :accessor mixed-key-record-from-camel-case))
  (:metaclass jzon-mop-model-class)
  (:key-parser mixed-key-parser))

(defun parse-json (class json)
  (funcall (make-object-parser-for-class class)
           json))

(defun mixed-key-parser (string)
  (if (string= string "fromCamelCase")
      (intern "FROM-CAMEL-CASE" (find-package :jzon-mop-test))
      (symbol-munger:underscores->lisp-symbol string (find-package :jzon-mop-test))))

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

(define-test parses-with-custom-key-parser
  (let ((record (parse-json 'mixed-key-record
                            "{\"underscored_name\":\"ok\",\"fromCamelCase\":\"still-ok\"}")))
    (is equal "ok" (mixed-key-record-underscored-name record))
    (is equal "still-ok" (mixed-key-record-from-camel-case record))))
