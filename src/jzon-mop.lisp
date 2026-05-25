(in-package #:cl-user)

(uiop:define-package jzon-mop/jzon-mop
  (:use #:cl)
  (:import-from #:symbol-munger
                #:camel-case->lisp-symbol)
  (:import-from #:com.inuoe.jzon
                #:parse-next
                #:parse-next-element
                #:with-parser)
  (:import-from #:closer-mop
                #:standard-direct-slot-definition
                #:standard-effective-slot-definition
                #:validate-superclass
                #:direct-slot-definition-class
                #:effective-slot-definition-class
                #:compute-effective-slot-definition
                #:class-slots
                #:slot-definition-name
                #:slot-definition-type
                #:finalize-inheritance)
  (:nicknames #:jzon-mop
              #:jzop)
  (:export #:*default-key-parser*
           #:*warn-on-missing-slots*
           #:jzon-mop-model-class
           #:make-object-parser-for-class
           #:parse))

(in-package #:jzon-mop/jzon-mop)

(defvar *default-key-parser* #'camel-case->lisp-symbol)

(defvar *warn-on-missing-slots* t
  "When true, missing model slots signal a warning instead of an error.")

(defclass jzon-mop-slot-definition (standard-direct-slot-definition)
  ((subtype
    :initarg :subtype
    :reader jzon-mop-slot-subtype
    :initform nil)))

(defclass jzon-mop-effective-slot-definition (standard-effective-slot-definition)
  ((subtype
    :initarg :subtype
    :reader jzon-mop-slot-subtype
    :initform nil)))

(defclass jzon-mop-model-class (standard-class)
  ((key-parser
    :initarg :key-parser
    :reader model-key-parser
    :initform nil)
   (slot-by-symbol
    :initform (make-hash-table :test #'equal)
    :reader model-slot-by-symbol)
   (child-class-by-symbol
    :initform (make-hash-table :test #'equal)
    :reader model-child-class-by-symbol)))

(defmethod validate-superclass ((class jzon-mop-model-class) (superclass standard-class)) t)
(defmethod validate-superclass ((class standard-class) (superclass jzon-mop-model-class)) t)

(defmethod direct-slot-definition-class ((class jzon-mop-model-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'jzon-mop-slot-definition))

(defmethod effective-slot-definition-class ((class jzon-mop-model-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'jzon-mop-effective-slot-definition))

(defmethod compute-effective-slot-definition ((class jzon-mop-model-class) name direct-slot-definitions)
  (let ((effective (call-next-method)))
    (let (subtype)
      (dolist (d direct-slot-definitions)
        (let ((s (and (typep d 'jzon-mop-slot-definition) (jzon-mop-slot-subtype d))))
          (when s (setf subtype s))))
      (when (typep effective 'jzon-mop-effective-slot-definition)
        (setf (slot-value effective 'subtype) subtype)))
    effective))

(defun model-key->symbol (model-class key-string)
  (let ((parser (car (model-key-parser model-class))))
    (funcall
     (cond ((null parser)
            *default-key-parser*)
           ((symbolp parser)
            (symbol-function parser))
           ((functionp parser)
            parser)
           ((and (consp parser)
                 (eq 'lambda (car parser)))
            (eval parser))
           (t (error "Unknown key parser type ~S" (type-of parser))))
     key-string
     (symbol-package (class-name model-class)))))

(defun finalize-model-class-caches (class)
  (let ((slot-by-symbol (model-slot-by-symbol class))
        (child-by-symbol (model-child-class-by-symbol class)))
    (clrhash slot-by-symbol)
    (clrhash child-by-symbol)
    (dolist (slot (class-slots class))
      (let* ((slot-name (slot-definition-name slot))
             (type (slot-definition-type slot))
             (subtype (and (typep slot 'jzon-mop-effective-slot-definition) (jzon-mop-slot-subtype slot))))
        (setf (gethash slot-name slot-by-symbol) slot)
        (when (and (or (eq type 'vector)
                       (eq type 'list)
                       (eq type 'sequence)
                       (eq type 'array))
                   subtype)
          (setf (gethash slot-name child-by-symbol) subtype))
        (when (and (not (member type '(vector list sequence array) :test #'eq))
                   (symbolp type)
                   (find-class type nil))
          (setf (gethash slot-name child-by-symbol) type)))))
  class)

(defmethod finalize-inheritance :after ((class jzon-mop-model-class))
  (finalize-model-class-caches class))

(defun model-slot-for-json-key (class-symbol key-string)
  (let ((class (find-class class-symbol)))
    (gethash (model-key->symbol class key-string)
             (model-slot-by-symbol class))))

(defun model-child-class-for-json-key (class-symbol key-string)
  (let ((class (find-class class-symbol)))
    (gethash (model-key->symbol class key-string)
             (model-child-class-by-symbol class))))

(defun set-model-slot-by-slot-definition (instance slot-definition value)
  (when slot-definition
    (setf (slot-value instance (slot-definition-name slot-definition)) value))
  instance)

(defun signal-missing-slot (class-symbol key-string)
  (if *warn-on-missing-slots*
      (warn "Non-existent slot \"~A\" for \"~S\"" key-string class-symbol)
      (error "Non-existent slot \"~A\" for \"~S\"" key-string class-symbol)))

(defun make-object-parser-for-class (toplevel-class-symbol)
  "Create a function that parses the TOPLEVEL-CLASS-SYMBOL

TOPLEVEL-CLASS-SYMBOL is the class-name symbol.

Returns a function takes the same arguments as `jzon:parse'."
  (labels
      ((parse-array (parser element-class)
         (let ((elements nil))
           (loop
             (multiple-value-bind (event value) (parse-next parser)
               (cond
                 ((eq event :end-array)
                  (return (coerce (reverse elements) 'simple-vector)))
                 ((eq event :value)
                  (push value elements))
                 ((eq event :begin-array)
                  (push (parse-array parser element-class) elements))
                 ((eq event :begin-object)
                  (push (parse-object parser element-class) elements))
                 (t
                  (error "Unexpected JSON event in array: ~S" event)))))))
       (parse-hash-table-object (parser)
         (let ((instance (make-hash-table :test #'equal))
               (current-key-string nil))
           (loop
             (multiple-value-bind (event value) (parse-next parser)
               (cond
                 ((eq event :object-key)
                  (setf current-key-string value))
                 ((eq event :end-object) (return instance))
                 ((eq event :value)
                  (when current-key-string
                    (setf (gethash current-key-string instance) value))
                  (setf current-key-string nil))
                 ((eq event :begin-array)
                  (if current-key-string
                      (setf (gethash current-key-string instance)
                            (parse-array parser 'hash-table))
                      (error "Unexpected object start for ~A" current-key-string))
                  (setf current-key-string nil))
                 ((eq event :begin-object)
                  (if current-key-string
                      (setf (gethash current-key-string instance)
                            (parse-hash-table-object parser))
                      (error "Unexpected object start for ~A" current-key-string))
                  (setf current-key-string nil))
                 (t
                  (error "Unexpected JSON event in object: ~S" event)))))))
       (parse-object (parser expected-class)
         (if (member expected-class '(nil t hash-table) :test #'eq)
             (parse-hash-table-object parser)
             (let ((instance (allocate-instance (find-class expected-class)))
                   (current-key-string nil)
                   (current-slot nil)
                   (current-child-class nil))
               (loop
                 (multiple-value-bind (event value) (parse-next parser)
                   (cond
                     ((eq event :object-key)
                      (setf current-key-string value)
                      (setf current-slot (model-slot-for-json-key expected-class current-key-string))
                      (setf current-child-class (model-child-class-for-json-key expected-class current-key-string)))
                     ((eq event :end-object) (return instance))
                     ((eq event :value)
                      (if current-slot
                          (set-model-slot-by-slot-definition instance current-slot value)
                          (signal-missing-slot expected-class current-key-string))
                      (setf current-key-string nil current-slot nil current-child-class nil))
                     ((eq event :begin-array)
                      (if current-slot
                          (let* ((element-class (or current-child-class 'hash-table))
                                 (array-value (parse-array parser element-class)))
                            (set-model-slot-by-slot-definition instance current-slot array-value))
                          (progn
                            (signal-missing-slot expected-class current-key-string)
                            (parse-array parser 'hash-table)))
                      (setf current-key-string nil
                            current-slot nil
                            current-child-class nil))
                     ((eq event :begin-object)
                      (if current-slot
                          (set-model-slot-by-slot-definition
                           instance current-slot
                           (parse-object parser (or current-child-class 'hash-table)))
                          (progn
                            (signal-missing-slot expected-class current-key-string)
                            (parse-object parser 'hash-table)))
                      (setf current-key-string nil
                            current-slot nil
                            current-child-class nil))
                     (t
                      (error "Unexpected JSON event in object: ~S" event)))))))))
    (lambda (in &key max-depth allow-comments allow-trailing-comma allow-multiple-content max-string-length key-fn)
      (with-parser (parser in
                           :allow-comments allow-comments
                           :allow-trailing-comma allow-trailing-comma
                           :allow-multiple-content allow-multiple-content
                           :max-string-length max-string-length
                           :key-fn key-fn)
        (multiple-value-bind (event value) (parse-next parser)
          (declare (ignore value))
          (cond
            ((eq event :begin-object)
             (parse-object parser toplevel-class-symbol))
            ((eq event :begin-array)
             (parse-array parser toplevel-class-symbol))
            ((eq event :value)
             (parse-next-element parser :max-depth max-depth))
            (t
             (error "Unexpected toplevel JSON event: ~S" event))))))))

(defun parse (source class-symbol)
  (funcall (make-object-parser-for-class class-symbol)
           source))
