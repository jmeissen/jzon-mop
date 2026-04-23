(in-package #:cl-user)

(uiop:define-package jzon-mop/jzon-mop
  (:use #:cl)
  (:import-from #:serapeum
                #:dict
                #:href)
  (:import-from #:symbol-munger
                #:underscores->lisp-symbol)
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
           #:jzon-mop-model-class
           #:make-object-parser-for-class))

(in-package #:jzon-mop/jzon-mop)

(defvar *default-key-parser* #'underscores->lisp-symbol)

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
    :initform (dict)
    :reader model-slot-by-symbol)
   (child-class-by-symbol
    :initform (dict)
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
          q          (when s (setf subtype s))))
      (when (typep effective 'jzon-mop-effective-slot-definition)
        (setf (slot-value effective 'subtype) subtype)))
    effective))

(defun model-key->symbol (model-class key-string &optional (package *package*))
  (let ((parser (car (model-key-parser model-class))))
    (cond
      ((null parser) (funcall *default-key-parser* key-string package))
      ((symbolp parser) (funcall (symbol-function parser) key-string))
      ((functionp parser) (funcall parser key-string))
      ((and (consp parser)
            (eq 'lambda (car parser))) (funcall (eval parser) key-string))
      (t (error "Unknown parser type ~S" parser)))))

(defun finalize-model-class-caches (class)
  (let ((slot-by-symbol (model-slot-by-symbol class))
        (child-by-symbol (model-child-class-by-symbol class)))
    (clrhash slot-by-symbol)
    (clrhash child-by-symbol)
    (dolist (slot (class-slots class))
      (let* ((slot-name (slot-definition-name slot))
             (type (slot-definition-type slot))
             (subtype (and (typep slot 'jzon-mop-effective-slot-definition) (jzon-mop-slot-subtype slot))))
        (setf (href slot-by-symbol slot-name) slot)
        (when (and (or (eq type 'vector)
                       (eq type 'list)
                       (eq type 'sequence)
                       (eq type 'array))
                   subtype)
          (setf (href child-by-symbol slot-name) subtype))
        (when (and (not (member type '(vector list sequence array) :test #'eq))
                   (symbolp type)
                   (find-class type nil))
          (setf (href child-by-symbol slot-name) type)))))
  class)

(defmethod finalize-inheritance :after ((class jzon-mop-model-class))
  (finalize-model-class-caches class))

(defun model-slot-for-json-key (class-symbol key-string &optional (package *package*))
  (let ((class (find-class class-symbol)))
    (href (model-slot-by-symbol class)
          (model-key->symbol class key-string package))))

(defun model-child-class-for-json-key (class-symbol key-string &optional (package *package*))
  (let ((class (find-class class-symbol)))
    (href (model-child-class-by-symbol class)
          (model-key->symbol class key-string package))))

(defun set-model-slot-by-slot-definition (instance slot-definition value)
  (when slot-definition
    (setf (slot-value instance (slot-definition-name slot-definition)) value))
  instance)

(defun make-object-parser-for-class (toplevel-class-symbol &optional (package *package*))
  "Create a function that parses the TOPLEVEL-CLASS-SYMBOL

TOPLEVEL-CLASS-SYMBOL is the class-name symbol.

PACKAGE is the optional package name in which the class-name symbol
 resides (if different that *PACKAGE*).

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
       (parse-object (parser expected-class)
         (let ((instance (allocate-instance (find-class expected-class)))
               (current-key-string nil)
               (current-slot nil)
               (current-child-class nil))
           (loop
             (multiple-value-bind (event value) (parse-next parser)
               (cond
                 ((eq event :object-key)
                  (setf current-key-string value)
                  (setf current-slot (model-slot-for-json-key expected-class
                                                              current-key-string
                                                              package))
                  (setf current-child-class (model-child-class-for-json-key expected-class
                                                                            current-key-string
                                                                            package)))
                 ((eq event :end-object) (return instance))
                 ((eq event :value)
                  (if current-slot
                      (set-model-slot-by-slot-definition instance current-slot value)
                      nil)
                  (setf current-key-string nil current-slot nil current-child-class nil))
                 ((eq event :begin-array)
                  (if current-slot
                      (let* ((element-class (or current-child-class expected-class))
                             (array-value (parse-array parser element-class)))
                        (set-model-slot-by-slot-definition instance current-slot array-value))
                      (error "Unexpected object start for ~A" current-key-string))
                  (setf current-key-string nil
                        current-slot nil
                        current-child-class nil))
                 ((eq event :begin-object)
                  (if current-slot
                      (set-model-slot-by-slot-definition
                       instance current-slot
                       (parse-object parser (or current-child-class expected-class)))
                      (error "Unexpected object start for ~A" current-key-string))
                  (setf current-key-string nil
                        current-slot nil
                        current-child-class nil))
                 (t
                  (error "Unexpected JSON event in object: ~S" event))))))))
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
