(defpackage :cffi-utils
  (:use :common-lisp :cffi :alexandria)
  (:export #:defcclass #:defcmethod #:it #:self #:dynamic-c-array-type))

(in-package :cffi-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *c-class-structs* (make-hash-table)))


(defmacro cffi-defun (method return-type &optional cffi-struct-name ptr &rest parameters)
  (let ((cfun-name (if cffi-struct-name
                       (list (concatenate 'string cffi-struct-name "_" (car method)) (second method))
                       (list (car method) (second method)))))
    `(defcfun ,cfun-name ,return-type ,@(if ptr '((pointer :pointer)) '()) ,@parameters)))


(defmacro dynamic-c-array-type (val &optional val-length)
  `(list :array :string (or ,val-length ,`(length ,val))))

(defmacro defcclass (name c-name)
  (setf (gethash name *c-class-structs*) c-name))

(defmacro defcmethod (class c-method method-name return-type &optional has-ptr parameters &body body)
  (let* ((cffi-method-name (gensym c-method))
         (c-struct-name    (gethash class *c-class-structs*)))
    `(progn
       (cffi-defun (,c-method ,cffi-method-name) ,return-type ,c-struct-name ,has-ptr
                   ,@(when (listp parameters) parameters))
       (defmethod ,method-name ((self ,class) &rest method-parameters)
         (format t "~a ~a ~%" ,(string cffi-method-name) method-parameters)
         ,(if (> (length body) 0)
              `(let ((it (apply ',cffi-method-name ,@(when has-ptr `((slot-value self ',has-ptr))) method-parameters)))
                 (declare (ignorable it))
                 ,@body)
              `(apply ',cffi-method-name ,@(when has-ptr `((slot-value self ',has-ptr))) method-parameters))))))
