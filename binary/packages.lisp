(in-package :cl-user)

(defpackage :xlxs4.binary
  (:use :common-lisp :xlxs4.macro-util)
  (:export :define-binary-class
           :define-tagged-binary-class
           :define-binary-type
           :read-value
           :write-value
           :*in-progress-objects*
           :parent-of-type
           :current-binary-object
           :+null+))
