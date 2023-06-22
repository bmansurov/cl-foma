(defpackage cl-foma
  (:use :cl)
  (:export :make-fst
           :apply-down
           :apply-up
           :apply-lower-words
           :apply-upper-words
           :apply-words
           :apply-random-lower
           :apply-random-upper
           :apply-random-words))
(in-package :cl-foma)

(cffi:define-foreign-library libfoma
  (:unix (:or "libfoma.so.0.9.18" "libfoma.so"))
  (t (:default "libfoma")))

(cffi:use-foreign-library libfoma)

(cffi:defcstruct fsm
  (name :string)
  (arity :int)
  (arccount :int)
  (statecount :int)
  (linecount :int)
  (finalcount :int)
  (pathcount :long-long)
  (is_deterministic :int)
  (is_pruned :int)
  (is_minimized :int)
  (is_epsilon_free :int)
  (is_loop_free :int)
  (is_completed :int)
  (arcs_sorted_in :int)
  (arcs_sorted_out :int)
  (fsm-state :pointer)
  (sigma :pointer)
  (medlookup :pointer))

(cffi:defcfun ("fsm_parse_regex" foma-fsm-parse-regex)
    (:pointer (:struct fsm))
  (regex :string)
  (defined-networks :pointer)
  (defined-functions :pointer))

(cffi:defcfun ("fsm_read_binary_file" foma-fsm-read-binary-file)
    (:pointer (:struct fsm))
  (filename :string))

(cffi:defcfun ("apply_init" foma-apply-init) :pointer
  (fsm :pointer (:struct fsm)))
(cffi:defcfun ("apply_clear" foma-apply-clear) :void
  (apply-handle :pointer))

(cffi:defcfun ("apply_down" foma-apply-down) :string
  (apply-handle :pointer)
  (word :string))
(cffi:defcfun ("apply_up" foma-apply-up) :string
  (apply-handle :pointer)
  (word :string))

(cffi:defcfun ("apply_lower_words" foma-apply-lower-words) :string
  (apply-handle :pointer))
(cffi:defcfun ("apply_upper_words" foma-apply-upper-words) :string
  (apply-handle :pointer))
(cffi:defcfun ("apply_words" foma-apply-words) :string
  (apply-handle :pointer))

(cffi:defcfun ("apply_random_lower" foma-apply-random-lower) :string
  (apply-handle :pointer))
(cffi:defcfun ("apply_random_upper" foma-apply-random-upper) :string
  (apply-handle :pointer))
(cffi:defcfun ("apply_random_words" foma-apply-random-words) :string
  (apply-handle :pointer))

(defclass fst-network-definitions ()
  ((handle
    :initform
    (cffi:foreign-funcall "defined_networks_init" :pointer))))

(defclass fst-function-definitions ()
  ((handle
    :initform
    (cffi:foreign-funcall "defined_functions_init" :pointer))))

(defclass fst ()
  ((network-definitions
    :reader fst-network-definitions
    :initform (make-instance 'fst-network-definitions))
   (function-definitions
    :reader fst-function-definitions
    :initform (make-instance 'fst-function-definitions))
   (handle :accessor fst-handle)))

(defmethod initialize-instance :after
    ((fst fst) &key (filename nil filename-supplied-p)
                 (regex nil regex-supplied-p))
  (when (and filename-supplied-p regex-supplied-p)
    (error "Supply either FILENAME or REGEX, but not both."))
  (when filename-supplied-p
    (load-binary-file fst filename))
  (when regex-supplied-p
    (setf (fst-handle fst)
          (foma-fsm-parse-regex regex
                                (fst-network-definitions fst)
                                (fst-function-definitions fst)))
    (unless (fst-handle fst)
      (error "Syntax error in regex."))))
#+nil
(defparameter *fst*
  (make-instance 'fst :filename "/path/to/morphology.bin"))

(defun make-fst (&key filename regex)
  (when (and filename regex)
    (error "Supply either FILENAME or REGEX, but not both."))
  (cond (filename (make-instance 'fst :filename filename))
        (regex (make-instance 'fst :regex regex))
        (t nil)))


(defmethod load-binary-file ((fst fst) filename)
  (setf (slot-value fst 'handle)
        (foma-fsm-read-binary-file filename))
  (unless (slot-value fst 'handle)
    (error "Could not load file ~A." filename))
  fst)
#+nil
(load-binary-file *fst* "/path/to/morphology.bin")

(defun apply% (function fst &optional word)
  (unless (slot-value fst 'handle)
    (error "FST is not defined."))
  (loop with apply-handle = (foma-apply-init (slot-value fst 'handle))
        ;; apply-{upper,lower}-words potentially generate infinite
        ;; number of words, thus limit here:
        for i from 0 below 100
        for output = (if word
                         (funcall function apply-handle word)
                         (funcall function apply-handle))
          then (if word
                   (funcall function apply-handle (cffi:null-pointer))
                   (funcall function apply-handle))
        while output
        collect output into results
        finally (progn
                  (foma-apply-clear apply-handle)
                  (return-from apply% results))))

(defun apply-down (fst word)
  (apply% #'foma-apply-down fst word))
#+nil
(apply-down *fst* "ол+VERB+Act+Neg+Pure+Fut+2pSg+Imp")

(defun apply-up (fst word)
  (apply% #'foma-apply-up fst word))
#+nil
(apply-up *fst* "олма")

(defun apply-lower-words (fst)
  (apply% #'foma-apply-lower-words fst))
#+nil
(print (apply-lower-words *fst*))

(defun apply-upper-words (fst)
  (apply% #'foma-apply-upper-words fst))
#+nil
(print (apply-upper-words *fst*))

(defun apply-words (fst)
  (apply% #'foma-apply-words fst))
#+nil
(print (apply-words *fst*))

(defun apply-random-lower (fst)
  (apply% #'foma-apply-random-lower fst))
#+nil
(print (apply-random-lower *fst*))

(defun apply-random-upper (fst)
  (apply% #'foma-apply-random-upper fst))
#+nil
(print (apply-random-upper *fst*))

(defun apply-random-words (fst)
  (apply% #'foma-apply-random-words fst))
#+nil
(print (apply-random-words *fst*))

