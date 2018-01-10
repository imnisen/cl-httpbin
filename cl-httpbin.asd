#|
This file is a part of cl-httpbin project.
Copyright (c) 2018 Nisen (imnisen@gmail.com)
|#

#|
Author: Nisen (imnisen@gmail.com)
|#

;;; (ql:quickload '(ningle clack cl-json flexi-streams salza2 s-base64 snakes cl-ppcre parse-float))

(in-package :cl-user)
(defpackage cl-httpbin-asd
  (:use :cl :asdf))
(in-package :cl-httpbin-asd)

(defsystem cl-httpbin
  :version "0.1"
  :author "Nisen"
  :license "BSD"
  :depends-on (:cl-json
               :flexi-streams
               :salza2
               :s-base64
               :snakes
               :cl-ppcre
               :parse-float
               :clack
               :ningle)
  :components ((:module "src"
                        :components
                        ((:file "utils" )
                         (:file "cl-httpbin" :depends-on ("utils")))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-httpbin-test))))
