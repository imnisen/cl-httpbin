#|
  This file is a part of cl-httpbin project.
  Copyright (c) 2018 Nisen (imnisen@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-httpbin-test-asd
  (:use :cl :asdf))
(in-package :cl-httpbin-test-asd)

(defsystem cl-httpbin-test
  :author "Nisen"
  :license "BSD"
  :depends-on (:cl-httpbin
               :prove
               :dexador
               :yason
               :drakma
               :chipz)
  :components ((:module "t"
                        :components
                        ((:test-file "cl-httpbin"))))
  :description "Test system for cl-httpbin"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
