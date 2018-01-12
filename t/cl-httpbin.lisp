(in-package :cl-user)
(defpackage cl-httpbin-test
  (:use :cl
        :cl-httpbin
        :prove
        :yason
        :dexador))
(in-package :cl-httpbin-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-httpbin)' in your Lisp.

(plan nil)



(ok (cl-httpbin:start) "Server started ?")

(sleep 0.5)

(multiple-value-bind (body status)
    (dex:get "http://127.0.0.1:5000/ip")
  (is status 200 "/ip status 200 ?")
  (is "127.0.0.1" (gethash "origin" (yason:parse body)) "/ip contains origin -> 127.0.0.1 ?"))

(ok (cl-httpbin:stop) "Server started ?")



(finalize)
