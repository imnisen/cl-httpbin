(in-package :cl-user)
(defpackage cl-httpbin-test
  (:use :cl
        :prove))
(in-package :cl-httpbin-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-httpbin)' in your Lisp.

(plan nil)



(ok (cl-httpbin:start) "Server started ?")

(sleep 0.5)


(defvar +user-agent+ "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18")
(defvar +accept+ "*/*")
(defvar +origin+ "127.0.0.1")
(defvar +host+ "127.0.0.1:5000")
(defvar +status-ok+ 200)
(defvar +x-key1+ "x-key1")
(defvar +x-key2+ "x-key2")
(defvar +x-value1+ "x-value1")
(defvar +x-value2+ "x-value2")
(defvar +x-arg1+ "x-arg1")
(defvar +x-arg2+ "x-arg2")
(defvar +x-v1+ "x-v1")
(defvar +x-v2+ "x-v2")
(defvar +content-k1+ "content-k1")
(defvar +content-k2+ "content-k2")
(defvar +content-v1+ "content-v1")
(defvar +content-v2+ "content-v2")

;; To finish
;; (defmacro uri-to-url (uri &rest args)
;;   (concatenate 'string

;;                )
;;   (let ((url (format nil "http://127.0.0.1~a" ,uri)))
;;     (loop for x on args by #'cddr
;;          (format "~a=~a")))


;;   )



;; ;; html TODO read html and compare
;; (multiple-value-bind (body status)
;;     (dex:get "http://127.0.0.1:5000/html")
;;   (is status 200 "/ip status 200 ?")
;;   (is 1 1))


(subtest "Testing /ip"
  (multiple-value-bind (body status)
      (dex:get (uri-to-url "/ip"))
    (is status +status-ok+)
    (is +origin+ (gethash "origin" (yason:parse body)))))


(subtest "Testing /headers"
  (multiple-value-bind (body status)
      (dex:get (uri-to-url "/headers")
               :headers `(("User-Agent" . ,+user-agent+)
                          ("Accept" . "*/*")
                          (,+x-key1+ . ,+x-value1+)
                          (,+x-key2+ . ,+x-value2+)))
    (format t "~a" body)
    (is status +status-ok+)
    (let* ((body-json (yason:parse body))
           (headers (gethash "headers" body-json)))
      (is t (nth-value 1 (gethash "headers" body-json)))
      (is +host+ (gethash "host" headers))
      (is +user-agent+ (gethash "user-agent" headers))
      (is +accept+ (gethash "accept" headers))
      (is +x-value1+ (gethash +x-key1+ headers))
      (is +x-value2+ (gethash +x-key2+ headers)))))


(subtest "Testing /user-agent"
  (multiple-value-bind (body status)
      (dex:get (uri-to-url  "/user-agent")
               :headers `(("User-Agent" . ,+user-agent+)))
    (is status +status-ok+)
    (let* ((body-json (yason:parse body)))
      (is +user-agent+ (gethash "user-agent" body-json)))))

(subtest "Testing /get"
  (multiple-value-bind (body status)
      (dex:get (format nil "http://127.0.0.1:5000/get?~a=~a&~a=~a" +x-arg1+ +x-v1+ +x-arg2+ +x-v2+)
               :headers `(("User-Agent" . ,+user-agent+)
                          ("Accept" . "*/*")
                          (,+x-key1+ . ,+x-value1+)
                          (,+x-key2+ . ,+x-value2+)))
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is +origin+ (gethash "origin" body-json))
      (is (format nil "http://127.0.0.1:5000/get?~a=~a&~a=~a" +x-arg1+ +x-v1+ +x-arg2+ +x-v2+) (gethash "url" body-json))
      (is +x-v1+ (gethash +x-arg1+ (gethash "args" body-json)))
      (is +x-v2+ (gethash +x-arg2+ (gethash "args" body-json)))
      (is +user-agent+ (gethash "user-agent" (gethash "headers" body-json)))
      (is +accept+ (gethash "accept" (gethash "headers" body-json)))
      (is +x-value1+ (gethash +x-key1+ (gethash "headers" body-json)))
      (is +x-value2+ (gethash +x-key2+ (gethash "headers" body-json))))))


(subtest "Testing /anything"
  (multiple-value-bind (body status)
      (dex:get "http://127.0.0.1:5000/anything")
    (is status 200))
  (multiple-value-bind (body status)
      (dex:post (format nil "http://127.0.0.1:5000/anything?~a=~a&~a=~a" +x-arg1+ +x-v1+ +x-arg2+ +x-v2+)
                :headers `(("User-Agent" . ,+user-agent+)
                           ("Accept" . "*/*")
                           (,+x-key1+ . ,+x-value1+)
                           (,+x-key2+ . ,+x-value2+))
                :content `((,+content-k1+ . ,+content-v1+)
                           (,+content-k2+ . ,+content-v2+)))
    (format t "~a" body)
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is +origin+ (gethash "origin" body-json))
      (is (format nil "http://127.0.0.1:5000/anything?~a=~a&~a=~a" +x-arg1+ +x-v1+ +x-arg2+ +x-v2+) (gethash "url" body-json))
      (is +x-v1+ (gethash +x-arg1+ (gethash "args" body-json)))
      (is +x-v2+ (gethash +x-arg2+ (gethash "args" body-json)))
      (is +user-agent+ (gethash "user-agent" (gethash "headers" body-json)))
      (is +accept+ (gethash "accept" (gethash "headers" body-json)))
      (is +x-value1+ (gethash +x-key1+ (gethash "headers" body-json)))
      (is +x-value2+ (gethash +x-key2+ (gethash "headers" body-json))))))
























(ok (cl-httpbin:stop) "Server Stopped ?")



(finalize)
