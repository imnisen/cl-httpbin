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

(defvar +x-header-key1+ "x-header-key1")
(defvar +x-header-key2+ "x-header-key2")
(defvar +x-header-value1+ "x-header-value1")
(defvar +x-header-value2+ "x-header-value2")

(defvar +x-arg-key1+ "x-arg-key1")
(defvar +x-arg-key2+ "x-arg-key2")
(defvar +x-arg-value1+ "x-arg-value1")
(defvar +x-arg-value2+ "x-arg-value2")

(defvar +x-content-key1+ "x-content-key1")
(defvar +x-content-key2+ "x-content-key2")
(defvar +x-content-value1+ "x-content-value1")
(defvar +x-content-value2+ "x-content-value2")


(defun uri-to-url (uri &rest args)
  (let ((url (format nil "http://127.0.0.1:5000~a" uri))
        (arg-string nil))
    (if args
        (concatenate
         'string
         url
         "?"
         (format nil "~{~a~^&~}"
                 (loop for x on args by #'cddr
                    collect (concatenate
                             'string
                             arg-string
                             (first x)
                             "="
                             (second x)))))
        url)))



;; ;; html TODO read html and compare
;; (multiple-value-bind (body status)
;;     (dex:get "http://127.0.0.1:5000/html")
;;   (is status 200 "/ip status 200 ?")
;;   (is 1 1))


(subtest "Testing /ip"
  (multiple-value-bind (body status)
      (dex:get (uri-to-url "/ip"))
    (format t "~a~&" body)
    (format t "~a~&" status)
    (is status +status-ok+)
    (is +origin+ (gethash "origin" (yason:parse body)))))


(subtest "Testing /headers"
  (multiple-value-bind (body status)
      (dex:get (uri-to-url "/headers")
               :headers `(("User-Agent" . ,+user-agent+)
                          ("Accept" . "*/*")
                          (,+x-header-key1+ . ,+x-header-value1+)
                          (,+x-header-key2+ . ,+x-header-value2+)))
    (is status +status-ok+)
    (let* ((body-json (yason:parse body))
           (headers (gethash "headers" body-json)))
      (is t (nth-value 1 (gethash "headers" body-json)))
      (is +host+ (gethash "host" headers))
      (is +user-agent+ (gethash "user-agent" headers))
      (is +accept+ (gethash "accept" headers))
      (is +x-header-value1+ (gethash +x-header-key1+ headers))
      (is +x-header-value2+ (gethash +x-header-key2+ headers)))))


(subtest "Testing /user-agent"
  (multiple-value-bind (body status)
      (dex:get (uri-to-url  "/user-agent")
               :headers `(("User-Agent" . ,+user-agent+)))
    (is status +status-ok+)
    (let* ((body-json (yason:parse body)))
      (is +user-agent+ (gethash "user-agent" body-json)))))

(subtest "Testing /get"
  (multiple-value-bind (body status)
      (dex:get (uri-to-url "/get" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+)
               :headers `(("User-Agent" . ,+user-agent+)
                          ("Accept" . "*/*")
                          (,+x-header-key1+ . ,+x-header-value1+)
                          (,+x-header-key2+ . ,+x-header-value2+)))
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is +origin+ (gethash "origin" body-json))
      (is (uri-to-url "/get" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+) (gethash "url" body-json))
      (is +x-arg-value1+ (gethash +x-arg-key1+ (gethash "args" body-json)))
      (is +x-arg-value2+ (gethash +x-arg-key2+ (gethash "args" body-json)))
      (is +user-agent+ (gethash "user-agent" (gethash "headers" body-json)))
      (is +accept+ (gethash "accept" (gethash "headers" body-json)))
      (is +x-header-value1+ (gethash +x-header-key1+ (gethash "headers" body-json)))
      (is +x-header-value2+ (gethash +x-header-key2+ (gethash "headers" body-json))))))


(subtest "Testing /anything"
  (multiple-value-bind (body status)
      (dex:get (uri-to-url "/anything"))
    (is status 200))
  (multiple-value-bind (body status)
      (dex:post (uri-to-url "/anything" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+)
                :headers `(("User-Agent" . ,+user-agent+)
                           ("Accept" . "*/*")
                           (,+x-header-key1+ . ,+x-header-value1+)
                           (,+x-header-key2+ . ,+x-header-value2+))
                :content `((,+x-content-key1+ . ,+x-content-value1+)
                           (,+x-content-key2+ . ,+x-content-value2+)))
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is +origin+ (gethash "origin" body-json))
      (is (uri-to-url "/anything" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+) (gethash "url" body-json))
      (is +x-arg-value1+ (gethash +x-arg-key1+ (gethash "args" body-json)))
      (is +x-arg-value2+ (gethash +x-arg-key2+ (gethash "args" body-json)))
      (is +user-agent+ (gethash "user-agent" (gethash "headers" body-json)))
      (is +accept+ (gethash "accept" (gethash "headers" body-json)))
      (is +x-header-value1+ (gethash +x-header-key1+ (gethash "headers" body-json)))
      (is +x-header-value2+ (gethash +x-header-key2+ (gethash "headers" body-json)))
      (is "application/x-www-form-urlencoded" (gethash "content-type" (gethash "headers" body-json)))
      (is "post" (gethash "method" body-json))
      (is +x-content-value1+ (gethash +x-content-key1+ (gethash "form" body-json)))
      (is +x-content-value2+ (gethash +x-content-key2+ (gethash "form" body-json)))
      ))
  (multiple-value-bind (body status)
      (dex:put (uri-to-url "/anything" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+)
               :headers `(("User-Agent" . ,+user-agent+)
                          ("Accept" . "*/*")
                          (,+x-header-key1+ . ,+x-header-value1+)
                          (,+x-header-key2+ . ,+x-header-value2+))
               :content `((,+x-content-key1+ . ,+x-content-value1+)
                          (,+x-content-key2+ . ,+x-content-value2+)))
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is "put" (gethash "method" body-json))))
  (multiple-value-bind (body status)
      (dex:delete (uri-to-url "/anything" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+)
                  :headers `(("User-Agent" . ,+user-agent+)
                             ("Accept" . "*/*")
                             (,+x-header-key1+ . ,+x-header-value1+)
                             (,+x-header-key2+ . ,+x-header-value2+))                  )
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is "delete" (gethash "method" body-json)))) )

(subtest "Testing /post"
  (multiple-value-bind (body status)
      (dex:post (uri-to-url "/post" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+)
                :headers `(("User-Agent" . ,+user-agent+)
                           ("Accept" . "*/*")
                           (,+x-header-key1+ . ,+x-header-value1+)
                           (,+x-header-key2+ . ,+x-header-value2+))
                :content `((,+x-content-key1+ . ,+x-content-value1+)
                           (,+x-content-key2+ . ,+x-content-value2+)))
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is +origin+ (gethash "origin" body-json))
      (is (uri-to-url "/post" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+) (gethash "url" body-json))
      (is +x-arg-value1+ (gethash +x-arg-key1+ (gethash "args" body-json)))
      (is +x-arg-value2+ (gethash +x-arg-key2+ (gethash "args" body-json)))
      (is +user-agent+ (gethash "user-agent" (gethash "headers" body-json)))
      (is +accept+ (gethash "accept" (gethash "headers" body-json)))
      (is +x-header-value1+ (gethash +x-header-key1+ (gethash "headers" body-json)))
      (is +x-header-value2+ (gethash +x-header-key2+ (gethash "headers" body-json)))
      (is "application/x-www-form-urlencoded" (gethash "content-type" (gethash "headers" body-json)))
      (is +x-content-value1+ (gethash +x-content-key1+ (gethash "form" body-json)))
      (is +x-content-value2+ (gethash +x-content-key2+ (gethash "form" body-json)))
      )))

(subtest "Testing /put"
  (multiple-value-bind (body status)
      (dex:put (uri-to-url "/put" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+)
               :headers `(("User-Agent" . ,+user-agent+)
                          ("Accept" . "*/*")
                          (,+x-header-key1+ . ,+x-header-value1+)
                          (,+x-header-key2+ . ,+x-header-value2+))
               :content `((,+x-content-key1+ . ,+x-content-value1+)
                          (,+x-content-key2+ . ,+x-content-value2+)))
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is +origin+ (gethash "origin" body-json))
      (is (uri-to-url "/put" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+) (gethash "url" body-json))
      (is +x-arg-value1+ (gethash +x-arg-key1+ (gethash "args" body-json)))
      (is +x-arg-value2+ (gethash +x-arg-key2+ (gethash "args" body-json)))
      (is +user-agent+ (gethash "user-agent" (gethash "headers" body-json)))
      (is +accept+ (gethash "accept" (gethash "headers" body-json)))
      (is +x-header-value1+ (gethash +x-header-key1+ (gethash "headers" body-json)))
      (is +x-header-value2+ (gethash +x-header-key2+ (gethash "headers" body-json)))
      (is "application/x-www-form-urlencoded" (gethash "content-type" (gethash "headers" body-json)))
      (is +x-content-value1+ (gethash +x-content-key1+ (gethash "form" body-json)))
      (is +x-content-value2+ (gethash +x-content-key2+ (gethash "form" body-json)))
      )))

(subtest "Testing /patch"
  (multiple-value-bind (body status)
      (dex:patch (uri-to-url "/patch" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+)
                 :headers `(("User-Agent" . ,+user-agent+)
                            ("Accept" . "*/*")
                            (,+x-header-key1+ . ,+x-header-value1+)
                            (,+x-header-key2+ . ,+x-header-value2+))
                 :content `((,+x-content-key1+ . ,+x-content-value1+)
                            (,+x-content-key2+ . ,+x-content-value2+)))
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is +origin+ (gethash "origin" body-json))
      (is (uri-to-url "/patch" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+) (gethash "url" body-json))
      (is +x-arg-value1+ (gethash +x-arg-key1+ (gethash "args" body-json)))
      (is +x-arg-value2+ (gethash +x-arg-key2+ (gethash "args" body-json)))
      (is +user-agent+ (gethash "user-agent" (gethash "headers" body-json)))
      (is +accept+ (gethash "accept" (gethash "headers" body-json)))
      (is +x-header-value1+ (gethash +x-header-key1+ (gethash "headers" body-json)))
      (is +x-header-value2+ (gethash +x-header-key2+ (gethash "headers" body-json)))
      (is "application/x-www-form-urlencoded" (gethash "content-type" (gethash "headers" body-json)))
      (is +x-content-value1+ (gethash +x-content-key1+ (gethash "form" body-json)))
      (is +x-content-value2+ (gethash +x-content-key2+ (gethash "form" body-json)))
      )))

(subtest "Testing /delete"
  (multiple-value-bind (body status)
      (dex:delete (uri-to-url "/delete" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+)
                  :headers `(("User-Agent" . ,+user-agent+)
                             ("Accept" . "*/*")
                             (,+x-header-key1+ . ,+x-header-value1+)
                             (,+x-header-key2+ . ,+x-header-value2+)))
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is +origin+ (gethash "origin" body-json))
      (is (uri-to-url "/delete" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+) (gethash "url" body-json))
      (is +x-arg-value1+ (gethash +x-arg-key1+ (gethash "args" body-json)))
      (is +x-arg-value2+ (gethash +x-arg-key2+ (gethash "args" body-json)))
      (is +user-agent+ (gethash "user-agent" (gethash "headers" body-json)))
      (is +accept+ (gethash "accept" (gethash "headers" body-json)))
      (is +x-header-value1+ (gethash +x-header-key1+ (gethash "headers" body-json)))
      (is +x-header-value2+ (gethash +x-header-key2+ (gethash "headers" body-json)))
      )))



(subtest "Testing /gzip"
  (multiple-value-bind (body)
      (drakma:http-request (uri-to-url "/gzip")
                           :method :get
                           :user-agent +user-agent+)
    (labels ((transfer-data (data)
               (make-array (length data)
                           :element-type '(unsigned-byte 8)
                           :initial-contents data))
             (ungzip (data)
               (chipz:decompress nil 'chipz:gzip (transfer-data data))))
      (let* ((body-string (flexi-streams:octets-to-string (ungzip body)))
             (body-json (yason:parse body-string)))
        (is +origin+ (gethash "origin" body-json))
        (is "get" (gethash "method" body-json))
        (is t  (gethash "gziped" body-json))
        (is +user-agent+ (gethash "user-agent" (gethash "headers" body-json)))
        ))
    ))

(subtest "Testing /deflate"
  (multiple-value-bind (body)
      (drakma:http-request (uri-to-url "/deflate")
                           :method :get
                           :user-agent +user-agent+)
    (labels ((transfer-data (data)
               (make-array (length data)
                           :element-type '(unsigned-byte 8)
                           :initial-contents data))
             (undeflate (data)
               (chipz:decompress nil 'chipz:deflate (transfer-data data))))
      (let* ((body-string (flexi-streams:octets-to-string (undeflate body)))
             (body-json (yason:parse body-string)))
        (is +origin+ (gethash "origin" body-json))
        (is "get" (gethash "method" body-json))
        (is t  (gethash "deflated" body-json))
        (is +user-agent+ (gethash "user-agent" (gethash "headers" body-json)))
        ))
    ))


(ok (cl-httpbin:stop) "Server Stopped ?")



(finalize)
