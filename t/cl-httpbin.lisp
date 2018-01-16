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




;; ;; html TODO read html and compare
;; (multiple-value-bind (body status)
;;     (dex:get "http://127.0.0.1:5000/html")
;;   (is status 200 "/ip status 200 ?")
;;   (is 1 1))


(multiple-value-bind (body status)
    (dex:get "http://127.0.0.1:5000/ip")
  (is status 200 "status 200 ?")
  (is "127.0.0.1" (gethash "origin" (yason:parse body)) "/ip: origin->127.0.0.1 ?"))



(multiple-value-bind (body status)
    (dex:get "http://127.0.0.1:5000/headers"
             :headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18")
                        ("Accept" . "*/*")
                        ("x-key1" . "value1")
                        ("x-key2" . "value2")))
  (format t "~a" body)
  (is status 200 "status 200 ?")
  (let* ((body-json (yason:parse body))
         (headers (gethash "headers" body-json)))
    (is t (nth-value 1 (gethash "headers" body-json)) "/headers: have headers")
    (is "127.0.0.1:5000" (gethash "host" headers) "/headers: headers->host->headers")
    (is "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18" (gethash "user-agent" headers) "/headers: headers->user-agent->long-words")
    (is "*/*" (gethash "accept" headers) "/headers: headers->accept-*/*")
    (is "value1" (gethash "x-key1" headers) "/headers: headers->x-key1->value1")
    (is "value2" (gethash "x-key2" headers) "/headers: headers->x-key2->value2")
    ))


(multiple-value-bind (body status)
    (dex:get "http://127.0.0.1:5000/user-agent"
             :headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18")))
  (format t "~a" body)
  (is status 200 "status 200 ?")
  (let* ((body-json (yason:parse body)))
    (is "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18" (gethash "user-agent" body-json) "/user-agent: headers->user-agent->long-words")))


;; To finish
;; (multiple-value-bind (body status)
;;     (dex:get "http://127.0.0.1:5000/get?arg1=v1&arg2=v2"
;;              :headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18")
;;                         ("Accept" . "*/*")
;;                         ("x-key1" . "value1")
;;                         ("x-key2" . "value2")))
;;   (format t "~a" body)
;;   (is status 200 "status 200 ?")
;;   (let* ((body-json (yason:parse body)))
;;     (is "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18" (gethash "user-agent" body-json) "/get headers->user-agent->long-words")
;;     (is "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18" (gethash "user-agent" body-json) "/get headers->user-agent->long-words")
;;     (is "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18" (gethash "user-agent" body-json) "/get headers->user-agent->long-words")
;;     (is "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18" (gethash "user-agent" body-json) "/get headers->user-agent->long-words")
;;     (is "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18" (gethash "user-agent" body-json) "/get headers->user-agent->long-words")
;;     ))
























(ok (cl-httpbin:stop) "Server Stopped ?")



(finalize)
