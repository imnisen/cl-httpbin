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

(defvar +ASCII-ART+ "
    -=[ teapot ]=-

       _...._
     .'  _ _ `.
    | .'` ^ `. _,
    \_;`---`|//
      |       ;/
      \_     _/
        `\"\"\"`
")



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


(subtest "Testing /redirect-to"
  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/redirect-to" "status_code" "307" "url" "http://www.163.com") :max-redirects 0)
    (is status 307)
    (is "http://www.163.com" (gethash "location" response-header))
    )
  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/redirect-to" "status_code" "200" "url" "http://www.163.com") :max-redirects 0)
    (is status 302)
    (is "http://www.163.com" (gethash "location" response-header))
    )
  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/redirect-to" "status_code" "200" "url" "http://www.163.com"))
    (is status 200)
    (like body "网易")
    ))

(subtest "Testing /redirect/:n"
  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/redirect/4" "absolute" "true") :max-redirects 0)
    (is status 302)
    (is "http://127.0.0.1:5000/absolute-redirect/3" (gethash "location" response-header))
    )
  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/redirect/4" "absolute" "true") :max-redirects 1)
    (is status 302)
    (is "http://127.0.0.1:5000/absolute-redirect/2" (gethash "location" response-header))
    )
  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/redirect/4" "absolute" "true") :max-redirects 2)
    (is status 302)
    (is "http://127.0.0.1:5000/absolute-redirect/1" (gethash "location" response-header))
    )

  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/redirect/4" "absolute" "true") :max-redirects 3)
    (is status 302)
    (is  "http://127.0.0.1:5000/get" (gethash "location" response-header))
    )
  (multiple-value-bind (body status)
      (dex:get (uri-to-url "/redirect/4" "absolute" "true"))
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is +origin+ (gethash "origin" body-json))
      (is (uri-to-url "/get") (gethash "url" body-json)))
    )


  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/redirect/4") :max-redirects 0)
    (is status 302)
    (is "/relative-redirect/3" (gethash "location" response-header))
    )
  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/redirect/4") :max-redirects 1)
    (is status 302)
    (is "/relative-redirect/2" (gethash "location" response-header))
    )
  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/redirect/4") :max-redirects 2)
    (is status 302)
    (is "/relative-redirect/1" (gethash "location" response-header))
    )

  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/redirect/4") :max-redirects 3)
    (is status 302)
    (is  "/get" (gethash "location" response-header))
    )
  (multiple-value-bind (body status)
      (dex:get (uri-to-url "/redirect/4"))
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is +origin+ (gethash "origin" body-json))
      (is (uri-to-url "/get") (gethash "url" body-json)))
    )
  )


(subtest "Testing /relative-redirect/:n"
  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/relative-redirect/2") :max-redirects 0)
    (is status 302)
    (is "/relative-redirect/1" (gethash "location" response-header))
    )
  (multiple-value-bind (body status)
      (dex:get (uri-to-url "/relative-redirect/2"))
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is +origin+ (gethash "origin" body-json))
      (is (uri-to-url "/get") (gethash "url" body-json)))
    )
  )

(subtest "Testing /absolute-redirect/:n"
  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/absolute-redirect/2") :max-redirects 0)
    (is status 302)
    (is "http://127.0.0.1:5000/absolute-redirect/1" (gethash "location" response-header))
    )
  (multiple-value-bind (body status)
      (dex:get (uri-to-url "/absolute-redirect/2"))
    (is status 200)
    (let* ((body-json (yason:parse body)))
      (is +origin+ (gethash "origin" body-json))
      (is (uri-to-url "/get") (gethash "url" body-json)))
    )
  )


(subtest "Testing /status/:codes"
  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/status/200"))
    (is status 200))

  (multiple-value-bind (body status response-header)
      (dex:get (uri-to-url "/status/302") :max-redirects 0)
    (is status 302)
    (is "/redirect/1" (gethash "location" response-header)))


  ;; dex when response code 4xx 5xx, it will rasie error, which make case complicated, so use drakma
  (multiple-value-bind (body status response-header)
      (drakma:http-request (uri-to-url "/status/401"))
    (is status 401)
    (is "Basic realm=\"Fake Realm\"" (cdr (assoc :www-authenticate response-header)))
    )

  (multiple-value-bind (body status response-header)
      (drakma:http-request (uri-to-url "/status/402"))
    (is body "Fuck you, pay me!")
    (is status 402)
    (is "http://vimeo.com/22053820" (cdr (assoc :x-more-info response-header)))
    )

  (multiple-value-bind (body status response-header)
      (drakma:http-request (uri-to-url "/status/406"))
    (let* ((body-json (yason:parse (flexi-streams:octets-to-string body))))
      (is "Client did not request a supported media type." (gethash "message" body-json))
      (is '("image/webp" "image/svg+xml" "image/jpeg" "image/png" "image/*")
          (gethash "accept" body-json)))
    (is status 406)
    (is "application/json" (cdr (assoc :content-type response-header)))
    )

  (multiple-value-bind (body status response-header)
      (drakma:http-request (uri-to-url "/status/407"))
    (is status 407)
    (is "Basic realm=\"Fake Realm\"" (cdr (assoc :proxy-authenticate response-header)))
    )

  (multiple-value-bind (body status response-header)
      (drakma:http-request (uri-to-url "/status/418"))
    (is body +ASCII-ART+)
    (is status 418)
    (is "http://tools.ietf.org/html/rfc2324" (cdr (assoc :x-more-info response-header)))
    )

  (multiple-value-bind (body status response-header)
      (drakma:http-request (uri-to-url "/status/200:0,201:1"))
    (is status 201)
    )


  )












(ok (cl-httpbin:stop) "Server Stopped ?")



(finalize)


;; Log headers to the REPL output stream
;; In some of the following examples, the headers exchanged between Drakma and the HTTP server should be shown, for illustration purposes. This can be achieved like so:

;; ? (setf drakma:*header-stream* *standard-output*)
;; #<SYNONYM-STREAM to *TERMINAL-IO* #x3020006AC7DD>

;; (defmacro test ()
;;   `(asdf:test-system :cl-httpbin))

;; (defmacro start ()
;;   `(cl-httpbin:start))

;; (defmacro stop ()
;;   `(cl-httpbin:stop))

;; (defun print-hash (h)
;;   (format t "----hashtable begin-----")
;;   (loop for k being the hash-keys of h
;;      using (hash-value v)
;;      do (format t "~& ~a => ~a~&" k v))
;;   (format t "----hashtable end -----"))
