(in-package :cl-user)
(defpackage cl-httpbin
  (:use :cl
        :cl-httpbin-utils)
  (:export :start
           :stop))
(in-package :cl-httpbin)

;;;
;;; Route handlers
;;;

;; ;;; 手动import cl-httpbin.utils  里定义的symbol
;; (let ((pack (find-package :cl-httpbin.utils)))
;;   (do-all-symbols (sym pack)
;;     (when (eql (symbol-package sym) pack)
;;       (export sym))))

(defvar *app* (make-instance 'ningle:<app>))

(setf (ningle:route *app* "/html")
      #'(lambda (params)
          (declare (ignore params))
          (set-response-content-type "text/html")
          (read-file-at-once "/Users/nisen/Projects/cl-httpbin/templates/moby.html"
                             :element-type 'character
                             :external-format :utf-8)))

;; Returns Origin IP
(setf (ningle:route *app* "/ip")
      #'(lambda (params)
          (declare (ignore params))
          (set-response-content-type "application/json")
          (let ((h (make-hash-table)))
            (setf (gethash "origin" h)
                  (gethash "x-forwarded-for" (get-request-headers) (get-request-remote-addr)))
            (json:encode-json-to-string h))))

;; Returns HTTP HEADERS
(setf (ningle:route *app* "/headers")
      #'(lambda (params)
          (declare (ignore params))
          (set-response-content-type "application/json")
          (let ((h (make-hash-table)))
            (setf (gethash "headers" h)
                  (get-request-headers))
            (json:encode-json-to-string h))))

;; Returns User-Agent.
(setf (ningle:route *app* "/user-agent")
      #'(lambda (params)
          (declare (ignore params))
          (set-response-content-type "application/json")
          (let ((h (make-hash-table)))
            (setf (gethash "user-agent" h)
                  (get-request-user-agent))
            (json:encode-json-to-string h))))

;; Returns GET Data.
(setf (ningle:route *app* "/get" :method :GET)
      #'(lambda (params)
          (declare (ignore params))
          (set-response-content-type "application/json")
          (get-json-response  "headers" "args" "origin" "url")))



(setf (ningle:route *app* "/anything" :method '(:GET :POST :PUT :DELETE :PATCH :TRACE))
      #'(lambda (params)
          (declare (ignore params))
          (set-response-content-type "application/json")
          (get-json-response "url" "args" "headers" "origin" "method" "form" "data" "files" "json")))

(setf (ningle:route *app* "/post" :method '(:POST))
      #'(lambda (params)
          (declare (ignore params))
          (set-response-content-type "application/json")
          (get-json-response "url" "args" "form" "data" "origin" "headers" "files" "json")))

(setf (ningle:route *app* "/put" :method '(:PUT))
      #'(lambda (params)
          (declare (ignore params))
          (set-response-content-type "application/json")
          (get-json-response "url" "args" "form" "data" "origin" "headers" "files" "json")))

(setf (ningle:route *app* "/patch" :method '(:PATCH))
      #'(lambda (params)
          (declare (ignore params))
          (set-response-content-type "application/json")
          (get-json-response "url" "args" "form" "data" "origin" "headers" "files" "json")))


(setf (ningle:route *app* "/delete" :method '(:delete))
      #'(lambda (params)
          (declare (ignore params))
          (set-response-content-type "application/json")
          (get-json-response "url" "args" "form" "data" "origin" "headers" "files" "json")))


(setf (ningle:route *app* "/gzip")
      #'(lambda (params)
          (declare (ignore params))
          (set-response-content-type "application/json")
          (set-response-content-encoding "gzip")
          (gzip-it (get-json-response  "origin" "headers" "method" '(gziped t))))) ; TODO 这里传参数的方式有些不太美观，考虑用宏改写

(setf (ningle:route *app* "/deflate")
      #'(lambda (params)
          (declare (ignore params))
          (set-response-content-type "application/json")
          (set-response-content-encoding "deflate")
          (deflate-it (get-json-response  "origin" "headers" "method" '(deflated t)))))

;; brotli 压缩方式暂时没找到合适的库 TODO
;; (setf (ningle:route *app* "/brotli")
;;       #'(lambda (params)
;;           (declare (ignore params))
;;           (set-response-content-type "application/json")
;;           (set-response-content-encoding "brotli")
;;           (brotli-it (get-json-response  "origin" "headers" "method" '(brotli t)))))



(setf (ningle:route *app* "/redirect/:n")
      #'(lambda (params)
          (let ((n (make-sure-number (cdr (assoc :n params))))
                (absolute? (string-equal "true" (gethash "absolute" (get-request-query-parameters)))))
            (set-response-status 302)
            (set-response-location
             (if (= 1 n)
                 (url-for "view_get" :external absolute?)
                 (url-for (format nil "~a_redirect_n_times" (if absolute? "absolute" "relative"))
                          :params (list (list "n" (- n 1))) ; 这里传参十分丑陋 TODO
                          :external absolute?)))
            nil)))

;; Status code 需要处理
(setf (ningle:route *app* "/redirect-to" :method '(:GET :POST :PUT :DELETE :PATCH :TRACE))
      #'(lambda (params)
          (declare (ignore params))
          (let* ((request-args (get-request-query-parameters))
                 (status-code-exsit? (nth-value 1 (gethash "status_code" request-args)))
                 (status-code
                  (if status-code-exsit?
                      (let ((status-code-arg (make-sure-number (nth-value 0 (gethash "status_code" request-args)))))
                        (if (and (>= status-code-arg 300) (< status-code-arg 400))
                            status-code-arg
                            302))
                      302)
                   )
                 (url (gethash "url" request-args)))
            (set-response-status status-code)
            (set-response-location url)
            nil)))



;; TODO 对n进行限制
(setf (ningle:route *app* "/relative-redirect/:n")
      #'(lambda (params)
          (let ((n (make-sure-number (cdr (assoc :n params)))))
            (set-response-status 302)
            (set-response-location
             (if (= 1 n)
                 (url-for "view_get")
                 (url-for "relative_redirect_n_times"
                          :params (list (list "n" (- n 1))) ; 这里传参十分丑陋 TODO
                          )))
            nil)))

(setf (ningle:route *app* "/absolute-redirect/:n")
      #'(lambda (params)
          (let ((n (make-sure-number (cdr (assoc :n params)))))
            (set-response-status 302)
            (set-response-location
             (if (= 1 n)
                 (url-for "view_get" :external t)
                 (url-for "absolute_redirect_n_times"
                          :params (list (list "n" (- n 1)))  ; 这里传参十分丑陋 TODO
                          :external t)))
            nil)))

;; ;; httpbin中是使用generator实现的，在这里我还不知道怎么实现同样的东西，就先这么做
;; (setf (ningle:route *app* "/stream/:n")
;;       #'(lambda (params)
;;           (let* ((n (make-sure-number (cdr (assoc :n params))))
;;                  (n (min n 100))
;;                  (res '()))
;;             (set-response-content-type "application/json")
;;             (loop for i below n
;;                do (setf res (append res (make-sure-list (get-json-response "url" "args" "headers" "origin" (list "id" i))))))
;;             (format nil "~{~a~%~}" res)
;;             )))


(setf (ningle:route *app* "/stream/:n")
      #'(lambda (params)
          (set-response-content-type "application/json")
          (let ((n (make-sure-number (cdr (assoc :n params))))
                (hash-response (get-hash-response "url" "args" "headers" "origin")))
            (flet ((generate-stream () (snakes:with-yield
                                           (loop for i below n do
                                                (snakes:yield
                                                 (json:encode-json-to-string
                                                  (progn
                                                    (setf (gethash "i" hash-response) i)
                                                    hash-response)
                                                  ))))))
              ;; 这里本来打算直接将生成器generate-stream返回的，但不知道是什么原因是clack还是哪个部分不能处理生成器返回，所以这里做完生成器后又手动调用将结果都取出来拼成字符串返回了
              (format nil "~{~a~%~}" (loop with g = (generate-stream)
                                        for val = (funcall g)
                                        until (eq 'generator-stop val)
                                        collecting val))))))



;; 这里的错误处理让我很难受，有没有什么好办法
(setf (ningle:route *app* "/status/:codes")
      #'(lambda (params)
          (block outer
            (let ((codes (cdr (assoc :codes params))))
              (if (not (search "," codes))
                  (status-code (handler-case
                                   (string-to-integer codes)
                                 (PARSE-ERROR ()
                                   (return-from outer (progn
                                                        (set-response-status 400)
                                                        "Invalid status code")))))
                  (status-code (weighted-choice (handler-case
                                                    (loop for code-pro in (cl-ppcre:split "," codes)
                                                       collecting (list (string-to-integer (first (cl-ppcre:split ":" code-pro)))
                                                                        (string-to-float (or (second (cl-ppcre:split ":" code-pro)) "1"))))
                                                  (PARSE-ERROR ()
                                                    (return-from outer (progn
                                                                         (set-response-status 400)
                                                                         "Invalid status code")))))))))))





;; 启动必须； 待修改 TODO

(defparameter *handler-to-url*
  (make-hash-table :test 'equal))

;; 这个地方得重构，先这么快速来
;; 根据handler name 获取 路由路径
(defun url-for (route-name &key params external)
  (let ((route-path (if (null params)
                        (gethash route-name *handler-to-url*)
                        (replace-route-with-args (gethash route-name *handler-to-url*) params))))
    (if (null external)
        route-path
        (concatenate 'string
                     (get-request-scheme)
                     "://"
                     (get-request-server-name)
                     ":"
                     (write-to-string (get-request-server-port))
                     route-path))))

(defun init ()
  (setf (gethash "view_get" *handler-to-url*) "/get")
  (setf (gethash "relative_redirect_n_times"  *handler-to-url*) "/relative-redirect/:n")
  (setf (gethash "absolute_redirect_n_times"  *handler-to-url*) "/absolute-redirect/:n")
  (setf (gethash "redirect_n_times"  *handler-to-url*) "/redirect/:n"))

(defvar *server* nil)

(defun start (&key (port 5000) )
  (init)
  (if *server*
      nil
      (progn (setf *server*
                   (clack:clackup *app* :port port))
             t)))

(defun stop ()
  (if *server*
      (progn (clack:stop *server*) (setf *server* nil) t)
      nil))
