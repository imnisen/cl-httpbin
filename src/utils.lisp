#|
This file contains some utils in cl-httpbin
|#
(in-package :cl-user)
(defpackage cl-httpbin-utils
  (:use :cl)
  (:export :PRINT-AND-RETURN
           :PRINT-HASH-AND-RETURN
           :PRINT-HASH
           :PRINT-CONS
           :HASH-TO-LIST
           :HASH-EMPTYP
           :LIST-TO-1D-ARRAY
           :NULL-TO-STRING
           :NULL-TO-HASH
           :FIRST-OR-ALL-LIST
           :GROUP-ALIST-TO-HASH
           :STRING-TO-INTEGER
           :STRING-TO-FLOAT
           :NUMBER-TO-STRING
           :MAKE-SURE-LIST
           :MAKE-SURE-STRING
           :MAKE-SURE-NUMBER
           :MAKE-SURE-FLOAT
           :SECOND-ELEMENT
           :READ-FILE-AT-ONCE
           :GZIP-IT
           :DEFLATE-IT
           :BISECT
           :WEIGHTED-CHOICE
           :MAKEHASH
           :SETHASH
           :GET-REQUEST-ENV
           :GET-REQUEST-ENV-TO-HASHTABLE
           :GET-REQUEST-CONTENT-LENGTH
           :GET-REQUEST-REMOTE-ADDR
           :GET-REQUEST-USER-AGENT
           :GET-REQUEST-QUERY-PARAMETERS
           :GET-REQUEST-SERVER-NAME
           :GET-REQUEST-SERVER-PORT
           :GET-REQUEST-URI
           :GET-REQUEST-SCHEME
           :GET-REQUEST-CONTENT-TYPE
           :GET-REQUEST-URL
           :GET-REQUEST-ORIGIN
           :GET-REQUEST-HEADERS
           :BODY-PARAMETERS-TO-HASHTABLE
           :FILTER-FORM-PARAMETERS
           :GET-REQUEST-FORM
           :GET-REQUEST-METHOD
           :GET-REQUEST-DATA
           :GET-REQUEST-JSON
           :GET-REQUEST-BODY-PARAMETERS
           :FILTER-FILES-PARAMETERS
           :PRODUCE-RETURN-CONTENT
           :GET-REQUEST-FILES
           :SET-RESPONSE-CONTENT-TYPE
           :SET-RESPONSE-CONTENT-ENCODING
           :SET-RESPONSE-STATUS
           :SET-RESPONSE-LOCATION
           :SET-RESPONSE-HEADERS
           :STATUS-CODE
           :GET-JSON-RESPONSE
           :GET-HASH-RESPONSE
           :REPLACE-ROUTE-WITH-ARGS
           :REPLACE-ALL
           ))
(in-package :cl-httpbin-utils)





;;; for some macro to compile first
;;; need solve latter

(defmacro make-sure-list (l)
  `(if (typep ,l 'list)
       ,l
       (list ,l)))


;;;
;;; debug utils 开发工具
;;;

(defun print-and-return (x)
  (format t "~&---print-and-return start----~&~a~&---print-and-return end----~&" x)
  x)

(defun print-hash-and-return (h)
  (print-hash h)
  h)

(defun print-hash (h)
  (format t "----hashtable begin-----")
  (loop for k being the hash-keys of h
     using (hash-value v)
     do (format t "~& ~a => ~a~&" k v))
  (format t "----hashtable end -----"))

(defun print-cons (a-cons)
  (loop for x in a-cons
     do (format t "~&~a~&" x))
  a-cons)



;;;
;;; constants 常量
;;;

(defvar empty-hash (make-hash-table))


(defvar empty-string "")

(defvar REDIRECT-LOACTION "/redirect/1")

(defvar ACCEPT-MEDIA-TYPES '("image/webp"
                             "image/svg+xml"
                             "image/jpeg"
                             "image/png"
                             "image/*"))

;; TODO common lisp 多行string里想要写入引号怎么处理，使用转义\"后，字符串不一样啊
(defvar ASCII-ART "
    -=[ teapot ]=-

       _...._
     .'  _ _ `.
    | .'` ^ `. _,
    \_;`---`|//
      |       ;/
      \_     _/
        `\"\"\"`
")


;;;
;;; type convert utils  类型转换工具
;;;

(defun hash-to-list (hash)
  "将hashtable里的元素拿出来组成列表"
  (loop for key being the hash-keys of hash using (hash-value value)
     collect key collect value))

;;; hashtable 相关
(defun hash-emptyp (hash)
  (> (hash-table-count hash) 0))


(defun list-to-1d-array (a-list)
  "列表转化成1维数组"
  (make-array (length a-list)
              :initial-contents a-list))

(defmacro null-to-string (&optional (n nil))
  `(if (null ,n) empty-string ,n))

(defmacro null-to-hash (&optional (n nil))
  `(if (null ,n) empty-hash ,n))


(defmacro first-or-all-list (l)
  `(if (= 1 (length ,l))
       (first ,l)
       ,l))

;; 将asscociated list 转化成hash table, 相同
;; 每个query-parameters是一个cons
(defun group-alist-to-hash (a-list)
  (let ((h (make-hash-table :test 'equal)))
    (dolist (pair a-list)
      (setf (gethash (car pair) h)
            (first-or-all-list
             (append (make-sure-list (gethash (car pair) h))
                     (make-sure-list (null-to-string (cdr pair)))))))
    h))

(defun string-to-integer (a-string)
  "将字符串转成整形数字"
  (nth-value 0 (parse-integer a-string :radix 10 :junk-allowed nil)))

(defun string-to-float (a-string)
  "将字符串转成整形数字"
  (nth-value 0 (parse-float:parse-float a-string :radix 10 :junk-allowed nil)))


(defun number-to-string (a-number)
  "将数字转化成字符串"
  (write-to-string a-number :base 10))



;;;
;;; type confirm 类型确认
;;;




(defun make-sure-string (x)
  (if (consp x)
      (make-sure-string (eval x))
      (format nil "~a" x)))

(defun make-sure-number (x)
  (cond ((stringp x) (parse-integer x :junk-allowed t))
        ((numberp x) x)
        (t nil)))

(defun make-sure-float (x)
  (cond ((stringp x) (parse-float:parse-float x :junk-allowed t))
        ((floatp x) x)
        ((integerp x) (float x))
        (t nil)))


;;;
;;; easy accesser 简便获取的方法
;;;

(defun second-element (c)
  "获取list或者cons的第2个元素"
  (if (typep (cdr c) 'list)
      (cadr c)
      (cdr c)))


;;;
;;; Others 其它工具
;;;

;; 一次性读取文件内容返回
(defun read-file-at-once (filespec &rest open-args)
  (with-open-stream (stream (apply #'open filespec open-args))
    (let* ((buffer
            (make-array (file-length stream)
                        :element-type (stream-element-type stream)
                        :fill-pointer t))
           (position (read-sequence buffer stream)))
      (setf (fill-pointer buffer) position)
      buffer)))

(defun gzip-it (content)
  "gizp压缩"
  (salza2:compress-data (flexi-streams:string-to-octets content) 'salza2:gzip-compressor))

(defun deflate-it (content)
  "deflate压缩"
  (salza2:compress-data (flexi-streams:string-to-octets content) 'salza2:deflate-compressor))

(defun bisect (a x)
  "在由小到大排序好的数字列表a里插入数字x 返回x 插入的位置"
  (let ((l (length a)))
    (do ((lo 0)
         (hi l))
        ((>= lo hi) lo)
      (let ((mid (nth-value 0 (floor (/ (+ lo hi) 2)))))
        (if (< x (nth mid a))
            (setf hi mid)
            (setf lo (+ mid 1)))))))

;; 传入的值像这种形式：(("val1" 5) ("val2" 0.3) ("val3" 1)), 根据每项的第2个值（代表概率），来随机返回一个值
(defun weighted-choice (choice)
  (let* ((values (mapcar #'first choice ))
         (weights (mapcar #'second choice))
         (cum-weights (loop
                         for x in weights
                         sum x into s
                         collect s))
         (total (car (last cum-weights)))
         (x (random (make-sure-float total)))
         (i (bisect cum-weights x)))
    (nth i values)))


;; demo:
;; (my-makehash :data "Fuck you, pay me!"
;;              :headers (my-makehash "x-more-info" "http://vimeo.com/22053820"))
(defun makehash (&rest init-values)
  (let ((h (make-hash-table :test 'equal)))
    (loop for x on init-values by #'cddr
       do (setf (gethash (first x) h) (second x)) )
    h))


;;; demo
;;; (sethash h :a nil)
;;; (sethash h :a nil :allow-nil nil)  :a 的 值nil 不会被设置
(defun sethash (h &rest set-values &key (allow-nil t supplied-p) &allow-other-keys)
  "这个方法会直接改变传入的hashtable ;如果传入了allow-nil 参数，那么更新hashtable时把rest参数里的allow nil和值剔除掉"
  (if supplied-p
      (if allow-nil
          (loop for x on set-values by #'cddr
             do (if (not (equal (first x) :allow-nil))
                    (setf (gethash (first x) h) (second x))))
          (loop for x on set-values by #'cddr
             do (if (and (not (equal (first x) :allow-nil))
                         (second x))
                    (setf (gethash (first x) h) (second x)))))
      (loop for x on set-values by #'cddr
         do (setf (gethash (first x) h) (second x))))
  h)





;;;
;;; Request:  simplify ningle request getter setter 简化request对象的读取和设置
;;;


;;; Request

(defmacro get-request-env ()
  `(lack.request:request-env ningle:*request*))

(defun get-request-env-to-hashtable ()
  (let ((h (make-hash-table :test 'equal)))
    (loop for x on (get-request-env) by #'cddr
       do (setf (gethash (car x) h) (cdr x)))
    h))

(defmacro get-request-content-length ()
  `(lack.request:request-content-length ningle:*request*))

(defmacro get-request-remote-addr ()
  `(lack.request:request-remote-addr ningle:*request*))

(defmacro get-request-user-agent ()
  `(gethash "user-agent" (lack.request:request-headers ningle:*request*)))

(defmacro get-request-query-parameters ()
  `(group-alist-to-hash (lack.request:request-query-parameters ningle:*request*)))


(defmacro get-request-server-name ()
  `(lack.request:request-server-name ningle:*request*))

(defmacro get-request-server-port ()
  `(lack.request:request-server-port ningle:*request*))

(defmacro get-request-uri ()
  `(lack.request:request-uri ningle:*request*))

(defun get-request-scheme ()
  (loop for x on (get-request-env) by #'cddr
     do (if (string-equal "url-scheme" (car x))
            (return (cadr x)))))

(defmacro get-request-content-type ()
  `(lack.request:request-content-type ningle:*request*))

(defun get-request-url ()
  (let ((scheme (get-request-scheme))
        (server-name (get-request-server-name))
        (server-port (get-request-server-port))
        (request-uri (get-request-uri))
        (protocol (or (gethash "x-forwarded-proto" (get-request-headers))
                      (gethash "x-forwarded-protocol" (get-request-headers)))))
    (cond ((and (null protocol)
                (string-equal "on" (gethash "x-forwarded-ssl" (get-request-headers))))
           nil)
          ((null protocol)
           (setf protocol "https"))
          (t (setf scheme protocol)))
    (concatenate 'string scheme "://" server-name  ":" (write-to-string server-port) request-uri))
  )


(defmacro get-request-origin ()
  `(gethash "x-forwarded-for" (get-request-headers) (get-request-remote-addr)))


;; 手动加上content-type 和 content-length(存在的情况下)
(defun get-request-headers ()
  (let ((headers (lack.request:request-headers ningle:*request*))
        (content-type (get-request-content-type))
        (content-length (get-request-content-length)))
    (if (not (null content-type))
        (setf (gethash "content-type" headers) content-type))
    (if (not (null content-length))
        (setf (gethash "content-length" headers) content-length))
    headers))


;; helper
;; 每个body-parameter是一个 list or cons
(defun body-parameters-to-hashtable (a-list)
  (let ((h (make-hash-table :test 'equal)))
    (dolist (l a-list)
      (setf (gethash (first l) h)
            (first-or-all-list
             (append (make-sure-list (gethash (first l) h))
                     (make-sure-list (null-to-string (second-element l)))))))
    h))

;; helper
(defun filter-form-parameters (all-list)
  (remove-if
   #'(lambda (x) (typep (second x) 'stream))
   all-list))


;; 条件:1. 当请求的content-type为application/x-www-form-urlencoded时，返回全部的内容
;; 2.当请求的content-type为multipart/form-data时，返回除文件类型外的内容
;; 实现: 需要的都在内容在 lack.request:request-body-parameters里，当content-type为multipart/form-data时只需要对文件内容过滤
(defun get-request-form ()
  (let ((content-type (get-request-content-type))
        (body-parameters (lack.request:request-body-parameters ningle:*request*)))
    (cond ((string-equal "application/x-www-form-urlencoded" content-type)
           (body-parameters-to-hashtable body-parameters))
          ((not (null (search "multipart/form-data; boundary" content-type)))
           (body-parameters-to-hashtable (filter-form-parameters body-parameters))
           )
          (t (null-to-hash)))))


(defmacro get-request-method ()
  `(lack.request:request-method ningle:*request*))


;; 条件: 当content-type是除了application/x-www-form-urlencoded和multipart/form-data时，返回请求的内容
(defun get-request-data ()
  (let ((request-content (lack.request:request-content ningle:*request*))
        (content-type (get-request-content-type)))
    (if (and (not (string-equal "application/x-www-form-urlencoded" content-type))
             (null (search "multipart/form-data; boundary" content-type)))
        (format nil "~{~a~}" (loop for c across request-content collect (code-char c)))
        (null-to-string))))


;; 条件:当请求的内容可以被json decode时，返回相应内容
(defun get-request-json ()
  (let ((request-data (get-request-data)))
    ;; (format t "!!!!~a!!!!" request-data)
    ;; (format t "^^^~a^^^" (json:decode-json-from-string request-data))
    (handler-case
        (json:decode-json-from-string request-data)
      (error () nil))))

(defmacro get-request-body-parameters ()
  `(lack.request:request-body-parameters ningle:*request*))


;; helper function, 考虑移到get-request-files函数内部
(defun filter-files-parameters (all-list)
  (remove-if-not
   #'(lambda (x) (typep (second x) 'stream))
   all-list))

;; helper function, 考虑移到get-request-files函数内部
(defun produce-return-content (v v2)
  (format nil "data:~a;base64,~a"
          (gethash "content-type" v2)
          (with-output-to-string (out)
            (s-base64:encode-base64 v out))))

;; 条件:当content-type: 为multi-formdata时, 返回请求内容中的文件内容
;; 实现:当content-type: 为multi-formdata时, 取出request-BODY-PARAMETERS中的
(defun get-request-files ()
  (let ((file-parameters (filter-files-parameters (get-request-body-parameters)))
        (h (make-hash-table)))
    (loop for (k v v1 v2) in file-parameters
       do (setf (gethash k h)
                (produce-return-content v v2 )))
    h))







;;;
;;; Response: simplify ningle esponse getter setter 简化response对象的读取和设置
;;;

(defmacro set-response-content-type (content-type)
  `(setf (lack.response:response-headers ningle:*response*)
         (append (lack.response:response-headers ningle:*response*)
                 (list :content-type ,content-type))))

(defmacro set-response-content-encoding (encoding-type)
  `(setf (lack.response:response-headers ningle:*response*)
         (append (lack.response:response-headers ningle:*response*)
                 (list :content-encoding ,encoding-type))))

(defmacro set-response-status (status-code)
  `(setf (lack.response:response-status ningle:*response*) ,status-code))

(defmacro set-response-location (location)
  `(setf (lack.response:response-headers ningle:*response*)
         (append (lack.response:response-headers ningle:*response*)
                 (list :location ,location)))
  )

(defun set-response-headers (set-headers)
  (setf (lack.response:response-headers ningle:*response*)
        (append (print-and-return (lack.response:response-headers ningle:*response*))
                (print-and-return set-headers))))


;; TODO 这边定义一些map太繁琐了，很有必要引入map字面量
;; 设置返回的response太繁琐，在falsk里，直接设置response对象就行
;; 在这里，设置返回的data和headers status_code等等是分开的，以后考虑整得清楚点
(defun status-code (code)
  "根据status code 生成相应的返回内容"
  (let* ((redirect (makehash :headers (makehash :location REDIRECT-LOACTION)))
         (code-map (makehash
                    301 redirect
                    302 redirect
                    303 redirect
                    304 (makehash :data "")
                    305 redirect
                    307 redirect
                    401 (makehash :headers (makehash "WWW-Authenticate" "Basic realm=\"Fake Realm\""))
                    402 (makehash :data "Fuck you, pay me!"
                                  :headers (makehash "x-more-info" "http://vimeo.com/22053820"))
                    406 (makehash :data (cl-json:encode-json-to-string
                                         (makehash :message "Client did not request a supported media type."
                                                   :accept ACCEPT-MEDIA-TYPES))
                                  :headers (makehash :Content-Type  "application/json"))
                    407 (makehash :headers (makehash "Proxy-Authenticate" "Basic realm=\"Fake Realm\""))
                    418 (makehash :data ASCII-ART
                                  :headers (makehash "x-more-info" "http://tools.ietf.org/html/rfc2324"))
                    )))
    (set-response-status code)
    (if (nth-value 1 (gethash code code-map))
        (let ((m (gethash code code-map)))
          (if (nth-value 1 (gethash :headers m))
              (set-response-headers (hash-to-list (gethash :headers m))))
          (if (nth-value 1 (gethash :data m))
              (gethash :data m))))))


;; 生成response
(defun get-json-response (&rest keys)
  (json:encode-json-to-string (apply #'get-hash-response keys)))

;; 生成response
(defun get-hash-response (&rest keys)
  (let ((h (make-hash-table :test 'equal))
        (out_h (make-hash-table :test 'equal)))
    (setf (gethash "headers" h) (get-request-headers)
          (gethash "args" h) (get-request-query-parameters)
          (gethash "origin" h) (get-request-origin)
          (gethash "url" h) (get-request-url)
          (gethash "form" h) (get-request-form)
          (gethash "method" h) (get-request-method)
          (gethash "data" h) (get-request-data)
          (gethash "json" h) (get-request-json)
          (gethash "files" h) (get-request-files)
          )
    (print-hash h)
    ;;(format t "~a" (get-request-files))
    (dolist (key keys)
      (if (consp key)
          (setf (gethash (car key) out_h) (cadr key))
          (setf (gethash key out_h) (gethash key h))))
    out_h))


;;;
;;; Route 相关
;;;

(defun replace-route-with-args (route-string args-list &key (test #'char=))
  (let ((replaced-string route-string))
    (loop for (arg value) in args-list
       do (progn
            (setf replaced-string (replace-all replaced-string (concatenate 'string ":" (make-sure-string arg)) (make-sure-string value) :test test))))
    replaced-string))



;; copy from https://stackoverflow.com/questions/4366668/str-replace-in-lisp
;; 需要研究下
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))
