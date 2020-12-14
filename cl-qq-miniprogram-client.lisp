(in-package :cl-qq-miniprogram-client)

;;; Common

(defconstant +qq-api-protocol+ "https")
(defconstant +qq-api-host+     "api.q.qq.com")

;;; QQ access token API

(defun qq-api-get-access-token (app-id app-secret
                                &key
                                  (protocol +qq-api-protocol+)
                                  (host +qq-api-host+)
                                  (uri "/api/getToken"))
  "Return access_token\(string\).
  API doc of getAccessToken: https://q.qq.com/wiki/develop/miniprogram/server/open_port/port_use.html#getaccesstoken"
  (wxa:wechat-api-get-access-token app-id app-secret
                                   :protocol protocol
                                   :host host
                                   :uri uri))

(defclass qq-miniprogram-client ()
  ((protocol
    :initarg :protocol
    :initform +qq-api-protocol+
    :reader protocol)
   (host
    :initarg :host
    :initform +qq-api-host+
    :reader host)
   (app-id
    :initarg :app-id
    :initform (error "Must pass a string as value of slot app-id")
    :reader app-id)
   (app-secret
    :initarg :app-secret
    :initform (error "Must pass a string as value of slot app-secret")
    :reader app-secret)
   (access-token
    :initarg :access-token
    :initform nil
    :reader access-token)
   (auto-refresh-access-token-p
    :initarg :auto-refresh-access-token-p
    :initform t
    :reader auto-refresh-access-token-p)))

(defmethod initialize-instance :after ((client qq-miniprogram-client) &rest args)
  (declare (ignore args))
  (flet ((init-access-token ()
           (setf (slot-value client 'access-token)
                 (qq-api-get-access-token (app-id client)
                                          (app-secret client)))))
    (init-access-token)
    (when (slot-value client 'auto-refresh-access-token-p)
      ;; refresh access-token every hour
      (cron:make-cron-job
       #'init-access-token
       :minute 0
       :hash-key (intern
                  (format nil
                          "~:@(qq-miniprogram-client-access-token-refresh-cron-job-~A~)"
                          (app-id client))))
      (cron:start-cron))))

(defun make-qq-miniprogram-client (app-id app-secret
                                   &key
                                     (protocol +qq-api-protocol+)
                                     (host +qq-api-host+)
                                     (auto-refresh-access-token-p t))
  (make-instance 'qq-miniprogram-client
                 :protocol protocol
                 :host host
                 :app-id app-id
                 :app-secret app-secret
                 :auto-refresh-access-token-p auto-refresh-access-token-p))

(defmethod print-object ((client qq-miniprogram-client) stream)
  (print-unreadable-object (client stream :type t :identity t)
    (format stream ":APP-ID ~S" (app-id client))))

;;; QQ Mini Programs APIs

(defmethod auth-code2session ((client qq-miniprogram-client)
                              js-code
                              &key (uri "/sns/jscode2session"))
  "Parameters:
    js-code: js_code from 
  Return session\(jsown) like this:
  \(:OBJ \
     \(\"session_key\" . \"xxxxx\"\)
     \(\"openid\" . \"yyyyy\"\)
     \(\"unionid\" . \"zzzzz\"\)\)
  Note: unionid will be contained in response only after binding miniprogram to WeChat Dev Platform account.
  API doc of code2Session: https://q.qq.com/wiki/develop/game/server/open-port/login.html#code2session
  API doc about login: https://q.qq.com/wiki/develop/game/frame/open-ability/login.html"
  (wxa:auth-code2session client
                         js-code
                         :uri uri))

(defmethod security-img-sec-check ((client qq-miniprogram-client)
                                   pathname
                                   &key (uri "/api/json/security/ImgSecCheck"))
  "Check whether image contain risky content.
  API doc of security.imgSecCheck: https://q.qq.com/wiki/develop/miniprogram/server/open_port/port_safe.html#security-imgseccheck"
  (multiple-value-bind (data status-code headers uri stream must-close-p status-text)
      (drakma:http-request (format nil "~A://~A~A?access_token=~A&appid=~A"
                                   protocol
                                   host
                                   uri
                                   access-token
                                   app-id)
                           :method :post
                           :content-type "multipart/form-data" 
                           :parameters (list (list "media" pathname)))
    (declare (ignore headers uri stream must-close-p status-text))
    (when (eq 200 status-code)
      (let ((response (jsown:parse (babel:octets-to-string data :encoding :utf-8))))
        (if (equal 0 (safe-val response "errCode"))
            t
            (values nil response))))))

(defun decrypt-private-user-info (encrypted-private-user-info session-key iv)
  "Decrypt encryptedData from QQ mini-program API qq.getUserInfo in QQ Mini Program.
  Parameters:
    encrypted-data: string\(base64 string\), encryptedData of response of API qq.getUserInfo in QQ Mini Program
    session-key: string, session_key of response of API code2session, as encoding-aes-key
    iv: string\(base64 string\), iv of response of API code2session
  Note: API code2Session require parameter js_code\(code of response of API qq.login\)
  API doc of code2Session: https://q.qq.com/wiki/develop/game/server/open-port/login.html#code2session
  API doc of signature, verification, encryption, and decryption of user data: https://q.qq.com/wiki/develop/game/frame/open-ability/signature.html"
  (wxa:decrypt-private-user-info encrypted-private-user-info session-key iv))