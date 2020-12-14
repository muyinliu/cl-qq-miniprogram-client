(in-package :cl-user)

(defpackage cl-qq-miniprogram-client
  (:use :cl)
  (:nicknames :qq-miniprogram-client :qqa-client :qqa)
  #+:sbcl (:shadow :defconstant)
  #+:sb-package-locks (:lock t)
  (:export
   ;; constants
   #:+qq-api-protocol+
   #:+qq-api-host+
   ;; classes
   #:qq-miniprogram-client
   ;; accessors
   #:protocol
   #:host
   #:app-id
   #:app-secret
   #:access-token
   #:auto-refresh-access-token-p
   ;; functions
   #:make-qq-miniprogram-client
   #:auth-code2session
   #:security-img-sec-check
   #:decrypt-private-user-info))

(in-package :cl-qq-miniprogram-client)

#+sbcl
(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
