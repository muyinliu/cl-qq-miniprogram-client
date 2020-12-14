(defsystem "cl-qq-miniprogram-client"
  :name "cl-qq-miniprogram-client"
  :description "QQ Mini Program client for server API in Common Lisp."
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "ISC"
  :depends-on ("cl-wechat-miniprogram-client")
  :serial t
  :components ((:file "packages")
               (:file "cl-qq-miniprogram-client")))
