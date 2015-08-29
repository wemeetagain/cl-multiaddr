(cl:in-package #:asdf-user)

(defsystem :cl-multiaddr
  :version "0.0.1"
  :author "Cayman Nava"
  :license "MIT"
  :depends-on (:alexandria :cl-ppcre :cl-multihash :octets-util :split-sequence :varint)
  :components ((:module "src"
	        :serial t
		:components
		((:file "package")
		 (:file "util")
		 (:file "protocols")
		 (:file "multiaddr"))))
  :description "Multiaddr utility for Common Lisp"
  :long-description #.(uiop:read-file-string
		       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (load-op cl-multiaddr-test))))
  
