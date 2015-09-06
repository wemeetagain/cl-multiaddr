(cl:in-package #:asdf-user)

(defsystem :cl-multiaddr
  :description "Multiaddr utility for Common Lisp"
  :version "0.0.1"
  :author "Cayman Nava"
  :license "MIT"
  :depends-on (:alexandria :cl-ppcre :cl-multihash :octets-util :split-sequence :usocket :varint)
  :components ((:module "src"
	        :serial t
		:components
		((:file "package")
		 (:file "conditions")
		 (:file "util")
		 (:file "protocols")
		 (:file "multiaddr"))))
  :long-description #.(uiop:read-file-string
		       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-multiaddr-test))))
 
