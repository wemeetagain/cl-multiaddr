(cl:in-package #:asdf-user)

(defsystem :cl-multiaddr-test
  :description "Multiaddr unit tests"
  :license "MIT"
  :depends-on (:cl-multiaddr :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "t"
		:components
		((:test-file "multiaddr"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
