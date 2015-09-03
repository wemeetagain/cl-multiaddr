(cl:in-package #:cl-multiaddr)

(define-condition multiaddr-error () ())

(define-condition invalid-protocol (multiaddr-error)
  ((slot)
   (value)))

