(cl:in-package #:cl-multiaddr)

(define-condition multiaddr-error (error) ())

(define-condition invalid-protocol (multiaddr-error)
  ((slot)
   (value)))

