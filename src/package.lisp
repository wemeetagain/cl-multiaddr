(cl:in-package #:cl-user)

(defpackage #:cl-multiaddr
  (:nickname #:multiaddr)
  (:use #:cl)
  (:export #:protocol
	   #:protocol-code
	   #:protocol-size
	   #:protocol-name
	   #:protocol-vcode
	   ;;
	   #:+p-ip4+ #:+p-tcp+ #:+p-udp+ #:+p-dccp+ #:+p-ip6+
	   #:+p-sctp+ #:+p-utp+ #:+p-ipfs+ #:+p-http+ #:+p-https+
	   ;;
	   #:*protocols*
	   #:protocol-with-name
	   #:protocol-with-code
	   #:protocol-with-string
	   ;;
           #:multiaddr
	   #:multiaddr-bytes
	   #:multiaddr-string
	   #:multiaddr-protocols
	   #:multiaddr-encapsulate
	   #:multiaddr-decapsulate
	   #:make-multiaddr
	   #:multiaddr-split
	   #:multiaddr-join))
