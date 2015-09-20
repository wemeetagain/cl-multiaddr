## cl-multiaddr
[![Build Status](https://travis-ci.org/WeMeetAgain/cl-multiaddr.svg)](https://travis-ci.org/WeMeetAgain/cl-multiaddr)
[![Coverage Status](https://coveralls.io/repos/WeMeetAgain/cl-multiaddr/badge.svg?branch=master&service=github)](https://coveralls.io/github/WeMeetAgain/cl-multiaddr?branch=master)

cl-multiaddr is an implementation of [Multiaddr](https://github.com/jbenet/multiaddr).

### Example

```lisp
;; create multiaddr from string
(multiaddr:make-multiaddr "/ip4/127.0.0.1")
;; => #<CL-MULTIADDR::SIMPLE-MULTIADDR {100691B853}>

;; return string representaiton of multiaddr
(multiaddr:multiaddr-string
  (multiaddr:make-multiaddr "/ip4/127.0.0.1"))
;; => "/ip4/127.0.0.1"

;; return byte representation of multiaddr
(multiaddr:multiaddr-bytes (multiaddr:make-multiaddr "/ip4/127.0.0.1"))
;; => #(4 127 0 0 1)

;; also works with valid multiaddr strings
(multiaddr:multiaddr-bytes "/ip4/127.0.0.1")
;; => #(4 127 0 0 1)

;; encapsulate two multiaddrs
(multiaddr:multiaddr-string
  (multiaddr:multiaddr-encapsulate "/ip4/127.0.0.1" "/tcp/8080"))
;; => "/ip4/127.0.0.1/tcp/8080"

;; decapsulate a multiaddr
(multiaddr:multiaddr-string
  (multiaddr:multiaddr-decapsulate "/ip4/127.0.0.1/tcp/8080/ipfs/QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH" "/tcp/8080"))
;; => "/ip4/127.0.0.1"
```

### Documentation

### Testing

To run unit tests, run the following command:

```lisp
(asdf:test-system :cl-multiaddr)
```

### License

MIT
