(cl:in-package #:cl-multiaddr)

(defclass protocol ()
  ((%code :accessor protocol-code :initarg :code)
   (%size :accessor protocol-size :initarg :size) ; a size of -1 indicates a length-prefixed variable size
   (%name :accessor protocol-name :initarg :name)
   (%vcode :accessor protocol-vcode :initarg :vcode))
  (:documentation "Protocol is a multihash protocol description structure."))

(defconstant +length-prefixed-var-size+ -1)

;;; replicating table here to:
;;; 1. avoid parsing the csv
;;; 2. ensuring errors in the csv don't screw up code.
;;; 3. changing a number has to happen in two places.
(constant +definitions+
  '((4 32 "ip4")
    (6 16 "tcp")
    (17 16 "udp")
    (33 16 "dccp")
    (41 28 "ip6")
    (132 16 "sctp")
    (301 0 "udt")
    (302 0 "utp")
    (421 +length-prefixed-var-size+ "ipfs")
    (480 0 "http")
    (443 0 "https")))

(defvar *protocols*
  (loop for (code size name) in +definitions+
     collect (make-instance 'protocol
			    :code code
			    :size size
			    :name name
			    :vcode (code-to-varint code))))

#.(loop for (code size name) in +definitions+
     collect `(defconstant ,(alexandria:symbolicate '+P- (string-upcase name) '+)
		,code) into forms
     finally (return `(progn ,@forms)))

(defun protocol-with-name (name)
  (let ((protocol (member name *procotols*
			  :key #'protocol-name
			  :test #'string=)))
    (unless protocol
      (error 'invalid-protocol :slot :name :value name))
    protocol))

(defun protocol-with-code (code)
  (let ((protocol (member code *procotols*
			  :key #'protocol-code
			  :test #'=)))
    (unless protocol
      (error 'invalid-protocol :slot :code :value code))
    protocol))

(defun protocol-with-string (string)
  (let* ((string (ppcre:regex-replace-all "(^/+|/+$)" string ""))
	 (names (split-sequence:split-sequence #\/ string)))
    (unless (zerop (length names))
      (loop for name in names
	 for protocol = (protocol-with-name name)
	 collect protocol))))
