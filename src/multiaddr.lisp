(cl:in-package #:cl-multiaddr)

;;; multihash interface

(defclass multiaddr () ()
  (:documentation "Multiaddr is a cross-protocol, cross-platform format for representing internet addresses."))

(defgeneric multiaddr-bytes (multiaddr)
  (:documentation "Returns the VECTOR (UNSIGNED-BYTE 8) representation of this multiaddr."))

(defgeneric multiaddr-string (multiaddr)
  (:documentation "Returns the string representation of this multiaddr."))

(defgeneric multiaddr-protocols (multiaddr)
  (:documentation "Returns the list of PROTOCOLs that this multiaddr has."))

(defgeneric multiaddr-encapsulate (outer inner)
  (:documentation "Wraps INNER with OUTER, returning a new joined multiaddr."))

(defgeneric multiaddr-decapsulate (multiaddr pattern)
  (:documentation "Removes up to the first instance of PATTERN in MULTIADDR, returning a new multiaddr."))

;;; simple-multiaddr

(defclass simple-multiaddr (multiaddr)
  ((%bytes :initarg :bytes)))

(defgeneric make-multiaddr (object))

(defmethod make-multiaddr ((string string))
  (make-instance 'simple-multiaddr :bytes (string-to-bytes string)))

(defmethod make-multiaddr ((bytes vector))
  (declare (type (vector (unsigned-bytes 8)) vector))
  (make-instance 'simple-multiaddr :bytes bytes))

(defmethod multiaddr-bytes ((ma simple-multiaddr))
  (copy-seq (slot-value ma '%bytes)))

(defmethod multiaddr-string ((ma simple-multiaddr))
  (bytes-to-string (multiaddr-bytes ma)))

(defmethod multiaddr-protocols ((ma simple-multiaddr))
  (loop with protocols = '()
     and bytes = (multiaddr-bytes ma)
     and size = 0
     while (> (length bytes) 0)
     for code-index = (multiple-value-list (varint-to-code bytes))
     for (code n) = code-index
     for protocol = (protocol-with-code code)
     if (zerop (protocol-code protocol))
     do (error 'invalid-protocol-code :code code)
     do (progn
	  (push protocol protocols)
	  (setf bytes (subseq bytes n))
	  (setf size (size-for-addr protocol bytes))
	  (setf bytes (subseq bytes size)))
     finally (return protocols)))

(defmethod multiaddr-encapsulate ((ma simple-multiaddr) (o multiaddr))
  (let ((outer-bytes (multiaddr-bytes ma))
	(inner-bytes (multiaddr-bytes o)))
    (make-instance 'simple-multiaddr
		   :bytes (concatenate '(vector (unsigned-byte 8))
				       outer-bytes
				       inner-bytes))))

(defmethod multiaddr-decapsulate ((ma simple-multiaddr) (o multiaddr))
  (let ((string-1 (multiaddr-string ma))
	(string-2 (multiaddr-string o))
	(i 0))
    (ppcre:do-matches (start e r t)
      (declare (ignore e r t))
      (setf i start))
    (if (zerop i)
	;; if multiaddr not contained, return a copy
	(make-instance 'simple-multiaddr :bytes (copy-seq (multiaddr-bytes ma)))
	(make-simple-multiaddr (subseq string-1 0 i)))))

;;; utility multiaddr unctions

(defun multiaddr-split (multiaddr)
  (declare (type multiaddr multiaddr))
  (loop for bytes in (bytes-split (multiaddr-bytes multiaddr))
     collect (make-multiaddr bytes)))

(defun multiaddr-join (multiaddrs)
  (loop for multiaddr in multiaddrs
     collect (multiaddr-bytes multiaddr) into bytes
     finally (return (make-multiaddr (apply #'concatenate
					    '(vector (unsigned-bytes 8))
					    bytes)))))
