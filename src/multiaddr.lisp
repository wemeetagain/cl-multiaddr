(cl:in-package #:cl-multiaddr)

;;; multihash interface

(defclass multiaddr () ()
  (:documentation "Multiaddr is a cross-protocol, cross-platform format for representing internet addresses."))

(defgeneric multiaddr-equal (m1 m2)
  (:documentation "Compares two multiaddrs for exact equality."))

(defgeneric multiaddr-bytes (multiaddr)
  (:documentation "Returns the VECTOR (UNSIGNED-BYTE 8) representation of this multiaddr."))

(defgeneric multiaddr-string (multiaddr)
  (:documentation "Returns the string representation of this multiaddr."))

(defgeneric multiaddr-protocols (multiaddr)
  (:documentation "Returns the list of PROTOCOLs that this multiaddr has."))

(defgeneric multiaddr-encapsulate (outer inner)
  (:documentation "Wraps INNER with OUTER, returning a new joined multiaddr."))

(defgeneric multiaddr-decapsulate (multiaddr pattern)
  (:documentation "Removes up to the first instance of PATTERN in MULTIADDR, returning a new multiaddr. If no match is found, a full copy is returned."))

;;; simple-multiaddr

(defclass simple-multiaddr (multiaddr)
  ((%bytes :initarg :bytes)))

(defgeneric make-multiaddr (object))

(defmethod make-multiaddr ((string string))
  (make-instance 'simple-multiaddr :bytes (string-to-bytes string)))

(defmethod make-multiaddr ((bytes vector))
  (declare (type (vector (unsigned-byte 8)) bytes))
  (make-instance 'simple-multiaddr :bytes bytes))

(defmethod multiaddr-equal ((m1 simple-multiaddr) (m2 simple-multiaddr))
  (loop
     with b1 = (multiaddr-bytes m1)
     with b2 = (multiaddr-bytes m2)
     initially (unless (= (length b1) (length b2))
		 (return nil))
     for i1 across b1
     for i2 across b2
     unless (= i1 i2)
     return nil
     finally (return t)))

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
     finally (return (reverse protocols))))

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
	(i -1))
    (ppcre:do-matches (start e string-2 string-1)
      (setf i start))
    (if (< i 0)
	;; if multiaddr not contained, return a copy
	(make-multiaddr (copy-seq (multiaddr-bytes ma)))
	(make-multiaddr (subseq string-1 0 i)))))

;;; string convenience methods

(defmethod multiaddr-equal ((m1 string) m2)
  (multiaddr-equal (make-multiaddr m1) m2))
(defmethod multiaddr-equal (m1 (m2 string))
  (multiaddr-equal m1 (make-multiaddr m2)))

(defmethod multiaddr-bytes ((multiaddr string))
  (multiaddr-bytes (make-multiaddr multiaddr)))

(defmethod multiaddr-string ((multiaddr string))
  (multiaddr-string (make-multiaddr multiaddr)))

(defmethod multiaddr-protocols ((multiaddr string))
  (multiaddr-protocols (make-multiaddr multiaddr)))

(defmethod multiaddr-encapsulate ((outer string) inner)
  (multiaddr-encapsulate (make-multiaddr outer) inner))
(defmethod multiaddr-encapsulate (outer (inner string))
  (multiaddr-encapsulate outer (make-multiaddr inner)))

(defmethod multiaddr-decapsulate ((multiaddr string) pattern)
  (multiaddr-decapsulate (make-multiaddr multiaddr) pattern))
(defmethod multiaddr-decapsulate (multiaddr (pattern string))
  (multiaddr-decapsulate multiaddr (make-multiaddr pattern)))

;;; utility multiaddr functions

(defun multiaddr-split (multiaddr)
  "Return a list of component multiaddrs."
  (declare (type multiaddr multiaddr))
  (loop for bytes in (bytes-split (multiaddr-bytes multiaddr))
     collect (make-multiaddr bytes)))

(defun multiaddr-join (multiaddrs)
  "Join a list of multiaddrs into a single multiaddr."
  (loop for multiaddr in multiaddrs
     collect (multiaddr-bytes multiaddr) into bytes
     finally (return (make-multiaddr (apply #'concatenate
					    '(vector (unsigned-byte 8))
					    bytes)))))
