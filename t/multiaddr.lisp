(cl:in-package #:cl-user)

(defpackage #:cl-multiaddr-test
  (:use #:cl #:prove))
(in-package #:cl-multiaddr-test)

(plan nil)

;;; use tests from https://github.com/jbenet/go-multiaddr/blob/master/multiaddr_test.go

#.(loop for case in '("/ip4"
		      "/ip4/::1"
		      "/ip4/fdpsofodsajfdoisa"
		      "/ip6"
		      "/udp"
		      "/tcp"
		      "/sctp"
		      "/udp/65536"
		      "/tcp/65536"
		      "/udp/1234/sctp"
		      "/udp/1234/udt/1234"
		      "/udp/1234/utp/1234"
		      "/ip4/127.0.0.1/udp/jfodsajfidosajfoidsa"
		      "/ip4/127.0.0.1/udp"
		      "/ip4/127.0.0.1/tcp/jfodsajfidosajfoidsa"
		      "/ip4/127.0.0.1/tcp"
		      "/ip4/127.0.0.1/ipfs"
		      "/ip4/127.0.0.1/ipfs/tcp")
     collect `(is-error (multiaddr:make-multiaddr ,case) 'error) into forms
     finally (return
	       `(subtest "MAKE-MULTIADDR Failure Tests"
		  ,@forms)))

#.(loop for case in '("/ip4/1.2.3.4"
		      "/ip4/0.0.0.0"
		      "/ip6/::1"
		      "/ip6/2601:9:4f81:9700:803e:ca65:66e8:c21"
		      "/udp/0"
		      "/tcp/0"
		      "/sctp/0"
		      "/udp/1234"
		      "/tcp/1234"
		      "/sctp/1234"
		      "/udp/65535"
		      "/tcp/65535"
		      "/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC"
		      "/udp/1234/sctp/1234"
		      "/udp/1234/udt"
		      "/udp/1234/utp"
		      "/tcp/1234/http"
		      "/tcp/1234/https"
		      "/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC/tcp/1234"
		      "/ip4/127.0.0.1/udp/1234"
		      "/ip4/127.0.0.1/udp/0"
		      "/ip4/127.0.0.1/tcp/1234"
		      "/ip4/127.0.0.1/tcp/1234/"
		      "/ip4/127.0.0.1/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC"
		      "/ip4/127.0.0.1/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC/tcp/1234")
     collect `(ok (multiaddr:make-multiaddr ,case)) into forms
     finally (return
	       `(subtest "MAKE-MULTIADDR Success Tests"
		  ,@forms)))

(subtest "MULTIADDR-EQUAL Tests"
	 (let ((m1 (multiaddr:make-multiaddr "/ip4/127.0.0.1/udp/1234"))
	       (m2 (multiaddr:make-multiaddr "/ip4/127.0.0.1/tcp/1234"))
	       (m3 (multiaddr:make-multiaddr "/ip4/127.0.0.1/tcp/1234"))
	       (m4 (multiaddr:make-multiaddr "/ip4/127.0.0.1/tcp/1234/")))
	   (ok
	    (not (multiaddr:multiaddr-equal m1 m2)))
	   (ok
	    (not (multiaddr:multiaddr-equal m2 m1)))
	   (ok
	    (multiaddr:multiaddr-equal m2 m3))
	   (ok
	    (multiaddr:multiaddr-equal m3 m2))
	   (ok
	    (multiaddr:multiaddr-equal m1 m1))
	   (ok
	    (multiaddr:multiaddr-equal m2 m4))
	   (ok
	    (multiaddr:multiaddr-equal m4 m3))))

(subtest "STRING-TO-BYTES Tests"
  (flet ((test-string (string hex)
	   (let* ((b1 (octets-util:hex-string-to-octets hex))
		  (b2 (multiaddr::string-to-bytes string))
		  (fail-desc
		   (format nil "failed to convert ~A to ~A, got ~A" string b1 b2)))
	     (unless (= (length b1) (length b2))
	       (fail fail-desc))
	     (loop
		for i1 across b1
		for i2 across b2
		unless (= i1 i2)
		do (fail fail-desc)
		finally (pass
			 (format nil "converted ~A to ~A" string b1))))))
    (test-string "/ip4/127.0.0.1/udp/1234" "047f0000011104d2")
    (test-string "/ip4/127.0.0.1/tcp/4321" "047f0000010610e1")
    (test-string "/ip4/127.0.0.1/udp/1234/ip4/127.0.0.1/tcp/4321" "047f0000011104d2047f0000010610e1")))

(subtest "BYTES-TO-STRING Tests"
  (flet ((test-string (string hex)
	   (let* ((bytes (octets-util:hex-string-to-octets hex))
		  (string2 (multiaddr::bytes-to-string bytes)))
	     (if (string= string string2)
		 (pass (format nil "converted ~A to ~A" bytes string2))
		 (fail (format nil "failed to convert ~A to ~A, got ~A" bytes string string2))))))
    (test-string "/ip4/127.0.0.1/udp/1234" "047f0000011104d2")
    (test-string "/ip4/127.0.0.1/tcp/4321" "047f0000010610e1")
    (test-string "/ip4/127.0.0.1/udp/1234/ip4/127.0.0.1/tcp/4321" "047f0000011104d2047f0000010610e1")))


(subtest "MULTIADDR-SPLIT and MULTIADDR-JOIN Tests"
  (flet ((test-string (string strings)
	   (let* ((m (multiaddr:make-multiaddr string))
		  (split (multiaddr:multiaddr-split m))
		  (joined (multiaddr:multiaddr-join split)))
	     (block end
	       (unless (= (length split) (length strings))
		 (return-from end
		   (fail
		    (format nil "not enough split components: ~A" split))))
	       (loop
		  for split-item in split
		  for strings-item in strings
		  unless (string= (multiaddr:multiaddr-string split-item) strings-item)
		  do (return-from end
		       (fail
			(format nil "split component failed: ~A != ~A" split-item strings-item))))
	       (unless (multiaddr:multiaddr-equal m joined)
		 (return-from end
		   (fail
		    (format nil "joined components failed: ~A != ~A" m joined))))

	       ;; modifying underlying bytes should not change anything (bytes should be properly copied)
	       (loop for index below (length (slot-value m 'multiaddr::%bytes))
		  do (setf (slot-value m 'multiaddr::%bytes) 0))
	       (loop
		  for split-item in split
		  for strings-item in strings
		  unless (string= (multiaddr:multiaddr-string split-item) strings-item)
		  do (return-from end
		       (fail
			(format nil "split component failed: ~A != ~A" split-item strings-item))))
	       (pass "split and join passed")))))
    (test-string "/ip4/1.2.3.4/udp/1234"
		 '("/ip4/1.2.3.4" "/udp/1234"))
    (test-string "/ip4/1.2.3.4/tcp/1/ip4/2.3.4.5/udp/2"
		 '("/ip4/1.2.3.4" "/tcp/1" "/ip4/2.3.4.5" "/udp/2"))
    (test-string "/ip4/1.2.3.4/utp/ip4/2.3.4.5/udp/2/udt"
		 '("/ip4/1.2.3.4" "/utp" "/ip4/2.3.4.5" "/udp/2" "/udt"))))

(subtest "MULTIADDR-PROTOCOLS Tests"
  (let* ((multiaddr (multiaddr:make-multiaddr "/ip4/127.0.0.1/udp/1234"))
	 (protocols (multiaddr:multiaddr-protocols multiaddr)))
    (is (multiaddr:protocol-code (first protocols))
	(multiaddr:protocol-code (multiaddr:protocol-with-name "ip4")))
    (is (multiaddr:protocol-code (second protocols))
	(multiaddr:protocol-code (multiaddr:protocol-with-name "udp")))))

#.(let ((good-cases '(("/ip4"
		       ((multiaddr:protocol-with-name "ip4")))
		      ("/ip4/tcp"
		       ((multiaddr:protocol-with-name "ip4")
			(multiaddr:protocol-with-name "tcp")))
		      ("ip4/tcp/udp/ip6"
		       ((multiaddr:protocol-with-name "ip4")
			(multiaddr:protocol-with-name "tcp")
			(multiaddr:protocol-with-name "udp")
			(multiaddr:protocol-with-name "ip6")))
		      ("////////ip4/tcp"
		       ((multiaddr:protocol-with-name "ip4")
			(multiaddr:protocol-with-name "tcp")))
		      ("ip4/udp/////////"
		       ((multiaddr:protocol-with-name "ip4")
			(multiaddr:protocol-with-name "udp")))
		      ("////////ip4/tcp////////"
		       ((multiaddr:protocol-with-name "ip4")
			(multiaddr:protocol-with-name "tcp")))))
	(bad-cases '("dsijafd"                           ; bogus proto
		     "/ip4/tcp/fidosafoidsa"             ; bogus proto
		     "////////ip4/tcp/21432141/////////" ; bogus proto
		     "////////ip4///////tcp/////////"))) ; empty protos in between
    `(subtest "PROTOCOL-WITH-STRING Tests"
       ,@(loop for (string protocols) in good-cases
	    collect `(ok
		      (loop
			 with protocols = (multiaddr:protocols-with-string ,string)
			 for protocol in protocols
			 for p in (list ,@protocols)
			 unless (= (multiaddr:protocol-code protocol)
				   (multiaddr:protocol-code p))
			 return nil
			 finally (return t))))
       ,@(loop for string in bad-cases
	    collect `(is-error (multiaddr:protocols-with-string ,string) 'error))))

(subtest "MULTIADDR-ENCAPSULATE and MULTIADDR-DECAPSULATE Tests"
  (let ((m1 (multiaddr:make-multiaddr "/ip4/127.0.0.1/udp/1234"))
	(m2 (multiaddr:make-multiaddr "/udp/5678"))
	(m3 (multiaddr:make-multiaddr "/udp/5678"))
	(m4 (multiaddr:make-multiaddr "/ip4/127.0.0.1"))
	(test1 "/ip4/127.0.0.1/udp/1234/udp/5678")
	(test2 "/ip4/127.0.0.1/udp/1234")
	(test3 ""))
    (let ((b (multiaddr:multiaddr-encapsulate m1 m2)))
      (is (multiaddr:multiaddr-string b) test1)
      (let ((c (multiaddr:multiaddr-decapsulate b m3)))
	(is (multiaddr:multiaddr-string c) test2)
	(let ((d (multiaddr:multiaddr-decapsulate c m4)))
	  (is (multiaddr:multiaddr-string d) test3))))))

(finalize)
