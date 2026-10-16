(in-package #:cbor-tests)

(defmacro with-typed-array-options (&body body)
  `(let ((*strict* t)
         (*array-format* :array)
         (*jsown-semantics* nil)
         (*use-stringrefs* nil)
         (*use-sharedrefs* nil))
     ,@body))

(defun signed16-vector (&rest values)
  (make-array (length values) :element-type '(signed-byte 16)
                             :initial-contents values))

(defun check-signed16-vector (expected actual)
  (true (typep actual '(simple-array (signed-byte 16) (*))))
  (is equalp expected actual))

(define-test typed-arrays)

(define-test (typed-arrays wire-format)
  (with-typed-array-options
    (dolist (*strict* '(t nil))
      (let ((samples (signed16-vector -32768 -1 0 1 32767)))
        ;; RFC 8746 tag 77, followed by a ten-byte little-endian string.
        (is equalp (bytes "d84d4a0080ffff00000100ff7f") (encode samples))
        (check-signed16-vector samples (decode (encode samples)))
        (check-signed16-vector
         samples (decode (bytes "d8494a8000ffff000000017fff"))))
      (is equalp (bytes "d84d40") (encode (signed16-vector)))
      (dolist (wire '("d84d40" "d84940"))
        (check-signed16-vector #() (decode (bytes wire)))))))

(define-test (typed-arrays indefinite-byte-strings)
  (with-typed-array-options
    (dolist (*strict* '(t nil))
      ;; Chunk boundaries need not align with individual 16-bit samples.
      (dolist (wire '("d84d5f41ff43ff0100ff" "d8495f41ff43ff0001ff"))
        (check-signed16-vector #(-1 1) (decode (bytes wire))))
      (dolist (wire '("d84d5f41ff42ff01ff" "d8495f41ff42ff01ff"
                      "d84d5f41ff43ff01" "d8495f41ff43ff01"))
        (fail (decode (bytes wire)) cbor-decode-error)))))

(define-test (typed-arrays payload-size)
  (with-typed-array-options
    (let ((samples (make-array 256 :element-type '(signed-byte 16))))
      (dotimes (i 256)
        (setf (aref samples i) (- (* i 257) 32768)))
      (let ((wire (encode samples)))
        (is = 517 (length wire))
        (is equalp (bytes "d84d590200") (subseq wire 0 5))
        (check-signed16-vector samples (decode wire))))))

(define-test (typed-arrays ordinary-values)
  (with-typed-array-options
    (dolist (*strict* '(t nil))
      (is equalp (bytes "f6") (encode nil))
      (is eql nil (decode (bytes "f6")))
      (is equalp (bytes "83002001") (encode #(0 -1 1)))
      (let ((decoded (decode (bytes "83002001"))))
        (true (typep decoded 'simple-vector))
        (is equalp #(0 -1 1) decoded))
      (let ((octets (bytes "00ff01")))
        (is equalp (bytes "4300ff01") (encode octets))
        (let ((decoded (decode (encode octets))))
          (true (typep decoded '(simple-array (unsigned-byte 8) (*))))
          (is equalp octets decoded))))))

(define-test (typed-arrays array-format)
  (with-typed-array-options
    (let ((*array-format* :list))
      (is equal '(1 2) (decode (bytes "820102")))
      (check-signed16-vector #(-1 1) (decode (bytes "d84d44ffff0100")))
      (check-signed16-vector #(-1 1) (decode (bytes "d84944ffff0001"))))))

(define-test (typed-arrays non-simple-vectors)
  (with-typed-array-options
    (let* ((backing (signed16-vector -2 -1 0 1 2))
           (displaced (make-array 3 :element-type '(signed-byte 16)
                                    :displaced-to backing
                                    :displaced-index-offset 1))
           (with-fill-pointer
             (make-array 5 :element-type '(signed-byte 16)
                           :initial-contents '(-1 0 1 2 3)
                           :adjustable t :fill-pointer 3)))
      (dolist (samples (list displaced with-fill-pointer))
        (is equalp (bytes "d84d46ffff00000100") (encode samples))
        (check-signed16-vector #(-1 0 1) (decode (encode samples)))))))

(define-test (typed-arrays invalid-payloads)
  (with-typed-array-options
    (dolist (*strict* '(t nil))
      (dolist (wire '("d84900" "d84d00"       ; unsigned integer
                      "d849f6" "d84df6"       ; null
                      "d849626162" "d84d626162" ; text string
                      "d849820001" "d84d820001" ; ordinary array
                      "d8494100" "d84d4100"   ; odd byte count
                      "d84943000102" "d84d43000102"))
        (fail (decode (bytes wire)) cbor-decode-error))
      (dolist (wire '("d849" "d84d"))
        (fail (decode (bytes wire)) error))
      (dolist (wire '("d849440001" "d84d440001" "440001" "5f440001"))
        (fail (decode (bytes wire)) cbor-decode-error)))))

(define-test (typed-arrays shared-references)
  (with-typed-array-options
    (dolist (*strict* '(t nil))
      (dolist (*use-sharedrefs* '(t nil))
        (dolist (*use-stringrefs* '(t nil))
          (let* ((samples (signed16-vector -32768 32767))
                 (distinct (copy-seq samples))
                 (decoded (decode (encode (vector samples samples distinct)))))
            (dotimes (i 3)
              (check-signed16-vector samples (aref decoded i)))
            (is eql *use-sharedrefs* (eq (aref decoded 0) (aref decoded 1)))
            (false (eq (aref decoded 0) (aref decoded 2)))))))))

(define-test (typed-arrays string-references)
  (with-typed-array-options
    (let ((*use-stringrefs* t))
      (let* ((samples (signed16-vector -1 1))
             (wire (encode (vector samples "abc" "abc"))))
        ;; The packed payload occupies string-reference index 0, so "abc"
        ;; must occupy index 1 on both encoder and decoder.
        (is equalp (bytes "d9010083d84d44ffff010063616263d81901") wire)
        (let ((decoded (decode wire)))
          (check-signed16-vector samples (aref decoded 0))
          (is equal "abc" (aref decoded 1))
          (is equal "abc" (aref decoded 2)))))
    ;; A typed array's byte string may itself be a string reference.
    (let ((decoded (decode (bytes "d901008244ffff0100d84dd81900"))))
      (true (typep (aref decoded 0) '(simple-array (unsigned-byte 8) (*))))
      (check-signed16-vector #(-1 1) (aref decoded 1)))
    ;; Resolving a reference to a text string is not a valid typed payload.
    (fail (decode (bytes "d901008263616263d84dd81900")) cbor-decode-error)))
