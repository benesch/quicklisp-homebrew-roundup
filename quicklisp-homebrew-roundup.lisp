;;;; quicklisp-homebrew-roundup.lisp

(in-package #:quicklisp-homebrew-roundup)

(defconstant +resource-format-string+ "~:
  resource '~A' do
    url '~A'
    sha1 '~A'
  end~%~%")

(defclass load-strategy ()
  ((name
    :initarg :name
    :accessor name)
   (missing-systems
    :initarg :missing-systems
    :accessor missing-systems)
   (included-systems
    :initarg :included-systems
    :accessor included-systems)))

(defgeneric releases (strategy)
  (:method (strategy)
    (remove-duplicates (mapcar 'ql-dist:release (included-systems strategy)))))

(defgeneric ensure-releases (releases)
  (:method (releases)
    (dolist (release releases releases)
        (ql-dist:ensure-local-archive-file release))))

(defgeneric sha1sum (release)
  (:method (release)
    (ironclad:byte-array-to-hex-string
      (ironclad:digest-file :sha1 (ql-dist:ensure-local-archive-file release)))))

(defun for-system (name)
  (let ((strategy (calculate-load-strategy name)))
    (format t "Resolving ~S:~%" name)
    (let ((included-systems (mapcar 'ql-dist:name (included-systems strategy)))
          (missing-systems (missing-systems strategy)))
      (when missing-systems
        (format t "  Missing ~D system~:P!~%" (length missing-systems))
        (print-wrapped-list missing-systems))
      (when included-systems
        (format t "  Included ~D system~:P: ~%" (length included-systems))
        (print-wrapped-list included-systems)
        (let ((releases (releases strategy)))
          (ensure-releases releases)
          (terpri)
          (format t "Homebrew formula block:~%")
          (terpri)
          (dolist (release (sort releases #'string< :key #'ql-dist:name))
            (print-homebrew-resource release)))))))

(defun calculate-load-strategy (name)
  (setf name (string-downcase name))
  (let ((included-systems '())
        (missing-systems '()))
    (labels ((recurse (name)
               (let ((system (ql-dist:find-system name)))
                 (cond (system
                        (push system included-systems)
                        (dolist (subname (ql-dist:required-systems system))
                          (recurse subname)))
                       (t
                        (push name missing-systems))))))
      (ql-dist:with-consistent-dists
        (recurse name)))
    (make-instance 'load-strategy
                   :name name
                   :missing-systems (remove-duplicates missing-systems)
                   :included-systems (remove-duplicates included-systems))))

(defun print-homebrew-resource (release)
  (format t +resource-format-string+
          (ql-dist:name release)
          (ql-dist:archive-url release)
          (sha1sum release)))

(defun print-wrapped-list (words &key (indent 4) (margin 60))
 (let ((*print-right-margin* margin)
       (*print-pretty* t)
       (*print-escape* nil)
       (prefix (make-string indent :initial-element #\Space)))
   (pprint-logical-block (nil words :per-line-prefix prefix)
     (pprint-fill *standard-output* (sort (copy-seq words) #'string<) nil))
   (fresh-line)
   (finish-output)))
