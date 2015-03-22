;;;; quicklisp-roundup.lisp

(in-package #:quicklisp-roundup)

(defparameter *resource-format-string* "~:
resource \"~A\" do
  url \"~A\"
  sha256 \"~A\"
end~%~%")

(defparameter *build-directory*
  (make-pathname :directory '(:relative "build")))

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
      (ironclad:digest-file :sha256 (ql-dist:ensure-local-archive-file release)))))

(defgeneric print-homebrew-resource (release &key stream)
  (:method (release &key (stream t))
    (format stream *resource-format-string*
            (ql-dist:name release)
            (ql-dist::archive-url release)
            (sha1sum release))))

(defun make-homebrew (name)
  (let* ((strategy (calculate-load-strategy name))
         (releases (releases strategy))
         (output-file (merge-pathnames (make-pathname :name (concatenate 'string name "-resources")
                                                      :type "rb")
                                       *build-directory*)))
    (print-load-strategy strategy)
    (ensure-releases releases)
    (ensure-directories-exist output-file)
    (with-open-file (stream output-file
                            :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create)
      (dolist (release (sort releases #'string< :key #'ql-dist:name))
        (print-homebrew-resource release :stream stream)))
    (terpri)
    (format t "Homebrew formula block written to:~%")
    (format t "  ~S" (namestring output-file))
    (terpri)))

(defun make-tarball (name)
  (let* ((strategy (calculate-load-strategy name))
         (output-file (merge-pathnames (make-pathname :name name :type "tgz")
                                       *build-directory*))
         (tmp-directory (merge-pathnames (make-pathname :directory (list :relative name))
                                         *build-directory*))
         (tmp-tar (merge-pathnames (make-pathname :name "tmp" :type "tar")
                                   tmp-directory)))
    (print-load-strategy strategy)
    (ignore-errors (sb-ext:delete-directory tmp-directory :recursive t))
    (ensure-directories-exist tmp-directory)
    (dolist (release (releases strategy))
      (ql-gunzipper:gunzip (ql-dist:ensure-local-archive-file release) tmp-tar)
      (ql-minitar:unpack-tarball tmp-tar :directory tmp-directory))
    (delete-file tmp-tar)
    (sb-ext:run-program "tar"
                        (list "cf" (namestring output-file)
                              "-C" (namestring tmp-directory)
                              ".")
                        :search t)
    (terpri)
    (format t "Tarball written to:~%")
    (format t "  ~S" (namestring output-file))
    (terpri)))

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

(defun print-load-strategy (strategy)
  (format t "Resolving ~S:~%" (name strategy))
  (let ((included-systems (mapcar 'ql-dist:name (included-systems strategy)))
        (missing-systems (missing-systems strategy)))
    (when missing-systems
      (format t "  Missing ~D system~:P!~%" (length missing-systems))
      (print-wrapped-list missing-systems))
    (when included-systems
      (format t "  Included ~D system~:P: ~%" (length included-systems))
      (print-wrapped-list included-systems))))

(defun print-wrapped-list (words &key (indent 4) (margin 60))
 (let ((*print-right-margin* margin)
       (*print-pretty* t)
       (*print-escape* nil)
       (prefix (make-string indent :initial-element #\Space)))
   (pprint-logical-block (nil words :per-line-prefix prefix)
     (pprint-fill *standard-output* (sort (copy-seq words) #'string<) nil))
   (fresh-line)
   (finish-output)))
