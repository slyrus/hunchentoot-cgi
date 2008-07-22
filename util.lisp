
(in-package :hunchentoot-cgi)

;;;
;;; copy-stream from BKNR.
;;;
(defun copy-stream (in out &optional (element-type '(unsigned-byte 8)))
  "Copy everything from in to out"
  (let* ((buffer-size 4096)
         (buffer (make-array buffer-size :element-type element-type)))
    (labels ((read-chunks ()
               (let ((size (read-sequence buffer in)))
                 (if (< size buffer-size)
                     (write-sequence buffer out :start 0 :end size)
                     (progn
                       (write-sequence buffer out)
                       (read-chunks))))))
      (read-chunks))))
