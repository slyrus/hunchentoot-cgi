
(in-package :hunchentoot-cgi)

(defmacro with-input-from-process ((stream program program-args environment) &body body)
  "Creates an new process of the specified by PROGRAM using
PROGRAM-ARGS as a list of the arguments to the program. Binds the
stream variable to an input stream from which the output of the
process can be read and executes body as an implicit progn."
  #+sbcl
  (let ((process (gensym)))
    `(let ((,process (sb-ext::run-program ,program
                                          ,program-args
                                          :output :stream
                                          :environment ,environment
                                          :wait nil)))
       (when ,process
         (unwind-protect
              (let ((,stream (sb-ext:process-output ,process)))
                ,@body)
           (sb-ext:process-wait ,process)
           (sb-ext:process-close ,process)))))
  #-sbcl
  `(error "Not implemented yet!"))

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
