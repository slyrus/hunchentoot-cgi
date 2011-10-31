;;; file: hunchentoot-cgi.lisp
;;;
;;; Copyright (c) 2008 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package #:hunchentoot-cgi)

(defun host-name (&optional request)
  "Returns just the host portion of the 'Host' incoming http header
value, rather than either host or host:port if the port is specified."
  (let ((host-and-port (apply #'hunchentoot:host 
                              (when request (list request)))))
    (let ((colon-pos (position #\: host-and-port)))
      (if colon-pos
          (subseq host-and-port 0 colon-pos)
          host-and-port))))

(defun host-name-and-port (&optional request)
  "Returns the multiple values host and port (or nil if no port
  is specified) of the 'Host' incoming http header value, rather than
  either host or host:port if the port is specified."
  (let ((host-and-port (apply #'hunchentoot:host 
                              (when request (list request)))))
    (let ((colon-pos (position #\: host-and-port)))
      (if colon-pos
          (values (subseq host-and-port 0 colon-pos)
                  (subseq host-and-port (1+ colon-pos)))
          host-and-port))))

(defun handle-cgi-script (path &optional content-type)
  "A function which acts like a Hunchentoot handler for the file
denoted by PATH.  Send a content type header corresponding to
CONTENT-TYPE or \(if that is NIL) tries to determine the content
type via the file's suffix."
  (declare (ignore content-type)
           (optimize (debug 3)))
  (unless (or (pathname-name path)
              (pathname-type path))
    ;; not a file
    (setf (return-code*) +http-bad-request+)
    (throw 'handler-done nil))
  (unless (probe-file path)
    ;; does not exist
    (setf (return-code*) +http-not-found+)
    (throw 'handler-done nil))
  (let ((time (or (file-write-date path) (get-universal-time))))
    #+nil (setf (content-type) (or content-type
                                   (mime-type path)
                                   "application/octet-stream"))
    (handle-if-modified-since time)

    (let ((env
           (mapcar (lambda (x) (format nil "~A=~A" (car x) (cdr x)))
                   `(("SERVER_SOFTWARE" 
                      . (format nil "hunchentoot/~A"
                                hunchentoot-asd:*hunchentoot-version*))
                     ("SERVER_NAME" . ,(host-name))
                     ("GATEWAY_INTERFACE" . "CGI/1.1")
                         
                     ("SERVER_PROTOCOL" . ,(tbnl:server-protocol*))
                     ("SERVER_PORT" . ,(nth-value 1 (host-name-and-port)))
                     ("REQUEST_METHOD" . ,(tbnl:request-method*))
                     #+nil ("PATH_INFO" . "FIXME!")
                     #+nil ("PATH_TRANSLATED" . "FIXME!")
                     ("SCRIPT_NAME" . ,(tbnl:script-name*))
                     ("QUERY_STRING" . ,(tbnl:query-string*))
                     #+nil ("REMOTE_HOST" . "FIXME!")
                     ("REMOTE_ADDR" . ,(tbnl:remote-addr*))
                     #+nil ("REMOTE_USER" . "FIXME!")
                     #+nil ("REMOTE_IDENT" . "FIXME!")
                         
                     #+nil ("AUTH_TYPE" . "FIX")
                     ("HTTP_HOST" . ,(tbnl:acceptor-address tbnl:*acceptor*))
                     ("REQUEST_URI" . ,(tbnl:request-uri*))
                     ("SERVER_ADDR" . ,(tbnl:acceptor-address tbnl:*acceptor*))
                     ("HTTP_USER_AGENT" . ,(tbnl:user-agent))
                     ("HTTP_REFERER" . ,(tbnl:referer))))))      
      
      (handler-case
	  (with-input-from-program (in path nil env)
	    (chunga:with-character-stream-semantics
		(loop for line = (chunga:read-line* in)
		   until (equal line "")
		   do (destructuring-bind
			    (key val)
			  (ppcre:split ": " line :limit 2)
			(setf (hunchentoot:header-out key) val))))
	    (let ((out (flexi-streams:make-flexi-stream
			(tbnl:send-headers)
			:external-format tbnl::+latin-1+)))                   
	      (copy-stream in out 'character)))
	(error (error)
	  (tbnl:log-message* :error
                             "error in handle-cgi-script from URL ~A"
                             (tbnl:request-uri*)))))))

(defun create-cgi-dispatcher-and-handler (uri-prefix base-path &optional content-type)
  (unless (and (stringp uri-prefix)
               (plusp (length uri-prefix))
               (char= (char uri-prefix (1- (length uri-prefix))) #\/))
    (error "~S must be string ending with a slash." uri-prefix))
  (flet ((handler ()
           (let* ((script-name (url-decode (script-name*)))
                  (script-path (tbnl::enough-url (ppcre:regex-replace-all "\\\\" script-name "/")
                                           uri-prefix))
                  (script-path-directory (pathname-directory script-path)))
             (unless (or (stringp script-path-directory)
                         (null script-path-directory)
                         (and (listp script-path-directory)
                              (eq (first script-path-directory) :relative)
                              (loop for component in (rest script-path-directory)
                                 always (stringp component))))
               (setf (return-code*) +http-forbidden+)
               (throw 'handler-done nil))
             (handle-cgi-script (merge-pathnames script-path base-path) content-type))))
    (create-prefix-dispatcher uri-prefix #'handler)))

