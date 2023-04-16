(ql:quickload '("uiop" "cl-ppcre"))

(declaim (ftype (function () null))
         (inline clear-screen))
(defun clear-screen ()
  (format t "~c[2J" (code-char 27)))

(declaim (ftype (function (string &optional symbol) t)
                get-description))
(defun get-description (name &optional (type 'xbps))
  (case type
    (xbps
     (uiop:run-program (format nil "xbps-query -p short_desc ~a" name)
                       :output '(:string :stripped t)))
    (flatpak
     (with-input-from-string (stream (uiop:run-program
                                      (format nil "flatpak info ~a" name)
                                      :output '(:string :stripped t)))
       (read-line stream)
       (svref (cadr (multiple-value-list
                     (ppcre:scan-to-strings
                      "\\s-\\s(.+)$"
                      (car (multiple-value-list (read-line stream))))))
              0)))
    (t (error "Unknown package type"))))

(declaim (ftype (function ((or string pathname) &optional symbol) list)
                make-package-list))
(defun make-package-list (package-file-path &optional (type 'xbps))
  (map 'list (lambda (package-name)
               (list t package-name (get-description package-name type)))
       (uiop:read-file-lines package-file-path)))

(declaim (ftype (function (list &optional fixnum) list) make-keyed-list))
(defun make-keyed-list (list &optional (from 97))
  (loop for i in list
        for char-code = from then (1+ char-code)
        collect (cons (code-char char-code) i)))

(declaim (ftype (function (list &optional symbol) t) install-packages))
(defun install-packages (package-list &optional (type 'xbps))
  (uiop:run-program (append (case type
                              (xbps '("sudo" "xbps-install" "-n"))
                              (flatpak '("flatpak" "install" "flathub"))
                              (t (error "Unknown package type")))
                            (map 'list #'cadr package-list))
                    :input :interactive
                    :output :interactive))

(declaim (ftype (function (fixnum list) list) toggle-package-install))
(defun toggle-package-install (index package-list
                               &aux (local-list (copy-tree package-list)))
  (setf (car (nth index local-list)) (not (car (nth index local-list))))
  local-list)

(declaim (ftype (function (list) fixnum) find-longest-string))
(defun find-longest-string (list)
  (reduce (lambda (longest string)
            (max longest (length (format nil "~a" string))))
          list :initial-value 0))

(declaim (ftype (function (list) list) extract-columns)
         (inline extract-columns))
(defun extract-columns (list)
  (apply #'mapcar #'list list))

(declaim (ftype (function (list) null) print-table))
(defun print-table (table)
  (loop with widths = (butlast
                       (map 'list #'find-longest-string (extract-columns table)))
        for line in table
        do (loop initially (format t "~&")
                 for item in (butlast line)
                 for width in widths
                 for tab-position = (1+ width) then (+ tab-position width 1)
                 do (format t (format nil "~~a~~~a,0t" tab-position)
                            item)
                 finally (format t "~a" (car (last line))))
        finally (format t "~%")
                (finish-output)))

(declaim (ftype (function (list
                           &optional (or pathname string))
                          (or pathname string))
                make-package-list-file))
(defun make-package-list-file (package-list
                               &optional (output-path "./misc/l-package-list"))
  (with-open-file (stream output-path :direction :output :if-exists :supersede)
    (prin1 package-list stream))
  output-path)

(declaim (ftype (function (&optional (or pathname string)) list)
                read-package-list-file))
(defun read-package-list-file (&optional (filepath "misc/l-package-list"))
  (with-open-file (stream filepath :direction :input :if-does-not-exist :error)
    (read stream)))

(declaim (ftype (function () list) package-dispatch))
(defun package-dispatch ()
  (loop named dispatch
        with package-list-file = "misc/l-package-list"
        and package-file = "misc/package-list"
        with package-list = (if (probe-file package-list-file)
                                (read-package-list-file package-list-file)
                                (make-package-list package-file))
        with package-max-index = (1- (list-length package-list))
        with package-per-page = 10
        with current-page = 0
        and package-max-page = (1- (ceiling (list-length package-list)
                                            package-per-page))
        and key-list = (loop for i from 97 below (+ package-per-page 97)
                             collect (code-char i))
        for current-sublist = (subseq package-list
                                      (* package-per-page current-page)
                                      (min (1+ package-max-index)
                                           (* package-per-page
                                              (1+ current-page))))
        for current-table = (cons '("key" "toggle" "name" "description")
                                  (make-keyed-list current-sublist))
        for user-input = (progn (clear-screen)
                                (print-table current-table)
                                (clear-input)
                                (read-char))
        do (cond
             ((member user-input key-list)
              (let ((index (min package-max-index
                                (+ (char-code user-input) -97
                                   (* current-page package-per-page)))))
                (setf package-list
                      (toggle-package-install index package-list))))
             (t
              (case user-input
                (#\Q (return-from dispatch package-list))
                (#\N (setf current-page (min package-max-page
                                             (1+ current-page))))
                (#\P (setf current-page (max 0 (1- current-page))))
                (#\I
                 (install-packages (remove-if-not #'car package-list))
                 (format t "Press RETURN/ENTER to continue...")
                 (finish-output)
                 (clear-input)
                 (read-char))
                (#\M (setf package-list (map 'list
                                             (lambda (x &aux (x (copy-list x)))
                                               (setf (car x) t)
                                               x)
                                             package-list)))
                (#\U (setf package-list (map 'list
                                             (lambda (x &aux (x (copy-list x)))
                                               (setf (car x) nil)
                                               x)
                                             package-list)))
                (t nil))))))
