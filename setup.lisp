;;;; Copyright (C) 2023  CToID

;;;; Copying and distribution of this file, with or without modification,
;;;; are permitted in any medium without royalty provided the copyright
;;;; notice and this notice are preserved.  This file is offered as-is,
;;;; without any warranty.

(ql:quickload '("uiop" "cl-ppcre"))

(declaim (ftype (function () null) clear-screen)
         (inline clear-screen))
(defun clear-screen ()
  (format t "~c[2J" (code-char 27)))

(declaim (ftype (function (string symbol) t)
                get-description))
(defun get-description (name type)
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

(declaim (ftype (function ((or string pathname) symbol) list)
                make-list-from-file))
(defun make-list-from-file (file-path type)
  (case type
    ((xbps flatpak)
       (map 'list (lambda (x)
                    (list t x (get-description x type)))
            (uiop:read-file-lines file-path)))
    ((config-files system-services)
       (with-open-file (stream file-path)
         (map 'list (lambda (x)
                      (cons t x))
              (cdr (assoc type (read stream))))))
    (t (error "Unknown type"))))

(declaim (ftype (function (list &optional fixnum) list) make-keyed-list))
(defun make-keyed-list (list &optional (from 97))
  (loop for i in list
        for char-code = from then (1+ char-code)
        collect (cons (code-char char-code) i)))

(defmacro beautify-list-booleans (list key &optional (yes-no '("v" . " ")))
  `(map 'list (lambda (entry &aux (entry (copy-tree entry)))
                (setf (,key entry) (if (,key entry)
                                     ,(car yes-no)
                                     ,(cdr yes-no)))
                entry)
        ,list))

(declaim (ftype (function (list symbol) t) install-packages))
(defun install-packages (package-list type)
  (uiop:run-program (append (case type
                              (xbps '("sudo" "xbps-install" "-n"))
                              (flatpak '("flatpak" "install" "flathub"))
                              (t (error "Unknown package type")))
                            (map 'list #'cadr package-list))
                    :ignore-error-status t
                    :input :interactive
                    :output :interactive
                    :error-output :interactive))

(declaim (ftype (function (list) t) make-symlinks))
(defun make-symlinks (list)
  (loop for (_ from to need-root) in list
        do (uiop:run-program (if need-root
                               (list "sudo" "ln" "-srf" from to)
                               (list "ln" "-srf" from to))
                             :ignore-error-status t
                             :input :interactive
                             :output :interactive
                             :error-output :interactive)))

(declaim (ftype (function (fixnum list) list) toggle-item))
(defun toggle-item (index list
                    &aux (local-list (copy-tree list)))
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
                       (map 'list #'find-longest-string
                            (extract-columns table)))
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

(declaim (ftype (function (list symbol &optional (or string pathname))
                          (or pathname string))
                make-list-file))
(defun make-list-file (list type
                       &optional
                       (output-path (case type
                                      (xbps "misc/l-package-list")
                                      (flatpak "misc/l-flatpak-list")
                                      (t (error (format nil "~{~a~}"
                                                        '("You need to specify "
                                                          "a path for non-xbps "
                                                          "or non-flatpak "
                                                          "list")))))))
  (with-open-file (stream output-path :direction :output :if-exists :supersede)
    (prin1 list stream))
  output-path)

(declaim (ftype (function ((or pathname string)) list)
                read-list-file))
(defun read-list-file (filepath)
  (with-open-file (stream filepath :direction :input :if-does-not-exist :error)
    (read stream)))

(declaim (ftype (function (symbol) list) package-symlink-dispatch))
(defun package-symlink-dispatch (type)
  (loop named dispatch
        with list-file = (case type
                           (xbps "misc/l-package-list")
                           (flatpak "misc/l-flatpak-list")
                           ((config-files system-services) nil)
                           (t (error "Unknown type")))
        and list-source-file = (case type
                                 (xbps "misc/package-list")
                                 (flatpak "misc/flatpak-list")
                                 ((config-files system-services)
                                    "misc/symlink-service-list")
                                 (t (error "Unknown type")))
        and first-row = (case type
                          ((xbps flatpak)
                             '("key" "toggle" "name" "description"))
                          ((config-files system-services)
                             '("key" "toggle" "from" "to" "need-root"))
                          (t (error "Unknown type")))
        with list = (if (and list-file (probe-file list-file))
                      (read-list-file list-file)
                      (make-list-from-file list-source-file type))
        with items-per-page = 10
        and current-page = 0
        and list-max-index = (1- (list-length list))
        with key-list = (loop for i from 97 below (+ items-per-page 97)
                              collect (code-char i))
        and max-page = (1- (ceiling (list-length list) items-per-page))
        for current-sublist = (subseq list
                                      (* items-per-page current-page)
                                      (min (1+ list-max-index)
                                           (* items-per-page
                                              (1+ current-page))))
        for current-table = (cons first-row
                                  (make-keyed-list
                                   (case type
                                     ((xbps flatpak)
                                        (beautify-list-booleans
                                            current-sublist
                                            car))
                                     ((config-files system-services)
                                        (beautify-list-booleans
                                            (beautify-list-booleans
                                                current-sublist
                                                car)
                                            cadddr
                                            ("yes" . "no")))
                                     (t (error "Unknown type")))))
        for user-input = (progn (clear-screen)
                                (print-table current-table)
                                (clear-input)
                                (read-char))
        do (cond
             ((member user-input key-list)
              (let ((index (min list-max-index
                                (+ (char-code user-input) -97
                                   (* current-page items-per-page)))))
                (setf list (toggle-item index list))))
             (t
              (case user-input
                (#\Q
                   (return-from dispatch list))
                (#\N
                   (setf current-page (min max-page
                                           (1+ current-page))))
                (#\P
                   (setf current-page (max 0 (1- current-page))))
                (#\I
                   (case type
                     ((xbps flatpak)
                        (install-packages (remove-if-not #'car list) type))
                     ((config-files system-services)
                        (make-symlinks (remove-if-not #'car list)))
                     (t (error "Unknown type")))
                   (format t "Press RETURN/ENTER to continue...")
                   (finish-output)
                   (clear-input)
                   (read-char))
                (#\M
                   (setf list (map 'list
                                   (lambda (x &aux (x (copy-list x)))
                                     (setf (car x) t)
                                     x)
                                   list)))
                (#\U
                   (setf list (map 'list
                                   (lambda (x &aux (x (copy-list x)))
                                     (setf (car x) nil)
                                     x)
                                   list)))
                (t nil))))))
