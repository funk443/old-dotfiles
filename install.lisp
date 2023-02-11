;;    ______ ______        ____ ____ 
;;   / ____//_  __/____   /  _// __ \
;;  / /      / /  / __ \  / / / / / /
;; / /___   / /  / /_/ /_/ / / /_/ / 
;; \____/  /_/   \____//___//_____/  
;; (require 'uiop)
;; (format t "~c[2J" #\Esc)
;; (format t "If you are not using executable, type (main) to start the installation")

;; (defun main ()
;;   (format t "~c[2J" #\Esc)
;;   (format t "Initialising, this might take a while...~%")
;;   (let ((pkg-list (make-pkg-list)))
;;     (loop
;;      (make-screen)
;;      (let ((usr-input (read-char)))
;;        (cond ((equal usr-input #\p)
;;               (setf pkg-list (select-pkgs pkg-list)))
;;              ((equal usr-input #\q)
;;               (format t "~c[2J" #\Esc)
;;               (format t "Bye~~~%")
;;               (sleep 1)
;;               (quit))
;;              (t nil))))))

;; ;; Package Related Functions
;; (defun select-pkgs (pkgs)
;;   (let* ((cur-page 0)
;;          (per-page 10)
;;          (total-pages (1- (ceiling (length pkgs) per-page))))
;;     (loop
;;      (format t "~c[2J" #\Esc)
;;      (let* ((first-item (+ 0 (* cur-page per-page)))
;;             (last-item (+ first-item per-page))
;;             (cur-page-list (subseq* pkgs first-item last-item))
;;             (keys-list "abcdefghijklm"))
;;        (dotimes (i (length cur-page-list))
;;          (let ((pkg (elt cur-page-list i))
;;                (key (elt keys-list i)))
;;            (format t "~&~a~T| ~a~2T| ~a~30T| ~a~%"
;;                    key
;;                    (elt pkg 0)
;;                    (elt pkg 1)
;;                    (elt pkg 2))))
;;        (format t "===== ~a / ~a =====~%" cur-page total-pages)
;;        (format t "T means yes, NIL means no~%")
;;        (format t "<a> ~~ <~a> Toggle installation of a package~%"
;;                (elt keys-list (1- (length cur-page-list))))
;;        (format t "<o> Install selected packages~%")
;;        (format t "<+> Select all packages, <-> Deselect all packages~%")
;;        (format t "<n> next page, <p> previous page, <q> Return to menu~%")
;;        (let ((usr-input (read-char)))
;;          (cond ((position usr-input keys-list :test #'equal)
;;                 (let ((i (position usr-input keys-list :test #'equal)))
;;                   (when (< i (length cur-page-list))
;;                     (if (elt (elt pkgs (+ first-item i)) 0)
;;                         (setf (elt (elt pkgs (+ first-item i)) 0) nil)
;;                         (setf (elt (elt pkgs (+ first-item i)) 0) t)))))
;;                ((equal usr-input #\o)
;;                 (install-pkgs pkgs)
;;                 (sleep 5))
;;                ((equal usr-input #\-)
;;                 (map 'nil #'(lambda (x)
;;                               (setf (elt x 0) nil))
;;                      pkgs))
;;                ((equal usr-input #\+)
;;                 (map 'nil #'(lambda (x)
;;                               (setf (elt x 0) t))
;;                      pkgs))
;;                ((equal usr-input #\n)
;;                 (when (< cur-page total-pages)
;;                   (setf cur-page (1+ cur-page))))
;;                ((equal usr-input #\p)
;;                 (when (> cur-page 0)
;;                   (setf cur-page (1- cur-page))))
;;                ((equal usr-input #\q)
;;                 (return-from select-pkgs pkgs))
;;                (t nil)))))))

;; (defun make-pkg-desc (pkg-name)
;;   (uiop:run-program (list "xbps-query" "-p" "short_desc" "--repository" pkg-name)
;;                     :output '(:string :stripped t)))

;; (defun make-pkg-list ()
;;   (let ((pkgs (with-open-file (stream "./void-packages.txt")
;;                 (uiop:read-file-lines stream)))
;;         (pkg-list nil))
;;     (dolist (pkg pkgs)
;;       (push (list t pkg (make-pkg-desc pkg)) pkg-list))
;;     (reverse pkg-list)))

;; (defun install-pkgs (pkgs)
;;   (let ((pkg-names (map 'list #'(lambda (x)
;;                                   (elt x 1))
;;                         (remove-if-not #'(lambda (x)
;;                                            (elt x 0))
;;                                        pkgs))))
;;     (format t "~c[2J" #\Esc)
;;     (format t "~&Packages to install:~%~a~%" pkg-names)
;;     (unless (y-or-n-p "Really install these packages?")
;;       (format t "Aborting...")
;;       (return-from install-pkgs))
;;     (uiop:run-program (list "xbps-install" "-S") :output t :error-output t)
;;     (if (check-root)
;;         (uiop:run-program (append (list "xbps-install" "-y") pkg-names) :output t :error-output t)
;;         (format t "~&YOU NEED TO RUN THIS PROGRAM AS ROOT TO INSTALL PACKAGES!~%"))))

;; ;; Utility Functions
;; (defun subseq* (lst start end)
;;   (if (>= end (length lst))
;;       (nthcdr start lst)
;;       (subseq lst start end)))

;; (defun make-screen ()
;;   (format t "~c[2J" #\Esc)
;;   (format t "~&   ______ ______        ____ ____  ")
;;   (format t "~&  / ____//_  __/____   /  _// __ \\ ")
;;   (format t "~& / /      / /  / __ \\  / / / / / / ")
;;   (format t "~&/ /___   / /  / /_/ /_/ / / /_/ /  ")
;;   (format t "~&\\____/  /_/   \\____//___//_____/ ~%")
;;   (format t "======~%")
;;   (format t " <p> Packages~%")
;;   (format t " <q> Quit~%"))

;; (defun check-root ()
;;   (= (parse-integer
;;       (uiop:run-program (list "id" "-u") :output '(:string :stripped t)))
;;      0))

;;;=====================================

(REQUIRE 'ASDF)

(DEFUN CHECK-ROOT ()
  (= (PARSE-INTEGER
      (UIOP:RUN-PROGRAM '("id" "-u") :OUTPUT '(:STRING :STRIPPED T)))
     0))

(DEFUN CLEAR-SCREEN ()
  (FORMAT T "~C[2J" (CODE-CHAR 27)))

(DEFUN MAKE-PACKAGE-LIST (PACKAGE-FILE)
  (LET* ((PACKAGE-NAME-LIST (UIOP:READ-FILE-LINES PACKAGE-FILE))
         (PACKAGE-NAME-LIST (MAKE-ARRAY (LENGTH PACKAGE-NAME-LIST)
                                        :INITIAL-CONTENTS
                                        PACKAGE-NAME-LIST)))
    (MAKE-ARRAY (LENGTH PACKAGE-NAME-LIST)
                :INITIAL-CONTENTS
                (MAP 'LIST
                     (LAMBDA (PACKAGE-NAME)
                       (LIST T PACKAGE-NAME (UIOP:RUN-PROGRAM
                                             (LIST "xbps-query"
                                                   "-p"
                                                   "short_desc"
                                                   PACKAGE-NAME
                                                   "--repository")
                                             :OUTPUT
                                             '(:STRING :STRIPPED T))))
                     PACKAGE-NAME-LIST))))

(DEFUN DISPLAY-PACKAGE-TABLE (PACKAGE-LIST)
  (FORMAT T "~&|KEY|?|PACKAGE NAME~29T|DESCRIPTION~79T|~%~A~%"
          (MAKE-STRING 80 :INITIAL-ELEMENT #\=))
  (DOTIMES (I (LENGTH PACKAGE-LIST))
    (LET ((PACKAGE (ELT PACKAGE-LIST I)))
      (IF (> (LENGTH (ELT PACKAGE 1)) 22)
          (SETF PACKAGE (COPY-SEQ PACKAGE)
                (ELT PACKAGE 1) (CONCATENATE 'STRING
                                             (SUBSEQ (ELT PACKAGE 1)
                                                     0 19)
                                             "...")))
      (IF (> (LENGTH (ELT PACKAGE 2)) 49)
          (SETF PACKAGE (COPY-SEQ PACKAGE)
                (ELT PACKAGE 2) (CONCATENATE 'STRING
                                             (SUBSEQ (ELT PACKAGE 2)
                                                     0 46)
                                             "...")))
      (FORMAT T "|~A~4,0T|~A~6,0T|~A~29,0T|~A~79,0T|~%"
              (CODE-CHAR (+ 97 I))
              (IF (ELT PACKAGE 0)
                  "V"
                  "")
              (ELT PACKAGE 1)
              (ELT PACKAGE 2)))))

(DEFUN PACKAGES-OPERATIONS (PACKAGE-LIST)
  (LET* ((CURRENT-PAGE 0)
         (PACKAGES-PER-PAGE 10)
         (TOTAL-PAGES (1- (CEILING (LENGTH PACKAGE-LIST)
                                   PACKAGES-PER-PAGE))))
    (LOOP
     (CLEAR-SCREEN)
     (DISPLAY-PACKAGE-TABLE (SUBSEQ PACKAGE-LIST
                                    (* CURRENT-PAGE
                                       PACKAGES-PER-PAGE)
                                    (MIN (+ (* CURRENT-PAGE
                                               PACKAGES-PER-PAGE)
                                            10)
                                         (LENGTH PACKAGE-LIST))))
     (FORMAT T "~A~%" (MAKE-STRING 80 :INITIAL-ELEMENT #\=))
     (LET ((USER-INPUT (READ-CHAR)))
       (CASE USER-INPUT
         (#\Q
          (RETURN))
         (#\N
          (IF (< CURRENT-PAGE TOTAL-PAGES)
              (SETF CURRENT-PAGE (1+ CURRENT-PAGE))))
         (#\P
          (IF (> CURRENT-PAGE 0)
              (SETF CURRENT-PAGE (1- CURRENT-PAGE))))
         (#\I
          (CLEAR-SCREEN)
          (UIOP:RUN-PROGRAM (APPEND '("sudo" "xbps-install" "-S")
                                    (MAP 'LIST
                                         (LAMBDA (X)
                                           (SECOND X))
                                         (REMOVE-IF #'NOT
                                                    PACKAGE-LIST
                                                    :KEY #'CAR)))
                            :INPUT :INTERACTIVE
                            :OUTPUT :INTERACTIVE
                            :ERROR-OUTPUT :INTERACTIVE)
          (SLEEP 3))
         (#\M
          (MAP 'LIST
               (LAMBDA (X)
                 (SETF (FIRST X) T)
                 X)
               PACKAGE-LIST))
         (#\U
          (MAP 'LIST
               (LAMBDA (X)
                 (SETF (FIRST X) NIL)
                 X)
               PACKAGE-LIST))
         (OTHERWISE
          (WHEN (MEMBER USER-INPUT (COERCE "abcdefghij" 'LIST)
                        :TEST #'EQUAL)
            (SETF (ELT (ELT PACKAGE-LIST
                            (MIN (+ (- (CHAR-CODE USER-INPUT) 97)
                                    (* CURRENT-PAGE
                                       PACKAGES-PER-PAGE))
                                 (1- (LENGTH PACKAGE-LIST))))
                       0)
                  (NOT (ELT (ELT PACKAGE-LIST
                                 (MIN (+ (- (CHAR-CODE USER-INPUT) 97)
                                         (* CURRENT-PAGE
                                            PACKAGES-PER-PAGE))
                                      (1- (LENGTH PACKAGE-LIST))))
                            0))))))))))
      
