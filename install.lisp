(REQUIRE 'ASDF)

(DEFUN CHECK-ROOT ()
  (= (PARSE-INTEGER
      (UIOP:RUN-PROGRAM '("id" "-u") :OUTPUT '(:STRING :STRIPPED T)))
     0))

(DEFUN CLEAR-SCREEN ()
  (FORMAT T "~C[2J" (CODE-CHAR 27)))

(DEFUN MAKE-PACKAGE-LIST (PACKAGE-FILE)
  (LET ((PACKAGE-NAME-LIST (UIOP:READ-FILE-LINES PACKAGE-FILE)))
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
         PACKAGE-NAME-LIST)))

(DEFUN DISPLAY-PACKAGE-TABLE (PACKAGE-LIST)
  (FORMAT T "~&~A~%|KEY|?|PACKAGE NAME~29T|DESCRIPTION~79T|~%~A~%"
          (MAKE-STRING 80 :INITIAL-ELEMENT #\=)
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

(DEFUN PACKAGES-OPERATIONS (PACKAGE-LIST FLATPAK)
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
          (UIOP:RUN-PROGRAM (IF FLATPAK
                                (APPEND '("flatpak" "install")
                                        (OR (MAP 'LIST
                                                 (LAMBDA (X)
                                                   (FOURTH X))
                                                 (REMOVE-IF #'NOT
                                                            PACKAGE-LIST
                                                            :KEY #'CAR))
                                            '("--help")))
                                (APPEND '("sudo" "xbps-install" "-S")
                                        (MAP 'LIST
                                         (LAMBDA (X)
                                           (SECOND X))
                                         (REMOVE-IF #'NOT
                                                    PACKAGE-LIST
                                                    :KEY #'CAR))))
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

(DEFUN MAKE-FLATPAK-LIST (FLATPAK-FILE)
  (UIOP:RUN-PROGRAM '("flatpak" "remote-add"
                      "--if-not-exists"
                      "flathub"
                      "https://flathub.org/repo/flathub.flatpakrepo"))
  (LET ((FLATPAK-NAME-LIST (UIOP:READ-FILE-LINES FLATPAK-FILE)))
    (MAP 'LIST
         (LAMBDA (FLATPAK-NAME)
           (LIST T
                 (UIOP:RUN-PROGRAM '("head" "-n1")
                  :INPUT
                  (UIOP:PROCESS-INFO-OUTPUT
                   (UIOP:LAUNCH-PROGRAM '("tail" "-n+1")
                                        :INPUT
                                        (UIOP:PROCESS-INFO-OUTPUT
                                         (UIOP:LAUNCH-PROGRAM
                                          (LIST "flatpak"
                                                "search"
                                                "--columns=name"
                                                FLATPAK-NAME)
                                          :OUTPUT :STREAM))
                                        :OUTPUT :STREAM))
                  :OUTPUT '(:STRING :STRIPPED T))
                 (UIOP:RUN-PROGRAM '("head" "-n1")
                  :INPUT
                  (UIOP:PROCESS-INFO-OUTPUT
                   (UIOP:LAUNCH-PROGRAM '("tail" "-n+1")
                                        :INPUT
                                        (UIOP:PROCESS-INFO-OUTPUT
                                         (UIOP:LAUNCH-PROGRAM
                                          (LIST "flatpak"
                                                "search"
                                                "--columns=description"
                                                FLATPAK-NAME)
                                          :OUTPUT :STREAM))
                                        :OUTPUT :STREAM))
                  :OUTPUT '(:STRING :STRIPPED T))
                 FLATPAK-NAME))
         FLATPAK-NAME-LIST)))
