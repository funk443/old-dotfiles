;; Copyright (C) 2023 CToID
;;
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(REQUIRE 'ASDF)
(REQUIRE 'SB-POSIX)

(FORMAT T "~&~C[2J~%" (CODE-CHAR 27))
(FORMAT T "TYPE `(MAIN)' TO START INSTALLING, `(QUIT)' OR `(EXIT)' TO QUIT")

(DEFMACRO INTERACTIVE-RUN (PROGRAM &REST ARGS)
  `(UIOP:RUN-PROGRAM ,PROGRAM
                     :INPUT :INTERACTIVE
                     :OUTPUT :INTERACTIVE
                     :ERROR-OUTPUT :INTERACTIVE
                     ,@ARGS))

(DEFUN CHECK-ROOT ()
  (= (PARSE-INTEGER
      (UIOP:RUN-PROGRAM '("id" "-u") :OUTPUT '(:STRING :STRIPPED T)))
     0))

(DEFUN CLEAR-SCREEN ()
  (FORMAT T "~&~C[2J~%" (CODE-CHAR 27)))

(DEFUN MAKE-PACKAGE-LIST (PACKAGE-FILE)
  (DECLARE (TYPE STRING PACKAGE-FILE))
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
  (DECLARE (TYPE LIST PACKAGE-LIST))
  (FORMAT T "~&~A~%|KEY|?|PACKAGE NAME~29T|DESCRIPTION~79T|~%~A~%"
          (MAKE-STRING 80 :INITIAL-ELEMENT #\=)
          (MAKE-STRING 80 :INITIAL-ELEMENT #\=))
  (DOTIMES (I (LENGTH PACKAGE-LIST))
    (LET ((PACKAGE (NTH I PACKAGE-LIST)))
      (IF (> (LENGTH (NTH 1 PACKAGE)) 22)
          (SETF PACKAGE (COPY-SEQ PACKAGE)
                (NTH 1 PACKAGE) (CONCATENATE 'STRING
                                             (SUBSEQ (NTH 1 PACKAGE)
                                                     0 19)
                                             "...")))
      (IF (> (LENGTH (NTH 2 PACKAGE)) 49)
          (SETF PACKAGE (COPY-SEQ PACKAGE)
                (NTH 2 PACKAGE) (CONCATENATE 'STRING
                                             (SUBSEQ (NTH 2 PACKAGE)
                                                     0 46)
                                             "...")))
      (FORMAT T "|~A~4,0T|~A~6,0T|~A~29,0T|~A~79,0T|~%"
              (CODE-CHAR (+ 97 I))
              (IF (NTH 0 PACKAGE)
                  "V"
                  "")
              (NTH 1 PACKAGE)
              (NTH 2 PACKAGE)))))

(DEFUN PACKAGE-OPERATIONS (PACKAGE-LIST FLATPAK)
  (DECLARE (TYPE LIST PACKAGE-LIST)
           (TYPE BOOLEAN FLATPAK))
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
      (FORMAT T "<a> ~~ <j> TO SELECT PACKAGES~%")
      (FORMAT T "<Q> TO QUIT, <I> TO INSTALL SELECTED PACKAGES~%")
      (FORMAT T "<M> TO MARK ALL PACKAGES TO INSTALL, <U> TO UNMARK ALL~%")
      (FORMAT T "<N> TO GO TO THE NEXT PAGE, <P> FOR PREVIOUS PAGE~%")
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
           (INTERACTIVE-RUN (IF FLATPAK
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
                                                        :KEY #'CAR)))))
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
             (SETF (NTH 0 (NTH (MIN (+ (- (CHAR-CODE USER-INPUT) 97)
                                     (* CURRENT-PAGE
                                        PACKAGES-PER-PAGE))
                                    (1- (LENGTH PACKAGE-LIST)))
                               PACKAGE-LIST))
                   (NOT (NTH 0 (NTH (MIN (+ (- (CHAR-CODE USER-INPUT) 97)
                                          (* CURRENT-PAGE
                                             PACKAGES-PER-PAGE))
                                         (1- (LENGTH PACKAGE-LIST)))
                                    PACKAGE-LIST)))))))))))

(DEFUN MAKE-FLATPAK-LIST (FLATPAK-FILE)
  (DECLARE (TYPE STRING FLATPAK-FILE))
  (INTERACTIVE-RUN '("flatpak" "remote-add"
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

(DEFUN ENABLE-SERVICES (SERVICE-FILE)
  (DECLARE (TYPE STRING SERVICE-FILE))
  (LET ((SERVICE-LIST (MAP 'LIST
                           (LAMBDA (X)
                             (CONCATENATE 'STRING
                                          "/etc/sv/"
                                          X))
                           (UIOP:READ-FILE-LINES SERVICE-FILE))))
    (LOOP FOR SERVICE IN SERVICE-LIST
          IF (Y-OR-N-P "ENABLE THIS SERVICE?~%~A" SERVICE)
            DO (INTERACTIVE-RUN (LIST "sudo" "ln" "-s"
                                      SERVICE "/var/service")
                                :IGNORE-ERROR-STATUS T))))

(DEFUN MAKE-SYMLINK (SYMLINK-FILE)
  (DECLARE (TYPE STRING SYMLINK-FILE))
  (LET ((SYMLINK-LIST (WITH-OPEN-FILE (S SYMLINK-FILE)
                        (READ S))))
    (LOOP FOR SYMLINK IN SYMLINK-LIST
          UNLESS (CDDR SYMLINK)
            DO (ENSURE-DIRECTORIES-EXIST (CADR SYMLINK))
          IF (Y-OR-N-P "MAKE THIS SYMLINK?~%~A -> ~A"
                       (CAR SYMLINK) (CADR SYMLINK))
            DO (INTERACTIVE-RUN (FORMAT NIL (IF (CDDR SYMLINK)
                                                "sudo ln -rs ~A ~A"
                                                "ln -rs ~A ~A")
                                        (CAR SYMLINK) (CADR SYMLINK))
                                :IGNORE-ERROR-STATUS T))))

(DEFUN MAKE-MEDIA-DIRECTORIES ()
  (LET ((DIRECTORIES (MAP 'LIST
                          (LAMBDA (X)
                            (CONCATENATE 'STRING
                                         "~/"
                                         X))
                          '("Downloads/"
                            "Videos/"
                            "Music/"
                            "Documents/"))))
    (WHEN (Y-OR-N-P "MAKE THESE DIRECTORES?~%~A"
                    DIRECTORIES)
      (LOOP FOR DIRECTORY IN DIRECTORIES
            DO (ENSURE-DIRECTORIES-EXIST DIRECTORY)))))

(DEFUN MAKE-INSTALL-EMACS (&OPTIONAL (DIRECTORY "~/Documents/git/emacs"))
  (DECLARE (TYPE STRING DIRECTORY))
  (LET ((BUILD-DEPENDENCIES '("gtk+3-devel" "libmagick-devel"
                              "webkit2gtk-devel" "libgccjit-devel"
                              "libXpm-devel" "gnutls-devel"
                              "jansson-devel" "tree-sitter-devel"
                              "giflib-devel" "ncurses-devel"
                              "libwebp-devel" "librsvg-devel"))
        (CONFIGURE-ARGS '("--with-x-toolkit=yes" "--with-imagemagick"
                          "--with-json" "--with-tree-sitter"
                          "--with-xwidgets" "--with-native-compilation=yes"
                          "--prefix=/usr"))
        (BRANCH "emacs-29")
        (CURRENT-DIRECTORY (UIOP/OS:GETCWD)))
    (UNLESS (EQUAL #\/ (AREF DIRECTORY (1- (LENGTH DIRECTORY))))
      (SETF DIRECTORY (CONCATENATE 'STRING DIRECTORY "/")))
    (ENSURE-DIRECTORIES-EXIST DIRECTORY)
    (UIOP:CHDIR DIRECTORY)
    (INTERACTIVE-RUN (LIST "git" "clone" "-b" BRANCH "--single-branch"
                           "https://github.com/emacs-mirror/emacs.git"
                           "."))
    (INTERACTIVE-RUN (APPEND '("sudo" "xbps-install")
                             BUILD-DEPENDENCIES))
    (INTERACTIVE-RUN '("sh" "autogen.sh"))
    (INTERACTIVE-RUN (APPEND '("sh" "configure")
                             CONFIGURE-ARGS))
    (INTERACTIVE-RUN "make -j$(nproc)")
    (INTERACTIVE-RUN (APPEND '("sudo" "xbps-remove" "-Ro")
                             BUILD-DEPENDENCIES))
    (INTERACTIVE-RUN '("sudo" "xbps-remove" "-Ro"))
    (UIOP:CHDIR CURRENT-DIRECTORY)))

(DEFUN MAKE-INSTALL-CHEWING (&OPTIONAL (DIRECTORY
                                        "~/Documents/git/ibus-chewing"))
  (DECLARE (TYPE STRING DIRECTORY))
  (LET ((BUILD-DEPENDENCIES '("libchewing-devel" "ibus-devel" "gob2"
                              "libX11-devel" "gtk+3-devel" "glib-devel"
                              "cmake"))
        (CURRENT-DIRECTORY (UIOP/OS:GETCWD)))
    (UNLESS (EQUAL #\/ (AREF DIRECTORY (1- (LENGTH DIRECTORY))))
      (SETF DIRECTORY (CONCATENATE 'STRING DIRECTORY "/")))
    (ENSURE-DIRECTORIES-EXIST DIRECTORY)
    (UIOP:CHDIR DIRECTORY)
    (INTERACTIVE-RUN '("git" "clone"
                       "https://github.com/definite/ibus-chewing.git"
                       "."))
    (INTERACTIVE-RUN '("git" "submodule" "init"))
    (INTERACTIVE-RUN '("git" "submodule" "update"))
    (INTERACTIVE-RUN (APPEND '("sudo" "xbps-install")
                             BUILD-DEPENDENCIES))
    (INTERACTIVE-RUN '("cmake" "."
                       "-DCMAKE_INSTALL_PREFIX='/usr'"
                       "-DLIBEXEC_DIR='/usr/libexec'"))
    (INTERACTIVE-RUN "make -j$(nproc)")
    (INTERACTIVE-RUN '("sudo" "make" "install"))
    (INTERACTIVE-RUN (APPEND '("sudo" "xbps-remove" "-Ro")
                             BUILD-DEPENDENCIES))
    (INTERACTIVE-RUN '("sudo" "xbps-remove" "-Ro"))
    (UIOP:CHDIR CURRENT-DIRECTORY)))

(DEFUN MAIN ()
  (CLEAR-SCREEN)
  (WHEN (CHECK-ROOT)
    (CLEAR-SCREEN)
    (FORMAT T "YOU SHOULD NOT RUN THIS AS ROOT!~%")
    (SLEEP 3)
    (EXIT :CODE 1))
  (LET ((SERVICE-FILE "misc/service-list")
        (SYMLINK-FILE "misc/symlink-list")
        (PACKAGE-FILE "misc/package-list")
        (FLATPAK-FILE "misc/flatpak-list"))
    (WHEN (Y-OR-N-P "CONFIGURE PACKAGES?")
      (FORMAT T "INITIALIZING PACKAGE LIST, THIS MIGHT TAKE A WHILE~%")
      (LET ((PACKAGE-LIST (MAKE-PACKAGE-LIST PACKAGE-FILE)))
        (PACKAGE-OPERATIONS PACKAGE-LIST NIL)))
    (WHEN (Y-OR-N-P "CONFIGURE FLATPAKS?")
      (FORMAT T "INITIALIZING FLATPAK LIST, THIS MIGHT TAKE A WHILE~%")
      (LET ((FLATPAK-LIST (MAKE-FLATPAK-LIST FLATPAK-FILE)))
        (PACKAGE-OPERATIONS FLATPAK-LIST T)))
    (WHEN (Y-OR-N-P "CONFIGURE SYSTEM SERVICES?")
      (ENABLE-SERVICES SERVICE-FILE))
    (WHEN (Y-OR-N-P "CONFIGURE CONFIG FILE SYMLINKS?")
      (MAKE-SYMLINK SYMLINK-FILE))
    (WHEN (Y-OR-N-P "CREATE MEDIA DIRECTORIES?")
      (MAKE-MEDIA-DIRECTORIES))
    (WHEN (Y-OR-N-P "BUILD EMACS?")
      (IF (Y-OR-N-P "DO YOU WANT TO CLONE THE REPO TO CUSTOM LOCATION?
DEFAULT LOCATION IS: `~~/Documents/git/emacs'")
          (PROGN (FORMAT T "ENTER THE DIRECTORY PATH:~%")
                 (MAKE-INSTALL-EMACS (READ-LINE)))
          (MAKE-INSTALL-EMACS)))
    (WHEN (Y-OR-N-P "BUILD IBUS CHEWING?")
      (IF (Y-OR-N-P "DO YOU WANT TO CLONE THE REPO TO CUSTOM LOCATION?
DEFAULT LOCATION IS: `~~/Documents/git/ibus-chewing'")
          (PROGN (FORMAT T "ENTER THE DIRECTORY PATH:~%")
                 (MAKE-INSTALL-CHEWING (READ-LINE)))
          (MAKE-INSTALL-CHEWING))))
  (EXIT :CODE 0))

(UNLESS (MEMBER "--no-main" (UIOP:COMMAND-LINE-ARGUMENTS)
                :TEST #'STRING-EQUAL)
  (MAIN))
