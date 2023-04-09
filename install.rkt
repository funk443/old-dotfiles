#lang racket
(require text-table)

(define (clear-screen) (display "\e[2J"))

(define (slice-list llist start end)
  (let loop ((llist llist) (start start) (end end) (result '()))
    (cond
      ((or (null? llist)
           (zero? end))
       (reverse result))
      ((zero? start) (loop (cdr llist) 0 (sub1 end) (cons (car llist) result)))
      (#t (loop (cdr llist) (sub1 start) (sub1 end) result)))))

(define (y-or-n?)
  (printf "(y or n)")
  (match (read-line)
    ((regexp #rx"y|Y") #t)
    ((regexp #rx"n|N") #f)
    (_ (y-or-n?))))

(define (make-package-list list-file)
  (map (lambda (x)
         (list #t x
               (string-trim
                (with-output-to-string
                  (lambda ()
                    (system (format "xbps-query -p short_desc ~a" x)))))))
       (file->lines list-file)))

(define (make-list-with-key llist (start-from 97))
  (reverse (car (foldl
                 (lambda (x storage)
                   (match storage
                     ((cons result char-code)
                      (cons (cons (cons (integer->char char-code) x) result)
                            (add1 char-code)))))
                 `(() . ,start-from) llist))))

(define (install-packages package-list)
  (system (format "sudo xbps-install -n ~a"
                  (string-trim (~a package-list)
                               (regexp "\\(|\\)")))))

(define (toggle-package-install package-list index)
  (list-update package-list index (lambda (sublist)
                                    (list-update sublist 0 not))))

(define (display-package-table package-list
                               (top-row '("key" "?" "name" "description"))
                               (alignment '(center left left left)))
  (print-table (cons top-row package-list) #:align alignment))

(define (package-dispatch package-list)
  (define packages-per-page 10)
  (define package-max-page (sub1 (ceiling (/ (length package-list)
                                             packages-per-page))))
  (define package-max-index (sub1 (length package-list)))
  (let loop ((package-list package-list) (current-page 0))
    (clear-screen)
    (display-package-table (make-list-with-key
                            (slice-list package-list
                                        (* current-page packages-per-page)
                                        (+ (* current-page
                                              packages-per-page)
                                           10))))
    (define user-input (read-char))
    (cond
      ((member user-input (build-list 10 (lambda (x) (integer->char (+ x 97)))))
       (define toggle-index (+ (- (char->integer user-input) 97)
                               (* current-page 10)))
       (loop (toggle-package-install package-list (min toggle-index
                                                       package-max-index))
             current-page))
      (#t (match user-input
            (#\N (loop package-list (min (add1 current-page) package-max-page)))
            (#\P (loop package-list (max (sub1 current-page) 0)))
            ((or #\M #\U)
             (define mark (equal? user-input #\M))
             (loop (map (lambda (package)
                          (match package
                            ((list _ name description)
                             (list mark name description))))
                        package-list)
                   current-page))
            (#\I
             (install-packages (map cadr (filter car package-list))))
            (#\Q package-list)
            (_ (loop package-list current-page)))))))

(define (make-package-list-file filepath
                                (output-file-path
                                 (expand-user-path
                                  "~/dotfiles/misc/r-package-list")))
  (with-output-to-file output-file-path
    (lambda ()
      (write (make-package-list filepath)))
    #:exists 'replace))
