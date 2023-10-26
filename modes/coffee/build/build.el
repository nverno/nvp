;; -*- no-byte-compile: t; lexical-binding: t; -*-
(require 'json)

(setq winpy "C:/Program Files/Anaconda/python.exe")

;; why am I making a hash then converting to alist?!
(defun build-hash (file)
  (let* ((json-key-type 'string)
         (lst (json-read-file file))
         (ht (make-hash-table :test 'equal :size (length lst))))
    (mapc (lambda (item) (puthash (car item) (cdr item) ht)) lst)
    ht))

(defun hash-to-alist (hash)
  (let (res)
    (maphash
     (lambda (key value)
       (setq res (cons (cons key value) res)))
     hash)
    res))

(defun save-hash (hash file)
  (with-temp-buffer
    (let (print-level print-length)
      (insert (pp-to-string (hash-to-alist hash)))
      (write-region (point-min) (point-max) file))))

;; scrape
(defun scrape-data ()
  (let ((proc (start-process
               "coffeescrape" "*scraping*"
               (if (eq system-type 'windows-nt) winpy "python")
               "index.py")))
    (set-process-sentinel proc #'(lambda (p s)
                                   (message "%s : %s" p s)
                                   (build-toc)))))

(defun build-toc ()
  (save-hash (build-hash "index.json") (expand-file-name "index.el" "..")))
