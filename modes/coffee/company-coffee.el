;;; company-coffee.el ---  -*- lexical-binding: t; -*- 
;;; Commentary:
;; Complete coffeescript keywords with company.
;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'company)

(defgroup company-coffee nil
  "Company completion backend for ."
  :group 'company
  :prefix "company-coffee-")

(defcustom company-coffee-modes '(coffee-mode)
  "Modes to activate `company-coffee'."
  :group 'company-coffee
  :type 'sexp)

;; ------------------------------------------------------------
;; Internal
(defvar company-keywords-alist)

(eval-when-compile 

 (defvar company-coffee-compares
   '(("is" . "===")
     ("isnt" . "!==")
     ("not" . "!")
     ("and" . "&&")
     ("or" . "||")
     ("or=" . "||= (false, \"\", 0, null)")
     ("?=" . "||= (only when null/undefined)")
     ("true" . "true")
     ("yes" . "true")
     ("on" . "true")
     ("false" . "false")
     ("no" . "false")
     ("off" . "false")))

 (defvar company-coffee-math
   '(("**" . "Math.pow")
     ("//" . "Math.floor(a, b)")
     ("%%" . "%")))

 ;; "var" "function"
 (defvar company-coffee-words
   '(
     "__extends" "__hasProp" "alert" "await" "break" "by" "case" "catch"
     "class" "const" "continue" "debugger" "default" "defer" "delete" "do"
     "else" "enum" "export" "extends" "finally" "for" "if"
     "import" "in" "instanceof" "let" "loop" "native" "new"
     "of" "or" "own" "return" "super" "switch" "then"
     "throw" "try" "typeof" "unless" "until" "void" "when" "while"
     "with" "yield" "undefined" "null"
     )))
 
(defvar company-coffee-keywords
  (eval-when-compile
    (sort
     (append
      (cl-loop for (k . v) in company-coffee-compares
         do (put-text-property 0 1 'annot v k)
         collect k)
      (cl-loop for (k . v) in company-coffee-math
         do (put-text-property 0 1 'annot v k)
         collect k)
      company-coffee-words)
     'string<)))

(defun company-coffee-prefix ()
  (and (derived-mode-p company-coffee-modes)
       (not (company-in-string-or-comment))
       (company-grab-symbol)))

(defun company-coffee-candidates (arg)
  (all-completions arg company-coffee-keywords))

(defun company-coffee-annotation (candidate)
  (or (get-text-property 0 'annot candidate) ""))

;;;###autoload
(defun company-coffee (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-coffee))
    (prefix (company-coffee-prefix))
    (annotation (company-coffee-annotation arg))
    (sorted t)
    (candidates (company-coffee-candidates arg))))

;; ;;;###autoload
;; (defun company-coffee-add-keywords ()
;;   "Add coffee keywords to `company-keywords-alist'."
;;   (setcdr
;;    (nthcdr (1- (length company-keywords-alist)) company-keywords-alist)
;;    `(,(append '(coffee-mode) company-coffee-keywords))))

;; ;;;###autoload
;; (eval-after-load "coffee-mode"
;;   '(company-coffee-add-keywords))

(provide 'company-coffee)

;;; company-coffee.el ends here
