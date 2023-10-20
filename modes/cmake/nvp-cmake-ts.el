;;; nvp-cmake-ts.el --- cmake tree-sitter extra -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; - Fixes `cmake-ts-mode' indentation
;; - Better imenu w/ custom targets
;; - Fixes font-lock for 'builtin feature
;; - Adds font-lock for function defs/params, assignment, some types
;; - Adds tree-sitter navigation for defuns
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'cmake-ts-mode)
(nvp:decls)

(defface nvp-cmake-assignment-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face for custom targets and assignments.")

(defface nvp-cmake-function-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for function definition.")

(defvar nvp-cmake-builtin-re
  (rx bos (or "cmake_host_system_information"
              "cmake_language"
              "cmake_minimum_required"
              "cmake_parse_arguments"
              "cmake_path"
              "cmake_policy"
              "configure_file"
              "execute_process"
              "file"
              "find_file"
              "find_library"
              "find_package"
              "find_path"
              "find_program"
              "foreach"
              "get_cmake_property"
              "get_directory_property"
              "get_filename_component"
              "get_property"
              "include"
              "include_guard"
              "list"
              "macro"
              "mark_as_advanced"
              "math"
              "message"
              "option"
              "separate_arguments"
              "set"
              "set_directory_properties"
              "set_property"
              "site_name"
              "string"
              "unset"
              "variable_watch"
              "add_compile_definitions"
              "add_compile_options"
              "add_custom_command"
              "add_custom_target"
              "add_definitions"
              "add_dependencies"
              "add_executable"
              "add_library"
              "add_link_options"
              "add_subdirectory"
              "add_test"
              "aux_source_directory"
              "build_command"
              "create_test_sourcelist"
              "define_property"
              "enable_language"
              "enable_testing"
              "export"
              "fltk_wrap_ui"
              "get_source_file_property"
              "get_target_property"
              "get_test_property"
              "include_directories"
              "include_external_msproject"
              "include_regular_expression"
              "install"
              "link_directories"
              "link_libraries"
              "load_cache"
              "project"
              "remove_definitions"
              "set_source_files_properties"
              "set_target_properties"
              "set_tests_properties"
              "source_group"
              "target_compile_definitions"
              "target_compile_features"
              "target_compile_options"
              "target_include_directories"
              "target_link_directories"
              "target_link_libraries"
              "target_link_options"
              "target_precompile_headers"
              "target_sources"
              "try_compile"
              "try_run"
              "ctest_build"
              "ctest_configure"
              "ctest_coverage"
              "ctest_empty_binary_directory"
              "ctest_memcheck"
              "ctest_read_custom_files"
              "ctest_run_script"
              "ctest_sleep"
              "ctest_start"
              "ctest_submit"
              "ctest_test"
              "ctest_update"
              "ctest_upload")
      eos))

;;; Indentation

(defvar cmake-ts-mode-align-arguments nil
  "When non-nil, align all arguments in argument list with first argument.")

(defun cmake-ts-mode--arg-anchor (&rest args)
  (apply (alist-get (if cmake-ts-mode-align-arguments
                        'first-sibling
                      'parent-bol)
                    treesit-simple-indent-presets)
         args))

(defun cmake-ts-mode--arg-offset (_n _p &rest _)
  (if cmake-ts-mode-align-arguments 0 cmake-ts-mode-indent-offset))

(setq cmake-ts-mode--indent-rules
      `((cmake
         ((parent-is "source_file") parent 0)
         ((node-is ")") parent-bol 0)
         ((node-is "else_command") parent-bol 0)
         ((node-is "elseif_command") parent-bol 0)
         ((node-is "endforeach_command") parent-bol 0)
         ((node-is "endfunction_command") parent-bol 0)
         ((node-is "endif_command") parent-bol 0)
         ((node-is "endblock") parent-bol 0)
         ((node-is "endmacro") parent-bol 0)
         ((node-is "function_def") parent-bol 0)
         ((parent-is "if") parent-bol cmake-ts-mode-indent-offset)
         ((parent-is "body") grand-parent cmake-ts-mode-indent-offset)
         ((match "" "argument_list" nil 1) cmake-ts-mode--arg-anchor
          cmake-ts-mode--arg-offset)
         ((parent-is "argument_list") parent-bol cmake-ts-mode-indent-offset)
         ((node-is "argument") parent-bol cmake-ts-mode-indent-offset)
         ((parent-is "foreach_loop") parent-bol cmake-ts-mode-indent-offset)
         ((parent-is "function_def") parent-bol cmake-ts-mode-indent-offset)
         ((parent-is "normal_command") parent-bol cmake-ts-mode-indent-offset)
         ((parent-is "quoted") no-indent)
         (no-node parent-bol cmake-ts-mode-indent-offset))))

;;; Font-lock

(defvar nvp-cmake-ts-extra-fonts
  (treesit-font-lock-rules
   :language 'cmake
   :feature 'definition
   :override t
   `(((normal_command
       (identifier) @font-lock-keyword-face)
      (:match ,(rx bos (or "return") eos) @font-lock-keyword-face))
     
     (function_command
      (argument_list
       :anchor (argument) @first-child @nvp-cmake-function-face
       [(argument)] @font-lock-variable-name-face :*))

     (macro_command
      (argument_list
       :anchor (argument) @first-child @nvp-cmake-function-face
       [(argument)] @font-lock-variable-name-face :*)))
   
   :language 'cmake
   :feature 'builtin
   `(((foreach_command
       ((argument_list (argument) @font-lock-constant-face)
        (:match ,(rx-to-string
                  `(seq bol
                        (or ,@cmake-ts-mode--foreach-options)
                        eol))
                @font-lock-constant-face))))
     ((if_command
       ((argument_list (argument) @font-lock-constant-face)
        (:match ,(rx-to-string
                  `(seq bol
                        (or ,@cmake-ts-mode--if-conditions)
                        eol))
                @font-lock-constant-face))))
     (elseif_command
      ((argument_list (argument) @font-lock-constant-face)
       (:match ,(rx-to-string
                 `(seq bol
                       (or ,@cmake-ts-mode--if-conditions)
                       eol))
               @font-lock-constant-face)))
     ((unquoted_argument) @font-lock-type-face
      ;; Message <mode>
      (:match ,(rx (or "STATUS" "WARNING" "ERROR" "DEBUG" "TRACE"
                       "DEPRECATION" "NOTICE" "VERBOSE" "CONFIGURE_LOG"
                       (seq "CHECK" (or "START" "PASS" "FAIL"))))
              @font-lock-type-face))

     ((normal_command
       (identifier) @font-lock-builtin-face
       (:match ,nvp-cmake-builtin-re @font-lock-builtin-face))))

   :language 'cmake
   :feature 'assignment
   `(((normal_command
       (identifier) @_name
       (argument_list :anchor (argument) @first-child
                      @nvp-cmake-assignment-face))
      (:match ,(rx bos (or "set" "add")) @_name)))

   :language 'cmake
   :feature 'property
   '(((normal_command
       (identifier)
       (argument_list
        (argument (unquoted_argument) @font-lock-constant-face)))
      (:match "\\`[A-Z@][A-Z0-9_]+\\'" @font-lock-constant-face)))

   ;; Missing keywords
   :language 'cmake
   :feature 'keyword
   `([(block) (endblock) ,@cmake-ts-mode--keywords] @font-lock-keyword-face)))

(defvar nvp-cmake-ts-mode-features
  (mapcar (lambda (e) (nth 2 e)) nvp-cmake-ts-extra-fonts))

;;; Imenu

(defun cmake-ts-mode--imenu-p (node)
  (pcase (treesit-node-type node)
    (`"normal_command"
     (string-match-p
      (rx bos (or "add_custom_target") eos)
      (treesit-node-text
       (treesit-node-child node 0))))
    (_ nil)))

(defun cmake-ts-mode--defun-name (node)
  (treesit-node-text
   (treesit-node-child 
    (treesit-search-subtree
     node "argument_list"
     nil t 1)
    0 t)))

(defvar cmake-ts-mode--imenu-settings
  '(("Function" "\\`function_command\\'")
    ("Macro" "\\`macro_command\\'")
    ("Target" "\\`normal_command\\'" cmake-ts-mode--imenu-p)))

;; Update `cmake-ts-mode'
(setq cmake-ts-mode--font-lock-settings
      (seq-uniq (append nvp-cmake-ts-extra-fonts
                        (seq-filter
                         (lambda (e) (not (eq 'error (nth 2 e))))
                         cmake-ts-mode--font-lock-settings))))

;; local settings in `cmake-ts-mode'
(defun nvp-cmake-ts-enable ()
  ;; Add font-lock features
  (setf (car treesit-font-lock-feature-list)
        (seq-uniq (append nvp-cmake-ts-mode-features
                          (car treesit-font-lock-feature-list))))
  ;; Navigation
  (setq-local treesit-defun-tactic 'top-level)
  (setq-local treesit-defun-name-function #'cmake-ts-mode--defun-name)
  (setq-local treesit-defun-type-regexp
              (rx bos (or "macro_def" "function_def") eos))
  ;; config for beg/end-of-defun from `treesit-major-mode-setup'
  (keymap-set (current-local-map) "<remap> <beginning-of-defun>"
              #'treesit-beginning-of-defun)
  (keymap-set (current-local-map) "<remap> <end-of-defun>"
              #'treesit-end-of-defun)
  (setq-local beginning-of-defun-function #'treesit-beginning-of-defun)
  (setq-local end-of-defun-function #'treesit-end-of-defun)
  ;; Imenu
  (setq-local imenu-create-index-function #'treesit-simple-imenu)
  (setq-local treesit-simple-imenu-settings cmake-ts-mode--imenu-settings))

(provide 'nvp-cmake-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-cmake-ts.el ends here
