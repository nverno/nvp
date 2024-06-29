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
(require 'cmake-ts-mode)
(nvp:decls)

(defface nvp-cmake-assignment-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for custom targets and assignments.")

(defface nvp-cmake-function-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for function definition.")

(defcustom cmake-ts-mode-align-arguments nil
  "When non-nil, align all arguments in argument list with first argument."
  :type 'boolean
  :safe 'booleanp
  :group 'cmake)


(defvar nvp-cmake-builtin
  '("cmake_host_system_information"
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
    "ctest_upload"))


;;; Imenu

(defun cmake-ts-mode--imenu-p (type node)
  (pcase (treesit-node-type node)
    (`"normal_command"
     (string-match-p (rx-to-string `(seq bos ,type eos))
                     (treesit-node-text
                      (treesit-node-child node 0))))
    (_ nil)))

(defun cmake-ts-mode--defun-name (node)
  (treesit-node-text
   (treesit-node-child
    (treesit-search-subtree node "argument_list" nil t 1)
    0 'named)))

(defvar cmake-ts-mode--imenu-settings
  `(("Function" "\\`function_command\\'")
    ("Macro" "\\`macro_command\\'")
    ("Project" "\\`normal_command\\'"
     ,(apply-partially #'cmake-ts-mode--imenu-p "project"))
    ("Executable" "\\`normal_command\\'"
     ,(apply-partially #'cmake-ts-mode--imenu-p "add_executable"))
    ("Target" "\\`normal_command\\'"
     ,(apply-partially #'cmake-ts-mode--imenu-p "add_custom_target"))
    ("Option" "\\`normal_command\\'"
     ,(apply-partially #'cmake-ts-mode--imenu-p "option"))))


;;; Indentation

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
         ((parent-is "source_file") parent-bol 0)
         ((node-is ")") parent-bol 0)
         ((node-is "else_command") parent-bol 0)
         ((node-is "elseif_command") parent-bol 0)
         ((node-is "endforeach_command") parent-bol 0)
         ((node-is "endfunction_command") parent-bol 0)
         ((node-is "endif_command") parent-bol 0)
         ;;-- start added
         ((node-is "endblock") parent-bol 0)
         ((node-is "endmacro") parent-bol 0)
         ((parent-is "quoted") no-indent)
         ;;-- end added
         ((parent-is "foreach_loop") parent-bol cmake-ts-mode-indent-offset)
         ((parent-is "function_def") parent-bol cmake-ts-mode-indent-offset)
         ((parent-is "if_condition") parent-bol cmake-ts-mode-indent-offset)
         ((parent-is "normal_command") parent-bol cmake-ts-mode-indent-offset)
     ;;; Release v0.4.0 wraps arguments in an argument_list node.
         ,@(ignore-errors
             (treesit-query-capture 'cmake '((argument_list) @capture))
             ;;-- start added
             `(((match "" "argument_list" nil 2) cmake-ts-mode--arg-anchor
                cmake-ts-mode--arg-offset)
               ((parent-is "argument_list") parent-bol cmake-ts-mode-indent-offset))
             ;;-- end added
             ;; `(((parent-is "argument_list") grand-parent cmake-ts-mode-indent-offset))
             )
     ;;; Release v0.3.0 wraps the body of commands into a body node.
         ,@(ignore-errors
             (treesit-query-capture 'cmake '((body) @capture))
             `(((parent-is "body") grand-parent cmake-ts-mode-indent-offset))))))


;;; Font-lock

(defvar nvp-cmake-ts-extra-fonts
  (cons
   (treesit-font-lock-rules
    :language 'cmake
    :feature 'definition
    ;; :override t
    `((normal_command
       ((identifier) @_project
        (:match ,(rx bol "project" eol) @_project)
        (argument_list :anchor (argument) @font-lock-function-name-face)))
      
      (normal_command
       ((identifier) @font-lock-keyword-face
        (:match ,(rx bol (or "return" "break" "continue") eol) @font-lock-keyword-face)))
      
      (function_command
       (argument_list :anchor (argument) @nvp-cmake-function-face
                      [(argument)] :* @font-lock-variable-name-face))

      (macro_command
       (argument_list :anchor (argument) @nvp-cmake-function-face
                      [(argument)] :* @font-lock-variable-name-face))

      (foreach_command
       (argument_list :anchor
                      (argument (unquoted_argument) @font-lock-variable-name-face)))

      (block_command
       (block)
       (argument_list
        (argument
         ((unquoted_argument) @font-lock-constant-face
          (:match ,(rx bol (or "SCOPE_FOR" "POLICIES" "VARIABLES" "PROPAGATE") eol)
                  @font-lock-constant-face))))))
    
    :language 'cmake
    :feature 'assignment
    `(((normal_command
        ((identifier) @_name
         (:match ,(rx bol (or "add_executable"
                              "add_custom_target"
                              "add_custom_command")
                      eol)
                 @_name))
        (argument_list :anchor (argument) @nvp-cmake-function-face)))
      
      ((normal_command
        ((identifier) @_name
         (:match ,(rx bol (or "add_test"
                              "add_subdirectory"
                              "add_library"
                              "add_dependencies"
                              "add_definitions"
                              "set"
                              "set_directory_properties"
                              "set_target_properties"
                              "set_property")
                      eol)
                 @_name))
        (argument_list :anchor (argument) @nvp-cmake-assignment-face))))
    
    :language 'cmake
    :feature 'builtin
    ;; Note: Overwrites feature
    `(((env_var
        ["$" "ENV" "{" "}"] @font-lock-preprocessor-face))

      ((foreach_command
        ((argument_list (argument) @font-lock-operator-face)
         (:match ,(rx-to-string
                   `(seq bol (or ,@cmake-ts-mode--foreach-options) eol))
                 @font-lock-operator-face))))
      ((if_command
        ((argument_list (argument) @font-lock-operator-face)
         (:match ,(rx-to-string
                   `(seq bol (or ,@cmake-ts-mode--if-conditions) eol))
                 @font-lock-operator-face))))
      (elseif_command
       ((argument_list (argument) @font-lock-operator-face)
        (:match ,(rx-to-string
                  `(seq bol (or ,@cmake-ts-mode--if-conditions) eol))
                @font-lock-operator-face)))
      (while_command
       ((argument_list (argument) @font-lock-operator-face)
        (:match ,(rx-to-string
                  `(seq bol (or ,@cmake-ts-mode--if-conditions) eol))
                @font-lock-operator-face)))
      ((unquoted_argument) @font-lock-type-face
       ;; Message <mode>
       (:match ,(rx (or "STATUS" "WARNING" "ERROR" "DEBUG" "TRACE"
                        "DEPRECATION" "NOTICE" "VERBOSE" "CONFIGURE_LOG"
                        (seq "CHECK" (or "START" "PASS" "FAIL"))
                        ;; set_target_properties
                        "PROPERTIES"
                        ;; add_custom_target
                        "DEPENDS"))
               @font-lock-type-face))

      ((normal_command
        (identifier) @font-lock-preprocessor-face
        (:match ,(rx bol "set") @font-lock-preprocessor-face)))

      ((normal_command
        (identifier) @font-lock-builtin-face
        (:match ,(rx-to-string
                  `(seq bol
                        (or ,@nvp-cmake-builtin)
                        eol))
                @font-lock-builtin-face))))

    :language 'cmake
    :feature 'property
    '((argument_list
        (argument
         ((unquoted_argument) @font-lock-constant-face
          (:match "\\`[A-Z@][A-Z0-9_]+\\'" @font-lock-constant-face))))
      ;; (normal_command
      ;;  (identifier)
      ;;  (argument_list
      ;;   (argument
      ;;    ((unquoted_argument) @font-lock-constant-face
      ;;     (:match "\\`[A-Z@][A-Z0-9_]+\\'" @font-lock-constant-face)))))
      )

    ;; Missing keywords
    :language 'cmake
    :feature 'keyword
    ;; Note: overwrites feature
    `([(block) (endblock) ,@cmake-ts-mode--keywords] @font-lock-keyword-face))

   (treesit-font-lock-rules
    :language 'cmake
    :feature 'argument
    `((argument ((unquoted_argument) @font-lock-property-use-face
                 (:match ,(rx bol (+ letter) eol) @font-lock-property-use-face)))))))


(nvp:treesit-add-rules cmake-ts-mode
  :mode-fonts cmake-ts-mode--font-lock-settings
  :new-fonts (car nvp-cmake-ts-extra-fonts)
  :post-fonts (cdr nvp-cmake-ts-extra-fonts))

;;;###autoload
(defun nvp-cmake-ts-enable ()
  "Enable tree-sitter imenu and navigation in `cmake-ts-mode'."
  (setq-local treesit-defun-tactic 'top-level)
  (setq-local treesit-simple-imenu-settings cmake-ts-mode--imenu-settings))

(provide 'nvp-cmake-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-cmake-ts.el ends here
