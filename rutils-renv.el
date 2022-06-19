;;; rutils-renv.el --- R renv with transient           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shuguang Sun

;; Author: Shuguang Sun <shuguang79@qq.com>
;; Created: 2021/06/25
;; Version: 0.0.1
;; URL: https://github.com/ShuguangSun/rutils.el
;; Package-Requires: ((emacs "26.1") (ess "18.10.1") (transient "0.3.0"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Transient for renv for R.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'rutils-lib)


(defvar rutils-renv-project-history nil "History of rutils renv.")

(defvar rutils-renv-option-boolean-list
  '("bare" "force" "restart" "regex" "version" "reload" "cache"
    "clean" "update" "dev")
  "R renv BOOLEAN options.")

(defvar rutils-renv-option-string-list
  '(;; directory
    "project" "path" "root"
    ;; file
    "lockfile"
    ;; string
    "profile" "repos" "type")
  "R renv STRING options.")

(defun rutils-renv--assert (args)
  "Parse transient ARGS."
  (let ((bollist (mapcar (lambda (x) (concat "--" x))
                         rutils-renv-option-boolean-list))
        (strlist (concat
                  "\\`--\\("
                  (mapconcat 'identity rutils-renv-option-string-list "\\|")
                  "\\)=\\(.+\\)")))
    (cl-loop for arg in args
             do (cond
                 ((string-match strlist arg)
                  (setq arg (concat
                             (substring arg (match-beginning 1) (match-end 1))
                             "="
                             (shell-quote-argument
                           (substring arg (match-beginning 2) (match-end 2))))))
                 ((member arg bollist)
                  (setq arg (concat (substring arg 2) "=TRUE")))
                 (t
                  (setq arg nil)))
             collect arg into ret
             finally return (string-join (delq nil (delete-dups ret)) ", "))))

(transient-define-argument rutils-renv:--reuse-project ()
  :description "The project directory. Default NULL, current getwd()."
  :class 'transient-option
  :shortarg "-p"
  :argument "--project="
  :reader 'transient-read-directory
  :history-key 'rutils-renv-project-history)


;;; * renv::init
(defun rutils-renv-init-run (&optional args)
  "Invoke renv::init with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-renv-init))))
  (rutils-lib--send-command-with-project "renv::init" args 'rutils-renv--assert))

;;;###autoload (autoload 'rutils-renv-init "rutils-renv" nil t)
(transient-define-prefix rutils-renv-init ()
  "R renv::init."
  ["Arguments"
   (rutils-renv:--reuse-project)
   ;; (rutils-renv:--reuse-profile)
   ;; ("-p" "The project directory. Default NULL, current getwd()."
   ;;  "--project=" transient-read-directory)
   ("-P" "The profile to be activated. Default NULL."
    "--profile=" read-string)
   ("-B" "Bare? Default FALSE" "--bare")
   ("-F" "Force? Default FALSE" "--force")
   ("-R" "Restart? Default `interactive()'" "--restart")
   ]
  [["Renv::"
    ("i" "Init"         rutils-renv-init-run)]])

;;; * renv::snapshot
(defun rutils-renv-snapshot-run (&optional args)
  "Invoke renv::snapshot with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-renv-snapshot))))
  (rutils-lib--send-command-with-project "renv::snapshot" args 'rutils-renv--assert))

(transient-define-infix rutils-renv:--snapshot-type ()
  :description "Type? Default `implicit'"
  :class 'transient-option
  :key "-t"
  :argument "--type="
  :choices '("implicit" "all" "explicit" "custom"))

;; (transient-define-infix rutils-renv:--snapshot-type ()
;;   :description "Type? Default `implicit'"
;;   :class 'transient-switches
;;   :key "-t"
;;   :argument "--type=%s"
;;   :argument-regexp "\\(--type=\\(implicit\\|all\\|explicit\\|custom\\)\\)"
;;   :choices '("implicit" "all" "explicit" "custom"))

;;;###autoload (autoload 'rutils-renv-snapshot "rutils-renv" nil t)
(transient-define-prefix rutils-renv-snapshot ()
  "R renv::snapshot."
  ["Arguments"
   (rutils-renv:--reuse-project)
   ;; ("-L" "library. (not supported by now)" "-L")
   ("-l" "lockfile" "--lockfile=" transient-read-file)
   ;; ("-t" "Type? Default `implicit'" "--type=" read-string)
   (rutils-renv:--snapshot-type)
   ;; ("-P" "packages. (not supported by now)" "-P")
   ("-F" "Force? Default FALSE" "--force")
   ("-R" "reprex? Default `FALSE'" "--regex")]
  [["Renv::"
    ("s" "Snapshot"         rutils-renv-snapshot-run)]])


;;; * renv::status
(defun rutils-renv-status-run (&optional args)
  "Invoke renv::status with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-renv-status))))
  (rutils-lib--send-command-with-project "renv::status" args 'rutils-renv--assert))

;;;###autoload (autoload 'rutils-renv-status "rutils-renv" nil t)
(transient-define-prefix rutils-renv-status ()
  "R renv::status."
  ["Arguments"
   (rutils-renv:--reuse-project)
   ;; ("-L" "library. (not supported by now)" "-L")
   ("-l" "lockfile" "--lockfile=" transient-read-file)
   ("-c" "cache? Default `FALSE'" "--cache")]
  [["Renv::"
    ("s" "Status"         rutils-renv-status-run)]])


;;; * renv::restore
(defun rutils-renv-restore-run (&optional args)
  "Invoke renv::restore with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-renv-restore))))
  (rutils-lib--send-command-with-project "renv::restore" args 'rutils-renv--assert))

;;;###autoload (autoload 'rutils-renv-restore "rutils-renv" nil t)
(transient-define-prefix rutils-renv-restore ()
  "R renv::restore."
  ["Arguments"
   (rutils-renv:--reuse-project)
   ;; ("-L" "library. (not supported by now)" "-L")
   ("-l" "lockfile" "--lockfile=" transient-read-file)
   ;; ("-P" "packages. (not supported by now)" "-P")
   ("-r" "rebuild? Default `NULL'" "--rebuild")
   ("-R" "repos" "--repos=" read-string)
   ("-C" "clean? Default `FALSE'" "--clean")]
  [["Renv::"
    ("r" "Restore"         rutils-renv-restore-run)]])


;;; * renv::update
(defun rutils-renv-update-run (&optional args)
  "Invoke renv::update with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-renv-update))))
  (rutils-lib--send-command-with-project "renv::update" args 'rutils-renv--assert))

;;;###autoload (autoload 'rutils-renv-update "rutils-renv" nil t)
(transient-define-prefix rutils-renv-update ()
  "R renv::update."
  ["Arguments"
   (rutils-renv:--reuse-project)
   ;; exclude
   ;; library
   ("-v" "version? Default `NULL'" "--version")
   ("-r" "reload? Default `FALSE'" "--reload")]
  [["Renv::"
    ("u" "Update"         rutils-renv-update-run)]])


;;; * renv::hydrate
(defun rutils-renv-hydrate-run (&optional args)
  "Invoke renv::hydrate with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-renv-hydrate))))
  (rutils-lib--send-command-with-project "renv::hydrate" args 'rutils-renv--assert))

;;;###autoload (autoload 'rutils-renv-hydrate "rutils-renv" nil t)
(transient-define-prefix rutils-renv-hydrate ()
  "R renv::hydrate."
  ["Arguments"
   (rutils-renv:--reuse-project)
   ;; packages not supported
   ;; library not supported
   ("-u" "update? Default `FALSE'" "--update")
   ;; ("-s" "sources? Default `NULL'" "--sources")
   ]
  [["Renv::"
    ("h" "Hydrate"         rutils-renv-hydrate-run)]])


;;; * renv::dependencies
(defun rutils-renv-dependencies-run (&optional args)
  "Invoke renv::dependencies with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-renv-dependencies))))
  (rutils-lib--send-command-with-project "renv::dependencies" args 'rutils-renv--assert))

(transient-define-infix rutils-renv:--dependencies-errors ()
  :description "errors? Default all (reported, fatal, ignored)."
  :class 'transient-option
  :key "-e"
  :argument "--errors="
  :multi-value t
  :choices '("reported" "fatal" "ignored" "all"))

;;;###autoload (autoload 'rutils-renv-dependencies "rutils-renv" nil t)
(transient-define-prefix rutils-renv-dependencies ()
  "R renv::dependencies."
  ["Arguments"
   ("-p" "The path to R files. Default getwd()."
    "--path=" transient-read-directory)
   ("-r" "root directory."
    "--root=" transient-read-directory)
   ;; exclude
   ;; library
   ;; ("-P" "Progress? Default `TRUE'" "--progress")
   (rutils-renv:--dependencies-errors)
   ("-d" "dev? Default `FALSE'" "--dev")]
  [["Renv::"
    ("d" "Dependencies"         rutils-renv-dependencies-run)]])


;;; * renv::diagnostics
(defun rutils-renv-diagnostics-run (&optional args)
  "Invoke renv::diagnostics with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-renv))))
  (rutils-lib--send-command-with-project "renv::diagnostics" args 'rutils-renv--assert))



;;; * renv::install
(defun rutils-renv-install--assert (args)
  "Parse transient ARGS of renv::install."
  (let ((bollist '("--rebuild"))
        (strlist (concat
                  "\\`--\\("
                  (mapconcat 'identity rutils-renv-option-string-list "\\|")
                  "\\)=\\(.+\\)")))
    (cl-loop for arg in args
             do (cond
                 ((string= (substring arg 0 7) "--cran=")
                  (setq arg (concat
                             "packages="
                             (shell-quote-argument
                              (substring arg 7)))))
                 ((string= (substring arg 0 9) "--remote=")
                  (setq arg (concat
                             "packages="
                             (shell-quote-argument
                              (substring arg 9)))))
                 ((string-match strlist arg)
                  (setq arg (concat
                             (substring arg (match-beginning 1) (match-end 1))
                             "="
                             (shell-quote-argument
                           (substring arg (match-beginning 2) (match-end 2))))))
                 ((member arg bollist)
                  (setq arg (concat (substring arg 2) "=TRUE")))
                 (t
                  (setq arg nil)))
             collect arg into ret
             finally return (string-join (delq nil (delete-dups ret)) ", "))))

(transient-define-infix rutils-renv:--install-type ()
  :description "Type? Default `NULL' `getOption(\"pkgType\")'"
  :class 'transient-option
  :key "-t"
  :argument "--type="
  :choices '("NULL" "source" "binary"))

(defun rutils-renv-install-run (&optional args)
  "Invoke renv::install with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-renv-install))))
  (rutils-lib--send-command-with-project "renv::install" args
                                         'rutils-renv-install--assert))

;;;###autoload (autoload 'rutils-renv-install "rutils-renv" nil t)
(transient-define-prefix rutils-renv-install ()
  "R renv::install."
  ["Arguments"
   (rutils-renv:--reuse-project)
   ;; packages not supported
   ;; library not supported
   ("-c" "CRAN packages" "--cran=" rutils-read-cran-package-list)
   ("-r" "remote package" "--remote=")
   ("-l" "library to install, default the active project"
    "--library=" transient-read-existing-directory)
   ;; ("-t" "type: source or binary" "--type=")
   (rutils-renv:--install-type)
   ("-R" "Force packages to be rebuilt" "--rebuild")
   ;; ("-s" "sources? Default `NULL'" "--sources")
   ]
  [["Renv::"
    ("I" "Install"         rutils-renv-install-run)]])


;;; * menu
;;;###autoload (autoload 'rutils-renv "rutils-renv" nil t)
(transient-define-prefix rutils-renv ()
  "R renv menu."
  ;; ["Arguments"
  ;;  (rutils-renv:--reuse-project)]
  [["Renv::"
    ("i" "init" rutils-renv-init)
    ("s" "snapshot" rutils-renv-snapshot)
    ("r" "restore" rutils-renv-restore)]
   ["Renv::"
    ("I" "Install package" rutils-renv-install)
    ("h" "hydrate" rutils-renv-hydrate)
    ("u" "update" rutils-renv-update)]
   ["Renv::"
    ("S" "Status" rutils-renv-status)
    ("D" "Diagnostics" rutils-renv-diagnostics-run)
    ("d" "Dependencies"         rutils-renv-dependencies)]])


(provide 'rutils-renv)
;;; rutils-renv.el ends here
