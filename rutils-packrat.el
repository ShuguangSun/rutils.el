;;; rutils-packrat.el --- R packrat with transient           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shuguang Sun

;; Author: Shuguang Sun <shuguang79@qq.com>
;; Created: 2021/07/02
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

;; Transient for packrat for R.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'rutils-lib)

(defcustom rutils-packrat-buffer "* R Packrat *"
  "Packrat logging buffer."
  :group 'rutils
  :type 'string)

(defvar rutils-packrat-project-history nil "History of rutils packrat.")

(defvar rutils-packrat-option-string-list
  '(;; directory
    "project" "where"
    ;; file
    "file" "bundle")
  "R packrat STRING options.")

(defun rutils-packrat--assert (args)
  "Parse transient ARGS."
  (let ((strlist (concat
                  "\\`--\\("
                  (mapconcat 'identity rutils-packrat-option-string-list "\\|")
                  "\\)=\\(.+\\)")))
    (cl-loop for arg in args
             do (cond
                 ((string-match strlist arg)
                  (print arg)
                  (setq arg (concat
                             (substring arg (match-beginning 1) (match-end 1))
                             "="
                             (shell-quote-argument
                              (substring arg (match-beginning 2) (match-end 2))))))
                 (t
                  (setq arg arg)))
             collect arg into ret
             finally return (string-join (delq nil (delete-dups ret)) ", "))))

(transient-define-argument rutils-packrat:--reuse-project ()
  :description "The project directory. Default NULL, current getwd()."
  :class 'transient-option
  :shortarg "-p"
  :argument "--project="
  :reader 'transient-read-directory
  :history-key 'rutils-packrat-project-history)


;;; * packrat::init
(defun rutils-packrat-init-run (&optional args)
  "Invoke packrat::init with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-packrat-init))))
  (rutils-lib--send-command-with-project "packrat::init" args 'rutils-packrat--assert))

;;;###autoload (autoload 'rutils-packrat-init "rutils-packrat" nil t)
(transient-define-prefix rutils-packrat-init ()
  "R packrat::init."
  ["Arguments"
   (rutils-packrat:--reuse-project)
   ("-e" "Enter? Default `TRUE'" "enter=FALSE")
   ("-R" "Restart? Default `TRUE'" "restart=FALSE")
   ("-d" "infer.dependencies? Default `TRUE'" "infer.dependencies=FALSE")
   ]
  [["Packrat::"
    ("i" "Init"         rutils-packrat-init-run)]])

;;; * packrat::snapshot
(defun rutils-packrat-snapshot-run (&optional args)
  "Invoke packrat::snapshot with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-packrat-snapshot))))
  (rutils-lib--send-command-with-project "packrat::snapshot" args 'rutils-packrat--assert))

;;;###autoload (autoload 'rutils-packrat-snapshot "rutils-packrat" nil t)
(transient-define-prefix rutils-packrat-snapshot ()
  "R packrat::snapshot."
  ["Arguments"
   (rutils-packrat:--reuse-project)
   ("-i" "ignore.stale? Default `FALSE'" "ignore.stable=TRUE")
   ("-d" "dry.run? Default `FALSE'" "dry.run=TRUE")
   ("-S" "snapshot.sources? Default `TRUE'" "snapshot.sources=FALSE")
   ("-i" "infer.dependencies? Default `TRUE'" "infer.dependencies=FALSE")]
  [["Packrat::"
    ("s" "Snapshot"         rutils-packrat-snapshot-run)]])


;;; * packrat::status
(defun rutils-packrat-status-run (&optional args)
  "Invoke packrat::status with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-packrat-status))))
  (rutils-lib--send-command-with-project "packrat::status" args 'rutils-packrat--assert))

;;;###autoload (autoload 'rutils-packrat-status "rutils-packrat" nil t)
(transient-define-prefix rutils-packrat-status ()
  "R packrat::status."
  ["Arguments"
   (rutils-packrat:--reuse-project)
   ("-q" "quiet? Default `FALSE'" "quiet=TRUE")]
  [["Packrat::"
    ("s" "Status"         rutils-packrat-status-run)]])


;;; * packrat::restore
(defun rutils-packrat-restore-run (&optional args)
  "Invoke packrat::restore with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-packrat-restore))))
  (rutils-lib--send-command-with-project "packrat::restore" args 'rutils-packrat--assert))

;;;###autoload (autoload 'rutils-packrat-restore "rutils-packrat" nil t)
(transient-define-prefix rutils-packrat-restore ()
  "R packrat::restore."
  ["Arguments"
   (rutils-packrat:--reuse-project)
   ("-o" "Overwrite.dirty? Default `FALSE'" "overwrite.dirty=TRUE")
   ("-d" "dry.run? Default `FALSE'" "dry.run=TRUE")
   ("-R" "Restart? Default `TRUE'" "restart=FALSE")]
  [["Packrat::"
    ("r" "Restore"         rutils-packrat-restore-run)]])

;;; * packrat::bundle
(defun rutils-packrat-bundle-run (&optional args)
  "Invoke packrat::bundle with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-packrat-bundle))))
  (rutils-lib--send-command-with-project "packrat::bundle" args 'rutils-packrat--assert))

;;;###autoload (autoload 'rutils-packrat-bundle "rutils-packrat" nil t)
(transient-define-prefix rutils-packrat-bundle ()
  "R packrat::bundle."
  ["Arguments"
   (rutils-packrat:--reuse-project)
   ("-f" "file" "--file=" transient-read-file)
   ("-is" "include.src?" "include.src=FALSE")
   ("-il" "include.lib?" "include.lib=TRUE")
   ("-ib" "include.bundles?" "include.bundles=FALSE")
   ("-iv" "include.vcs.history?" "include.vcs.history=TRUE")
   ("-ov" "Overwrite.dirty? Default `FALSE'" "overwrite=TRUE")
   ("-om" "omit.cran.src? Default `FALSE'" "omit.cran.src=TRUE")]
  [["Packrat::"
    ("b" "Bundle"         rutils-packrat-bundle-run)]])

;;; * packrat::unbundle
(defun rutils-packrat-unbundle-run (&optional args)
  "Invoke packrat::unbundle with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-packrat-unbundle))))
  (rutils-lib--send-command-with-project "packrat::unbundle" args 'rutils-packrat--assert))

;;;###autoload (autoload 'rutils-packrat-unbundle "rutils-packrat" nil t)
(transient-define-prefix rutils-packrat-unbundle ()
  "R packrat::unbundle."
  ["Arguments"
   ("-b" "bundle" "--bundle=" transient-read-file)
   ("-w" "where" "--where=" transient-read-directory)
   ("-R" "Restore? Default `TRUE'" "restore=FALSE")]
  [["Packrat::"
    ("u" "Unbundle"         rutils-packrat-unbundle-run)]])

;;; * packrat::clean
(defun rutils-packrat-clean-run (&optional args)
  "Invoke packrat::clean with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-packrat-clean))))
  (rutils-lib--send-command-with-project "packrat::clean" args 'rutils-packrat--assert))

;;;###autoload (autoload 'rutils-packrat-clean "rutils-packrat" nil t)
(transient-define-prefix rutils-packrat-clean ()
  "R packrat::clean."
  ["Arguments"
   (rutils-packrat:--reuse-project)
   ;; packages
   ;; lib.loc
   ("-d" "dry.run? Default `FALSE'" "dry.run=TRUE")
   ("-f" "force? Default `FALSE'" "force=TRUE")]
  [["Packrat::"
    ("c" "Clean"         rutils-packrat-clean-run)]])

;;; * packrat::disable
(defun rutils-packrat-disable-run (&optional args)
  "Invoke packrat::disable with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-packrat-disable))))
  (rutils-lib--send-command-with-project "packrat::disable" args 'rutils-packrat--assert))

;;;###autoload (autoload 'rutils-packrat-disable "rutils-packrat" nil t)
(transient-define-prefix rutils-packrat-disable ()
  "R packrat::disable."
  ["Arguments"
   (rutils-packrat:--reuse-project)
   ("-r" "restart? Default `TRUE'" "restart=FALSE")]
  [["Packrat::"
    ("d" "Disable"         rutils-packrat-disable-run)]])

;;; * packrat::unused_packages
(defun rutils-packrat-unused_packages-run (&optional args)
  "Invoke packrat::unused_packages with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-packrat-unused_packages))))
  (rutils-lib--send-command-with-project "packrat::unused_packages" args
                                         'rutils-packrat--assert
                                         rutils-packrat-buffer))

(transient-define-prefix rutils-packrat-unused_packages ()
  "R packrat::unused_packages."
  ["Arguments"
   (rutils-packrat:--reuse-project)]
  [["Packrat::"
    ("u" "Unused_Packages"         rutils-packrat-unused_packages-run)]])


;;; * packrat::get_opts
(defun rutils-packrat-get_opts-run (&optional args)
  "Invoke packrat::get_opts with ARGS if provided."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'rutils-packrat-get_opts))))
  (rutils-lib--send-command-with-project "packrat::init" args
                                         'rutils-packrat--assert
                                         rutils-packrat-buffer))

(transient-define-prefix rutils-packrat-get_opts ()
  "R packrat::get_opts."
  ["Arguments"
   (rutils-packrat:--reuse-project)
   ;; options
   ;; lib.loc
   ("-s" "simplify? Default `TRUE'" "simplify=FALSE")]
  [["Packrat::"
    ("g" "get_opts"         rutils-packrat-get_opts-run)]])


;;; * menu
;;;###autoload (autoload 'rutils-packrat "rutils-packrat" nil t)
(transient-define-prefix rutils-packrat ()
  "R packrat menu."
  ;; ["Arguments"
  ;;  (rutils-packrat:--reuse-project)]
  [["Packrat::"
    ("i" "init" rutils-packrat-init)
    ("s" "snapshot" rutils-packrat-snapshot)
    ("r" "restore" rutils-packrat-restore)
    ("d" "disable" rutils-packrat-disable)]
   ["Packrat::"
    ("b" "bundle" rutils-packrat-bundle)
    ("u" "unbundle" rutils-packrat-unbundle)
    ("c" "clean" rutils-packrat-clean)]
   ["Packrat::"
    ("S" "Status" rutils-packrat-status)
    ("U" "Unused_packages" rutils-packrat-unused_packages)
    ("g" "get_opts"         rutils-packrat-get_opts)]])


(provide 'rutils-packrat)
;;; rutils-packrat.el ends here
