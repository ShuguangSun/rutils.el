;;; rutils-lib.el --- R utilities common functions                -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shuguang Sun

;; Author: Shuguang Sun <shuguang79@qq.com>
;; Created: 2021/06/25
;; Version: 0.0.1
;; URL: https://github.com/ShuguangSun/rutils
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

;; General stuffs used in multiple files.

;;; Code:


(require 'ess-inf)
(require 'ess-r-mode)
(require 'ess-r-completion)
(require 'subr-x)
(require 'json)
(require 'transient)

(defgroup rutils nil
  "R utilities with transient menu."
  :group 'rutils
  :prefix "rutils")


(defsubst rutils-lib--send-command (cmd &optional buffer)
  "Wrap up of `ess-command' with checking process availability first.
Argument CMD R script/command as string.
Optional argument BUFFER if non-nil, display the output in the BUFFER."
  (unless (and (string= "R" ess-dialect) ess-local-process-name)
    (ess-switch-process))
  (let* ((buf (current-buffer))
         (proc-name (buffer-local-value 'ess-local-process-name buf))
         (proc (get-process proc-name)))
    (when (and proc-name proc
               (not (process-get proc 'busy)))
      (if buffer
          (ess-execute cmd (get-buffer-create buffer))
        (pop-to-buffer (process-buffer proc))
        (ess-send-string proc cmd t)
        (pop-to-buffer buf)
        (revert-buffer)))))

(defun rutils-lib--send-command-with-project (verb args argfilter &optional buffer)
  "Send command with project path.
Argument VERB R command, a string.
Argument ARGS arguments from transient.
Optional argument BUFFER if non-nil, display outputs in the buffer.
Argument ARGFILTER function to filter or assert arguments to verb."
  (if (not args)
       (rutils-lib--send-command (concat verb "()") buffer)
     (let (proj)
       ;; dir first
       (when (cl-find-if (lambda (a) (string-match-p "\\`--project=" a)) args)
         (setq proj
               (cl-find nil args
                        :if (lambda (x) (string-match-p "\\`--project=" x))))
         (when (> (length proj) 0)
           (setq proj (file-name-as-directory (substring proj 10)))
           (if (file-exists-p proj)
               (dired proj)
             (if (y-or-n-p-with-timeout
                  (format "\"%s\" not exist. Create it? " proj) 4 nil)
                 (progn (make-directory proj)
                        (dired proj))))))
       (when (and args (functionp argfilter))
         (setq args (funcall argfilter args)) "")
       (rutils-lib--send-command (concat verb "(" args ")") buffer))))

(defun rutils-read-cran-package-list (prompt _initial-input _history)
  "Read a package from CRAN."
  (if (not (string-match-p "^R" ess-dialect))
      (message "Sorry, not available for %s" ess-dialect)
    (let ((ess-eval-visibly-p t)
          (packs (ess-get-words-from-vector
                  "print(rownames(available.packages()), max=1e6)\n")))
      (completing-read prompt packs nil nil))))


(provide 'rutils-lib)
;; Local variables:
;; package-lint-main-file: "rutils.el"
;; end:
;;; rutils-lib.el ends here
