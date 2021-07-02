;;; rutils.el --- General stuffs                -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shuguang Sun

;; Author: Shuguang Sun <shuguang79@qq.com>
;; Created: 2021/06/25
;; Version: 0.0.1
;; URL: https://github.com/ShuguangSun/rutils
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


(defcustom rutils-using-compile-p nil
  "Using compile or using `ess-command' with R process?"
  :group 'rutils
  :type 'boolean)


(defun rutils--command (cmd &optional buffer)
  "Wrap up of `ess-command' with checking process avalability frist.
Argument CMD R script/command as string.
Optional argument BUFFER if non-nill, display the output in the BUFFER."
  ;; dir first
  (unless (and (string= "R" ess-dialect) ess-local-process-name)
    (ess-switch-process))
  (let* ((buf (current-buffer))
         (proc-name (buffer-local-value 'ess-local-process-name buf))
         (proc (get-process proc-name)))
    (when (and proc-name proc
               (not (process-get proc 'busy)))
      (if buffer
          (ess-execute cmd (get-buffer-create buffer))
        ;; (ess-command cmd nil nil nil nil proc)
        (pop-to-buffer (process-buffer proc))
        (ess-send-string proc cmd t)
        (pop-to-buffer buf)
        (revert-buffer)))))



(provide 'rutils)
;;; rutils.el ends here
