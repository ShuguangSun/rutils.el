;;; rutils-devtools.el --- R devtools with transient           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shuguang Sun

;; Author: Shuguang Sun <shuguang79@qq.com>
;; Created: 2021/06/19
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

;; Transient for devtools for R.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'rutils-lib)


;; TODO: ess-r-devtools-execute-command
;; ["devtools::"
;;  ("ba" "bash")
;;  ("bu" "build")
;;  ("bm" "build-manual")
;;  ("bv" "build-vignettes")
;;  ("ck" "check")
;;  ("cM" "check-mac-release")
;;  ("cm" "check-man")
;;  ("cr" "check-rhub")
;;  ("cwd" "check-win-devel")
;;  ("cwo" "check-win-oldrelease")
;;  ("cwr" "check-win-release")
;;  ("cv" "clean_vignettes")
;;  ("ds" "dev_sitrep")
;;  ("do" "document")
;;  ("in" "install")
;;  ("id" "install_deps")
;;  ("iD" "install_dev_deps")
;;  ("l" "lint")
;;  ("m" "missing_s3")
;;  ("rl" "release")

;;  ]


;;; * menu
;;;###autoload (autoload 'rutils-devtools "rutils-devtools" nil t)
(transient-define-prefix rutils-devtools ()
  "R devtools menu."
  ;; ["Arguments"
  ;;  (rutils-renv:--reuse-project)]
  [["Dev"
    ("b" "Build" ess-r-devtools-build)
    ("c" "check" ess-r-devtools-check-package)
    ("d" "document" ess-r-devtools-document-package)
    ("l" "load" ess-r-devtools-load-package)
    ("t" "test" ess-r-devtools-test-package)
    ("u" "unload" ess-r-devtools-unload-package)]
   ["Install"
    ("i" "install-package" ess-r-devtools-install-package)
    ("I" "install-github" ess-r-devtools-install-github)]
   ["Create"
    ("C" "Create" ess-r-devtools-create-package)]
   ["Excute"
    ("A" "Ask excute-command:" ess-r-devtools-execute-command)
    ("E" "excute-command:" ignore)]
   ["Misc"
    ("W" "check-with-winbuilder" ess-r-devtools-check-with-winbuilder)]
   ])


(provide 'rutils-devtools)
;;; rutils-renv.el ends here
