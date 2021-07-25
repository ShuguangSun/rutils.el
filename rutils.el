;;; rutils.el --- R utilities with transient       -*- lexical-binding: t; -*-

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

;; R utilities with transient.

;;; Code:


(require 'rutils-lib)

(require 'rutils-renv)
(require 'rutils-packrat)

(provide 'rutils)
;;; rutils.el ends here
