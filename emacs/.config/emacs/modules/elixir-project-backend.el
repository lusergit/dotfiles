;;; elixir-project-backend.el --- Elixir projects backend for project.el -*- lexical-binding: t -*-

;; Author: Luca
;; Maintainer: Luca
;; Version: v0.01
;; Package-Requires: (nil)


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; This should provide everything related to the discovery of `elixir'
;; projects within Emacs.

;;; Code:

(require 'project)

(defun lz/project-try-elixir (dir)
  "Determine if DIR is the root of an Elixir project."
  (let* ((root (locate-dominating-file dir "mix.exs")))
    (when root
      (let ((igns (list
		   (expand-file-name "deps/" root)
		   (expand-file-name ".elixir-tools/" root)
		   (expand-file-name "_build/" root))))
	(list 'elixir root igns)))))

(cl-defmethod project-root ((project (head elixir)))
  "Return the root directory of the Elixir PROJECT."
  (car (cdr project)))

(cl-defmethod project-ignores ((project (head elixir)) dir)
  "Ignore `deps` and `_build` directories in Elixir PROJECT."
  (append (car (cdr (cdr project))) grep-find-ignored-files))

(add-hook 'project-find-functions #'lz/project-try-elixir)

(provide 'elixir-project-backend)

;;; elixir-project-backend.el ends here
