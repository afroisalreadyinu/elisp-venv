;;; elisp-venv.el --- Elisp virtual environment

;; Author: Ulas Tuerkmen <ulas.tuerkmen at gmail dot com>
;; URL: http://github.com/afroisalreadyinu/elisp-venv
;; Version: 0.9.2
;; Keywords: tools, tdd, elisp
;; Package-Requires: ((f "0.20.0"))
;;
;; Copyright (C) 2011 Ulas Tuerkmen
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Create a virtual environment which installs the dependencies of a package in
;; a sandbox.

(require 'f)

(defgroup elisp-venv nil
  "Elisp virtual environments."
  :group 'elisp)

(defvar elisp-venv-mode nil
  "Mode variable for elisp-venv-mode")
(make-variable-buffer-local 'elisp-venv-mode)

(defcustom elisp-venv-base-directory "~/.elisp-venv/%s-sandbox"
  "The directory in which the virtual environments are created")


(defun run-elisp-tests ()
  (let ((tests (mapcar (lambda (x) (intern (substring x (length  "-run-test="))))
		       (seq-filter (lambda (x) (abl-mode-starts-with x "-run-test="))
				   command-line-args-left))))
    (setq command-line-args-left nil)
    (if tests
	(ert `(member ,@tests))
      (ert 't))))

(defvar init-file-content
  (string-join
   '("(custom-set-variables"
     " '(package-user-dir \"%s\"))"
     "(require 'package)"
     "(add-to-list 'package-archives (cons \"melpa\" \"https://melpa.org/packages/\") t)"
     "(package-initialize)"
     "(when (not package-archive-contents)"
     "  (package-refresh-contents))")
   "\n"))

(defvar install-file-content
  (string-join
   '("(require '%1$s)"
     "(find-file (find-lisp-object-file-name '%1$s 'defvar))"
     "(let ((package (package-buffer-info)))"
     "  (package-download-transaction"
     "   (package-compute-transaction nil (package-desc-reqs package))))")
   "\n"))

(defun elisp-venv-create-directories(package-name)
  """Create the directories for installing dependcies etc.,
  return command that will actually do so"""
  (let* ((venv-directory (format elisp-venv-base-directory package-name))
	 (init-file-path (f-join venv-directory "init.el"))
	 (install-file-path (f-join venv-directory "install.el"))
	 (packages-dir (f-join venv-directory "packages")))
    (make-directory venv-directory)
    (write-to-file init-file-path (format init-file-content packages-dir))
    (write-to-file install-file-path
		   (format install-file-content package-name))
    (makedir packages-dir)
    (format "emacs -q -L . -l %s -l %s" init-file-path install-file-path)))

(defun create-package-sandbox ()
  """Create a sandbox directory with necessary files, spit out command to install"""
  (interactive)
  (let ((command (elisp-venv-create-directories
		  (package-desc-name (save-excursion (package-buffer-info))))))
    (shell-command command)))
