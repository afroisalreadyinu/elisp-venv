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


(defun elisp-venv-run-elisp-tests ()
  (let ((tests (mapcar (lambda (x) (intern (substring x (length  "-run-test="))))
		       (seq-filter (lambda (x) (abl-mode-starts-with x "-run-test="))
				   command-line-args-left))))
    (setq command-line-args-left nil)
    (if tests
	(ert `(member ,@tests))
      (ert 't))))

(defvar elisp-venv-init-file-content
  (string-join
   '("(custom-set-variables"
     " '(package-user-dir \"%s\"))"
     "(require 'package)"
     "(add-to-list 'package-archives (cons \"melpa\" \"https://melpa.org/packages/\") t)"
     "(package-initialize)"
     "(when (not package-archive-contents)"
     "  (package-refresh-contents))")
   "\n"))

(defvar elisp-venv-install-file-content
  (string-join
   '("(require '%1$s)"
     "(find-file (find-lisp-object-file-name '%1$s 'defvar))"
     "(let ((package (package-buffer-info)))"
     "  (package-download-transaction"
     "   (package-compute-transaction nil (package-desc-reqs package))))"
     "(print \"SUCCESS\")"
     "(kill-emacs 0)")
   "\n"))

(defun elisp-venv-current-package-name()
    (package-desc-name (save-excursion (package-buffer-info))))

(defun elisp-venv-create-directories(package-name)
  """Create the directories for installing dependcies etc.,
  return command that will actually do so"""
  (let* ((venv-directory (format elisp-venv-base-directory package-name))
	 (init-file-path (f-join venv-directory "init.el"))
	 (install-file-path (f-join venv-directory "install.el"))
	 (packages-dir (f-join venv-directory "packages")))
    (condition-case nil (make-directory venv-directory 't) (error nil))
    (f-write-text (format elisp-venv-init-file-content packages-dir) 'utf-8 init-file-path)
    (f-write-text (format elisp-venv-install-file-content package-name)
		  'utf-8 install-file-path)
    (condition-case nil (make-directory packages-dir) (error nil))
    (format "emacs --batch -q -L . -l %s -l %s &" init-file-path install-file-path)))

(defun create-package-sandbox ()
  """Create a sandbox directory with necessary files, spit out command to install"""
  (interactive)
  (let ((command (elisp-venv-create-directories (elisp-venv-current-package-name))))
    ;;(message command)))
    (shell-command command)))

(defun elisp-venv-delete-venv()
  """Delete the virtual environment for the package in buffer"""
  (interactive)
  (let* ((package-name (elisp-venv-current-package-name))
	 (venv-dir (format elisp-venv-base-directory package-name)))
    (if (f-exists? venv-dir)
	(progn (f-delete venv-dir 't)
	       (message "Deleted venv for %s" package-name))
      (message "No venv for %s" package-name))))
