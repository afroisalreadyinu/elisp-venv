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
   '("(require '%s)"
     "(find-file (find-lisp-object-file-name '%s 'defvar))"
     "(let ((package (package-buffer-info)))"
     "  (package-download-transaction"
     "   (package-compute-transaction nil (package-desc-reqs package))))")
   "\n"))

(defun create-package-sandbox ()
  """Create a sandbox directory with necessary files, spit out command to install"""
  (interactive)
  (let* ((package (save-excursion (package-buffer-info)))
	 (sandbox-directory (format "~/temp/%s-sandbox" (package-desc-name package)))
	 (init-file-path (abl-mode-concat-paths sandbox-directory "init.el"))
	 (install-file-path (abl-mode-concat-paths sandbox-directory "install.el"))
	 (packages-dir (abl-mode-concat-paths sandbox-directory "packages")))
    ;; create base directory
    (make-directory sandbox-directory)
    ;; write init.el
    (write-to-file init-file-path (format init-file-content packages-dir))
    ;; write install file
    (write-to-file install-file-path
		   (format install-file-content
			   (package-desc-name package)
			   (package-desc-name package)))
    (makedir packages-dir)
    (kill-new (format "emacs -q -L . -l %s -l %s"
		      init-file-path
		      install-file-path))))



(run-elisp-tests)
