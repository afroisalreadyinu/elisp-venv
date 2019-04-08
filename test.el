(require 'ert)
(require 'f)
(require 'elisp-venv)
(require 'mocker)

(defvar fake-base-directory "~/.elisp-venv/the-package-master-sandbox")

(defvar fake-init-el
  (s-join
   "\n"
   (list "(custom-set-variables"
	 (format " '(package-user-dir \"%s\"))" fake-base-directory)
	 "(require 'package)"
	 "(add-to-list 'package-archives (cons \"melpa\" \"https://melpa.org/packages/\") t)"
	 "(package-initialize)"
	 "(when (not package-archive-contents)"
	 "  (package-refresh-contents))")))


(defmacro with-custom-var(custom-vars &rest body)
  """Run code with temporary customization options"""
  `(let ((old-values (mapcar (lambda (x) (cons (car x) (symbol-value (car x)))) ',custom-vars)))
     (unwind-protect
	 (progn
	   ,@(mapcar (lambda (x) `(setq ,(car x) ,(cadr x))) custom-vars)
	   ,@body)
       (mapcar (lambda (x) (set (car x) (cdr x))) old-values))
     ))

(ert-deftest test-dir-path-from-package-name ()
  (with-custom-var ((elisp-venv-use-git-branch t))
        (should (string-equal (elisp-venv-dir-path-from-package-name "the-package")
			(f-full "~/.elisp-venv/the-package-master-sandbox"))))
  (with-custom-var ((elisp-venv-use-git-branch nil))
	(should (string-equal (elisp-venv-dir-path-from-package-name "the-package")
			      (f-full "~/.elisp-venv/the-package-sandbox")))))

(ert-deftest test-create-directories ()
  (mocker-let ((make-directory (package-name)
			       ((:input (list fake-base-directory) :output 't)))
	       (f-write-text (text encoding file-path)
			     ((:input (list fake-init-el 'utf-8 "~/.elisp-venv/the-package-master-sandbox/init.el")))))
    (elisp-venv-create-directories "the-package")))



(ert 'test-create-directories)
