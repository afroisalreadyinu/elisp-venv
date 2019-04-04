(require 'ert)
(require 'f)
(require 'elisp-venv)

  ;;"""Run code with temporary customization options"""

(defmacro with-custom-var(custom-vars &rest body)
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

(ert 't)
