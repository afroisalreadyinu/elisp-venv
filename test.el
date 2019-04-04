(require 'ert)
(require 'f)
(require 'elisp-venv)

  ;;"""Run code with temporary customization options"""

(defmacro with-custom-var(custom-vars &rest body)
  `(let ((old-values (mapcar (lambda (x) (list (quote (car x)) (car x))) ,@custom-vars)))
     (unwind-protect
	 (progn ,@body))))

(ert-deftest test-dir-path-from-package-name ()
  (should (string-equal (elisp-venv-dir-path-from-package-name "the-package")
			(f-full "~/.elisp-venv/the-package-master-sandbox")))
  (should (string-equal (elisp-venv-dir-path-from-package-name "the-package")
			(f-full "~/.elisp-venv/the-package-sandbox"))))

(ert 't)
