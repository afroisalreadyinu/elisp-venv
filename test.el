(require 'ert)
(require 'f)
(require 'elisp-venv)

(defmacro with-custom-var(&rest custom-var-definitions)
  """Run code with temporary customization options"""
  `(let ((old-values ()))))

(ert-deftest test-dir-path-from-package-name ()
  (should (string-equal (elisp-venv-dir-path-from-package-name "the-package")
			(f-full "~/.elisp-venv/the-package-master-sandbox")))
  (setq elisp-venv-use-git-branch nil)
  (should (string-equal (elisp-venv-dir-path-from-package-name "the-package")
			(f-full "~/.elisp-venv/the-package-sandbox")))
  )

(ert 't)
