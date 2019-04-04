(require 'ert)
(require 'elisp-venv)

(defmacro with-custom-var(&rest custom-var-definitions)
  """Run code with temporary customization options"""
  `(let ((old-values ()))))

(ert-deftest test-something ()
  (fail))

(ert 't)
