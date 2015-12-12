;;; test-helper --- Test helper for swift-mode

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar swift-mode-test-path
  (f-dirname (f-this-file)))

(defvar swift-mode-root-path
  (f-parent swift-mode-test-path))

(defvar swift-mode-sandbox-path
  (f-expand "sandbox" swift-mode-test-path))

(when (f-exists? swift-mode-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" swift-mode-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory swift-mode-sandbox-path))
     (when (f-exists? swift-mode-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir swift-mode-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'cl-lib)
(require 's nil t)
(require 'dash nil t)

(require 'undercover)
(undercover "swift-mode.el")
(require 'swift-mode)

(provide 'test-helper)
;;; test-helper.el ends here
