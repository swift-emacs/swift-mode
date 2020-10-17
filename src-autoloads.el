;;; src-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "swift-mode" "swift-mode.el" (0 0 0 0))
;;; Generated autoloads from swift-mode.el

(let ((loads (get 'swift 'custom-loads))) (if (member '"swift-mode" loads) nil (put 'swift 'custom-loads (cons '"swift-mode" loads))))
 (custom-add-load 'languages 'swift-mode)
 (with-eval-after-load 'cus-load
  (custom-add-load 'languages 'swift-mode))

(defsubst swift-mode:add-supported-extension-for-speedbar nil "\
Register .swfit to speedbar." (if (fboundp (quote speedbar-add-supported-extension)) (speedbar-add-supported-extension ".swift") (add-hook (quote speedbar-load-hook) (lambda nil (speedbar-add-supported-extension ".swift")))))

(autoload 'swift-mode "swift-mode" "\
Major mode for editing Swift code.

\\{swift-mode-map}

\(fn)" t nil)
 (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))
 (swift-mode:add-supported-extension-for-speedbar)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "swift-mode" '("swift-mode")))

;;;***

;;;### (autoloads nil "swift-mode-beginning-of-defun" "swift-mode-beginning-of-defun.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from swift-mode-beginning-of-defun.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "swift-mode-beginning-of-defun" '("swift-mode:")))

;;;***

;;;### (autoloads nil "swift-mode-font-lock" "swift-mode-font-lock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from swift-mode-font-lock.el

(let ((loads (get 'swift-mode:faces 'custom-loads))) (if (member '"swift-mode-font-lock" loads) nil (put 'swift-mode:faces 'custom-loads (cons '"swift-mode-font-lock" loads))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "swift-mode-font-lock" '("swift-mode:")))

;;;***

;;;### (autoloads nil "swift-mode-imenu" "swift-mode-imenu.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from swift-mode-imenu.el

(let ((loads (get 'swift-mode:imenu 'custom-loads))) (if (member '"swift-mode-imenu" loads) nil (put 'swift-mode:imenu 'custom-loads (cons '"swift-mode-imenu" loads))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "swift-mode-imenu" '("swift-mode:")))

;;;***

;;;### (autoloads nil "swift-mode-indent" "swift-mode-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from swift-mode-indent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "swift-mode-indent" '("swift-mode:")))

;;;***

;;;### (autoloads nil "swift-mode-lexer" "swift-mode-lexer.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from swift-mode-lexer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "swift-mode-lexer" '("swift-mode:")))

;;;***

;;;### (autoloads nil "swift-mode-repl" "swift-mode-repl.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from swift-mode-repl.el

(let ((loads (get 'swift-mode:repl 'custom-loads))) (if (member '"swift-mode-repl" loads) nil (put 'swift-mode:repl 'custom-loads (cons '"swift-mode-repl" loads))))

(autoload 'swift-mode:run-repl "swift-mode-repl" "\
Run a Swift REPL process.

This function input and output via buffer `*CMD*' where CMD is replaced with
the CMD given.
If there is a process already running in `*CMD*', and DONT-SWITCH is nil,
switch to that buffer.
CMD is a string or a list, interpreted as a command line.  The default value is
`swift-mode:repl-executable'.  This function updates the buffer local variable
`swift-mode:repl-executable' with the given CMD if KEEP-DEFAULT is nil,
so it will be used as the default value for the next invocation in the current
buffer.
If KEEP-DEFAULT is non-nil, the `swift-mode:repl-executable' and the global
variable `swift-mode:repl-buffer' are not updated.  The buffer local variable
`swift-mode:repl-buffer' is always updated.
Runs the hook `swift-repl-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn CMD &optional DONT-SWITCH KEEP-DEFAULT)" t nil)

(defalias 'run-swift 'swift-mode:run-repl)

(autoload 'swift-mode:send-region "swift-mode-repl" "\
Send the current region to the inferior swift process.

START and END define region within current buffer

\(fn START END)" t nil)

(autoload 'swift-mode:send-buffer "swift-mode-repl" "\
Send the buffer to the Swift REPL process.

\(fn)" t nil)

(autoload 'swift-mode:build-swift-module "swift-mode-repl" "\
Build a Swift module in the PROJECT-DIRECTORY.

If PROJECT-DIRECTORY is nil or omitted, it is searched from `default-directory'
or its ancestors.
An list ARGS are appended for builder command line arguments.

\(fn &optional PROJECT-DIRECTORY ARGS)" t nil)

(autoload 'swift-mode:build-ios-app "swift-mode-repl" "\
Build an iOS app in the PROJECT-DIRECTORY.
Build it for iOS device DEVICE-IDENTIFIER for the given SCHEME.
If PROJECT-DIRECTORY is nil or omitted, it is searched from `default-directory'
or its ancestors.
DEVICE-IDENTIFIER is the device identifier of the iOS simulator.  If it is nil
or omitted, the value of `swift-mode:ios-device-identifier' is used.  If it is
equal to `swift-mode:ios-local-device-identifier', a local device is used via
`ios-deploy' instead.
SCHEME is the name of the project scheme in Xcode.  If it is nil or omitted,
the value of `swift-mode:ios-project-scheme' is used.

\(fn &optional PROJECT-DIRECTORY DEVICE-IDENTIFIER SCHEME)" t nil)

(autoload 'swift-mode:debug-swift-module "swift-mode-repl" "\
Run debugger on a Swift module in the PROJECT-DIRECTORY.

If PROJECT-DIRECTORY is nil or omitted, it is searched from `default-directory'
or its ancestors.

\(fn &optional PROJECT-DIRECTORY)" t nil)

(autoload 'swift-mode:debug-ios-app-on-device "swift-mode-repl" "\
Run debugger on an iOS app in the PROJECT-DIRECTORY.
Run it for the iOS local device DEVICE-IDENTIFIER for the given SCHEME.
CODESIGNING-FOLDER-PATH is the path of the codesigning folder in Xcode
build settings.

\(fn PROJECT-DIRECTORY SCHEME CODESIGNING-FOLDER-PATH)" nil nil)

(autoload 'swift-mode:debug-ios-app-on-simulator "swift-mode-repl" "\
Run debugger on an iOS app in the PROJECT-DIRECTORY.
Run it for the iOS simulator DEVICE-IDENTIFIER for the given SCHEME.
DEVICE-IDENTIFIER is the device identifier of the iOS simulator.
SCHEME is the name of the project scheme in Xcode.
CODESIGNING-FOLDER-PATH is the path of the codesigning folder used in Xcode
build settings.
PRODUCT-BUNDLE-IDENTIFIER is the name of the product bundle identifier used
in Xcode build settings.

\(fn PROJECT-DIRECTORY DEVICE-IDENTIFIER SCHEME CODESIGNING-FOLDER-PATH PRODUCT-BUNDLE-IDENTIFIER)" nil nil)

(autoload 'swift-mode:debug-ios-app "swift-mode-repl" "\
Run debugger on an iOS app in the PROJECT-DIRECTORY.
Run it for the iOS simulator device DEVICE-IDENTIFIER for the given SCHEME.
If PROJECT-DIRECTORY is nil or omitted, it is searched from `default-directory'
or its ancestors.
DEVICE-IDENTIFIER is the device identifier of the iOS simulator.  If it is
nil or omitted, the value of `swift-mode:ios-device-identifier' is used.  If
it is equal to `swift-mode:ios-local-device-identifier', a local build via
`ios-deploy' is generated instead.
SCHEME is the name of the project scheme in Xcode.  If it is nil or omitted,
the value of `swift-mode:ios-project-scheme' is used.

\(fn &optional PROJECT-DIRECTORY DEVICE-IDENTIFIER SCHEME)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "swift-mode-repl" '("swift-")))

;;;***

;;;### (autoloads nil "swift-mode-standard-types" "swift-mode-standard-types.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from swift-mode-standard-types.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "swift-mode-standard-types" '("swift-mode:")))

;;;***

;;;### (autoloads nil nil ("swift-mode-autoloads.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; src-autoloads.el ends here
