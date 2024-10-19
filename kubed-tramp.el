;;; kubed-tramp.el --- Kubed Tramp integration   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Keywords: tools

;;; Commentary:

;; This library provides Tramp integration for Kubed.  This is similar
;; to the built-in "kubernetes" Tramp method from tramp-container.el,
;; except that the Kubed method always requires the container name and
;; Kubernetes namespace to be specified, as well a `kubectl' context.
;; In other words, the filename syntax of this method is fully explicit.
;; An explicit syntax is useful when juggling between different contexts
;; and namespaces with Kubed; on the other hand, for finding a remote
;; file with C-x C-f, the concise syntax of the built-in "kubernetes"
;; method is probably more convenient.

;;; Code:

(require 'kubed-common)
(require 'tramp)

(defun kubed-tramp--context (vec)
  "Extract the context name from a kubernetes host name in VEC."
  (or (when-let ((host (and vec (tramp-file-name-host vec))))
        (nth 0 (split-string host "%")))
      ""))

(defun kubed-tramp--namespace (vec)
  "Extract the namespace from a kubernetes host name in VEC."
  (or (when-let ((host (and vec (tramp-file-name-host vec))))
        (nth 1 (split-string host "%")))
      ""))

(defun kubed-tramp--pod (vec)
  "Extract the pod name from a kubernetes host name in VEC."
  (or (when-let ((host (and vec (tramp-file-name-host vec))))
        (nth 2 (split-string host "%")))
      ""))

(defun kubed-tramp--container (vec)
  "Extract the container name from a kubernetes host name in VEC."
  (or (when-let ((host (and vec (tramp-file-name-host vec))))
        (nth 3 (split-string host "%")))
      ""))

;;;###autoload
(defun kubed-tramp-context (file-name)
  "Extract `kubectl' context from Kubed Tramp remote file name FILE-NAME."
  (nth 0 (split-string
          (tramp-file-name-host (tramp-dissect-file-name file-name)) "%")))

;;;###autoload
(defun kubed-tramp-namespace (file-name)
  "Extract Kubernetes namespace from Kubed Tramp remote file name FILE-NAME."
  (nth 1 (split-string
          (tramp-file-name-host (tramp-dissect-file-name file-name)) "%")))

;;;###autoload
(defun kubed-tramp-assert-support ()
  "Check if Kubed Tramp support is available, throw `user-error' if not."
  (unless (assoc kubed-tramp-method tramp-methods)
    (user-error "Kubed Tramp support requires Tramp version 2.7 or later")))

;;;###autoload
(defun kubed-tramp-enable ()
  "Enable Kubed integration with Tramp."
  (when (boundp 'tramp-extra-expand-args) ; Tramp 2.7+

    (setf (alist-get kubed-tramp-method tramp-methods nil nil #'string=)
          `((tramp-login-program ,kubed-kubectl-program)
            (tramp-login-args (("exec")
                               ("--context" "%x")
                               ("--namespace" "%y")
                               ("-c" "%a")
                               ("%h")
                               ("-it")
                               ("--")
		               ("%l")))
            (tramp-direct-async (,tramp-default-remote-shell "-c"))
            (tramp-remote-shell ,tramp-default-remote-shell)
            (tramp-remote-shell-login ("-l"))
            (tramp-remote-shell-args ("-i" "-c"))))

    (connection-local-set-profile-variables
     'kubed-tramp-connection-local-default-profile
     '((tramp-extra-expand-args
        ?a (kubed-tramp--container (car tramp-current-connection))
        ?h (kubed-tramp--pod       (car tramp-current-connection))
        ?x (kubed-tramp--context   (car tramp-current-connection))
        ?y (kubed-tramp--namespace (car tramp-current-connection)))))

    (connection-local-set-profiles
     `(:application tramp :protocol ,kubed-tramp-method)
     'kubed-tramp-connection-local-default-profile)))

;;;###autoload (with-eval-after-load 'tramp (kubed-tramp-enable))

(provide 'kubed-tramp)
;;; kubed-tramp.el ends here
