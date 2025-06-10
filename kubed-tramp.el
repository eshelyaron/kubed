;;; kubed-tramp.el --- Kubed Tramp integration   -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

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

(defun kubed-tramp--unhex (x)
  ;; Simplified version of `url-unhex'.
  (if (> x ?9) (+ 10 (- x ?A)) (- x ?0)))

(defun kubed-tramp--decode-context-name (str)
  ;; Adopted from `url-unhex-string'.
  (let ((tmp "") (case-fold-search nil))
    (while (string-match "[.][0-9A-F][0-9A-F]" str)
      (let* ((start (match-beginning 0))
	     (ch1 (kubed-tramp--unhex (elt str (+ start 1))))
	     (code (+ (* 16 ch1)
		      (kubed-tramp--unhex (elt str (+ start 2))))))
	(setq tmp (concat tmp (substring str 0 start) (byte-to-string code))
	      str (substring str (match-end 0)))))
    (concat tmp str)))

(defun kubed-tramp--v2-context (vec)
  "Extract the context name from a kubernetes host name in VEC."
  (or (when-let ((host (and vec (tramp-file-name-host vec))))
        (shell-quote-argument
         (decode-coding-string
         (kubed-tramp--decode-context-name (nth 0 (split-string host "%")))
         'utf-8)))
      ""))

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
  ;; TODO: Dispatch based on method version.  The following is intended
  ;; for v2, although it also works for v1 in most cases.
  (decode-coding-string
   (kubed-tramp--decode-context-name
    (nth 0 (split-string
            (tramp-file-name-host (tramp-dissect-file-name file-name))
            "%")))
   'utf-8))

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

    (let ((params `((tramp-login-program ,kubed-kubectl-program)
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
                    (tramp-remote-shell-args ("-i" "-c")))))

      ;; Old version.
      (setf (alist-get "kubedv1" tramp-methods nil nil #'string=) params)

      (connection-local-set-profile-variables
       'kubed-tramp-connection-local-default-profile
       '((tramp-extra-expand-args
          ?a (kubed-tramp--container  (car tramp-current-connection))
          ?h (kubed-tramp--pod        (car tramp-current-connection))
          ?x (kubed-tramp--context    (car tramp-current-connection))
          ?y (kubed-tramp--namespace  (car tramp-current-connection)))))

      (connection-local-set-profiles
       '(:application tramp :protocol "kubedv1")
       'kubed-tramp-connection-local-default-profile)

      ;; New version.
      (setf (alist-get kubed-tramp-method tramp-methods nil nil #'string=) params)

      (connection-local-set-profile-variables
       'kubed-tramp-v2-connection-local-default-profile
       '((tramp-extra-expand-args
          ?a (kubed-tramp--container  (car tramp-current-connection))
          ?h (kubed-tramp--pod        (car tramp-current-connection))
          ?x (kubed-tramp--v2-context (car tramp-current-connection))
          ?y (kubed-tramp--namespace  (car tramp-current-connection)))))

      (connection-local-set-profiles
       `(:application tramp :protocol ,kubed-tramp-method)
       'kubed-tramp-v2-connection-local-default-profile))))

;;;###autoload (with-eval-after-load 'tramp (kubed-tramp-enable))

(provide 'kubed-tramp)
;;; kubed-tramp.el ends here
