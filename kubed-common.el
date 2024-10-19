;;; kubed-common.el --- Common definitons for Kubed   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Keywords: tools

;;; Commentary:

;; This library provides definitions that both `kubed' and `kubed-tramp'
;; require.

;;; Code:

(defvar kubed-tramp-method "kubedv1"    ;Versioned, for compatibility.
  ;; (find-file "/kubedv1:CONTEXT%NAMESPACE%POD%CONTAINER:/some/file")
  "Name of the Kubed Tramp method.")

(defcustom kubed-kubectl-program "kubectl"
  "Name of `kubectl' executable to use for interacting with Kubernetes."
  :type 'string
  :group 'kubed)

(provide 'kubed-common)
;;; kubed-tramp.el ends here
