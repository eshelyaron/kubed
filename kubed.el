;;; kubed.el --- Kubernetes, Emacs, done!   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Eshel Yaron

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Maintainer: Eshel Yaron <~eshel/kubed-devel@lists.sr.ht>
;; Keywords: tools kubernetes containers
;; URL: https://eshelyaron.com
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This library defines commands for interacting with Kubernetes
;; resources, such as Kuberenetes pods, services, deployments, and more.
;;
;; Use `kubed-display-pod' to display a Kuberenetes pod,
;; `kubed-edit-pod' to edit it, `kubed-delete-pods' to delete it, and
;; `kubed-list-pods' to see a menu of all pods.  You can create new pods
;; from YAML or JSON files with `kubed-create-pod'.  To update the list
;; of current pods, use `kubed-update-pods' or `kubed-update-all'.
;;
;; Similar commands are defined for other types of resources as well.
;;
;; This library interacts with Kuberenetes via `kubectl', and uses the
;; current `kubectl' context and namespace.  To change your current
;; Kuberenetes context or namespace, use `kubed-use-context' and
;; `kubed-set-namespace'; all resource lists are updated automatically
;; after you do so.  In addition, you can use the minor mode
;; `kubed-all-namespaces-mode' to see resources from all namespaces.
;; The prefix keymap `kubed-prefix-map' gives you quick access to these
;; and other useful commands.  You may want to bind it to a convenient
;; key in your global keymap with `keymap-global-set':
;;
;;   (keymap-global-set "C-c k" 'kubed-prefix-map)
;;
;; If you want to work with more or different types of Kubernetes
;; resources, use the macro `kubed-define-resource'.  This macro defines
;; some common functions and commands that'll get you started with ease.
;;
;; You may also want to try out the companion library `kubed-transient',
;; which provides transient menus for some of the commands defined here.

;;; Todo:

;; - Support filtering resource lists.

;;; Code:

(defgroup kubed nil
  "Kubernetes interface."
  :group 'tools)

(defcustom kubed-update-hook nil
  "List of functions that `kubed-update-all' runs."
  :type 'hook)

(defcustom kubed-kubectl-program "kubectl"
  "Name of `kubectl' executable to use for interacting with Kubernetes."
  :type 'string)

(defcustom kubed-yaml-setup-hook '(yaml-ts-mode view-mode)
  "List of functions to call in Kubernetes resource description YAML buffers.

The first function in the list should normally be the major mode to use,
by default it is `yaml-ts-mode'."
  :type 'hook)

(defcustom kubed-logs-setup-hook '(view-mode)
  "List of functions to call when setting up Kubernetes pod logs buffers."
  :type 'hook)

;;;###autoload
(defun kubed-update-all ()
  "Update all Kuberenetes resource lists."
  (interactive)
  (run-hooks 'kubed-update-hook))

(defvar-local kubed-frozen nil
  "Whether the current buffer shows a frozen list of Kuberenetes resources.

If a resource lists is frozen, then Emacs does not update it when
obtaining new information from Kuberenetes clusters.")

(defcustom kubed-name-column '("Name" 48 t)
  "Specification of name column in Kubernetes resource list buffers."
  :type '(list string natnum boolean))

(defcustom kubed-namespace-column '("Namespace" 12 t)
  "Specification of namespace column in Kubernetes resource list buffers."
  :type '(list string natnum boolean))

;;;###autoload
(define-minor-mode kubed-all-namespaces-mode
  "Show Kubernetes resources from all namespaces, not just current namespace."
  :global t
  (message "Kubed \"all namespaces\" mode is now %s"
           (if kubed-all-namespaces-mode "ON" "OFF")))

(defun kubed-list-mark-for-deletion ()
  "Mark Kubernetes resource at point for deletion."
  (interactive "" kubed-list-mode)
  (tabulated-list-put-tag (propertize "D" 'help-echo "Marked for deletion") t))

(defun kubed-list-unmark ()
  "Remove mark from Kubernetes resource at point."
  (interactive "" kubed-list-mode)
  (tabulated-list-put-tag " " t))

(defvar-keymap kubed-list-mode-map
  :doc "Common keymap for Kubernetes resource list buffers."
  "A" #'kubed-all-namespaces-mode
  "d" #'kubed-list-mark-for-deletion
  "u" #'kubed-list-unmark)

(define-derived-mode kubed-list-mode tabulated-list-mode "Kubernetes Resources"
  "Major mode for listing Kubernetes resources.

Modes for specific resource types, such as `kubed-pods-mode', use this
mode as their parent."
  :interactive nil
  (add-hook 'revert-buffer-restore-functions
            (lambda ()
              (let (marks)
                (save-excursion
                  (goto-char (point-min))
                  (while (not (eobp))
                    (unless (eq (char-after) ?\s)
                      (push (cons (tabulated-list-get-id)
                                  ;; Preserve mark properties.
                                  (buffer-substring (point) (1+ (point))))
                            marks))
                    (forward-line)))
                (lambda ()
                  (save-excursion
                    (goto-char (point-min))
                    (while (not (eobp))
                      (when-let ((mark (alist-get (tabulated-list-get-id) marks nil nil #'equal)))
                        (tabulated-list-put-tag mark))
                      (forward-line))))))
            nil t)
  (setq-local truncate-string-ellipsis (propertize ">" 'face 'shadow)))

;;;###autoload
(defmacro kubed-define-resource (resource &optional properties &rest commands)
  "Define Kubernetes RESOURCE with associated PROPERTIES and COMMANDS.

RESOURCE is a symbol corresponding to a Kubernetes resource type, such
as `pod' or `service'.  This macro defines the following commands for
interacting with Kubernetes RESOURCEs:

- `kubed-display-RESOURCE': prompt for a RESOURCE and display its
  description in YAML format.  See also `kubed-yaml-setup-hook'.
- `kubed-edit-RESOURCE': prompt for a RESOURCE and edit it.
- `kubed-delete-RESOURCE': prompt for a RESOURCE and delete it.
- `kubed-list-RESOURCEs': display a buffer listing all RESOURCEs in the
  current namespace.  The RESOURCEs list buffer uses a dedicated major
  mode, `kubed-RESOURCEs-mode', which is also defined by this macro.
- `kubed-update-RESOURCEs': update and repopulate RESOURCEs list.
- `kubed-create-RESOURCE': create a RESOURCE from a YAML or a JSON file.
- `kubed-explain-RESOURCEs': show buffer with help about RESOURCEs.

This macro also defines a prefix keymap, `kubed-RESOURCE-prefix-map',
with bindings for the above commands.

PROPERTIES is a list of lists (PROP JSON-PATH WIDTH SORT FORMAT . ATTRS)
that specify properties of RESOURCEs.  PROP is the name of the property,
as a symbol; JSON-PATH is a JSONPath expression that evaluates to the
value of PROP when applied to the JSON representation of a RESOURCE.
WIDTH, SORT, FORMAT and ATTRS are optional and can be omitted.  WIDTH is
used as the default width of the column corresponding to PROP in
RESOURCEs list buffers; SORT is sort predicate, a function that takes
two values of PROP as strings and return non-nil if the first should
sort before the second; FORMAT is a function that takes a value of PROP
and formats it; and ATTRS is a plist of additional attributes of the
PROP column, see `tabulated-list-format' for available attributes.  For
example, (phase \".status.phase\" 10) says that RESOURCE has a `phase'
property at JSONPath \".status.phase\" whose values are typically 10
columns wide.  The first property in PROPERTIES, is used to annotate
completion candidates when prompting for a RESOURCE.

COMMANDS is a list of elements (COMMAND KEYS DOC-PREFIX . BODY) that
define commands for RESOURCE list buffers.  COMMAND is a symbol
specifying the suffix of the command name, the full name of the command
is `kubed-RESOURCEs-COMMAND' (for example, `kubed-pods-shell'); KEYS is
either a string that specifies a key sequence to bind to the command in
`kubed-RESOURCEs-mode-map', or nil if the command should not be bound;
DOC-PREFIX is a string used to construct the docstring of the command,
this macro appends the string \" Kubernetes RESOURCE at point.\" to it
to obtain the final docstring; lastly, BODY is the body the command.
Within BODY, the variable RESOURCE is let-bound to the name of the
RESOURCE at point.  If RESOURCE is namespaced, then also the variable
`k8sns' is let-bound to the namespace of the RESOURCE at point within
BODY when `kubed-all-namespaces-mode' is enabled.  For example, if
RESOURCE is `pod', the following COMMANDS element defines a command
`kubed-pods-frob' and binds it to the key \"f\" in
`kubed-pods-mode-map':

  (frob \"f\" \"Frobnicate\"
        (message \"Preparing...\")
        (frobnicate-pod pod k8sns)
        (message \"Done.\"))

By default, this macro assumes that RESOURCE is namespaced.  To define a
namespaceless resource type, put `:namespaced nil' before COMMANDS:

  (kubed-define-resource global-thingy (PROP1 PROP2 ...) :namespaced nil
    CMD1
    CMD2
    ...)

Other keyword arguments that go between PROPERTIES and COMMANDS are:

- `:create (ARGLIST DOCSTRING INTERACTIVE BODY...)': specialize the
  resource creation command, `kubed-create-RESOURCE'.  ARGLIST,
  DOCSTRING, INTERACTIVE and BODY have the same meaning as in `defun'.
- `:prefix ((KEY LABEL DEFINITION) ...)': additional keybinding for the
  prefix keymap `kubed-RESOURCE-prefix-map' and the
  `kubed-RESOURCE-menu-map' menu.  Each element (KEY LABEL DEFINITION)
  says to bind KEY to DEFINITION in `kubed-RESOURCE-menu-map', and to
  add DEFINITION to `kubed-RESOURCE-menu-map' with the label LABEL.
- `:plural PLURAL': specify plural form of RESOURCE, as a symbol.  If
  you omit this keyword argument, the plural form defaults to RESOURCE
  followed by \"s\"."
  (declare (indent 2))
  (let ((hist-var (intern (format "kubed-%S-history"            resource)))
        (plrl-var (intern (format "%Ss"                         resource)))
        (read-fun (intern (format "kubed-read-%S"               resource)))
        (read-nms (intern (format "kubed-read-namespaced-%S"    resource)))
        (desc-fun (intern (format "kubed-%S-description-buffer" resource)))
        (buf-name         (format "*kubed-%S*"                  resource))
        (dsp-name (intern (format "kubed-display-%S"            resource)))
        (edt-name (intern (format "kubed-edit-%S"               resource)))
        (crt-name (intern (format "kubed-create-%S"             resource)))
        (map-name (intern (format "kubed-%S-prefix-map"         resource)))
        (menu-map (intern (format "kubed-%S-menu-map"           resource)))
        (namespaced t)
        (keyword nil)
        list-var ents-var hook-var proc-var frmt-var read-crm sure-fun
        ents-fun buff-fun frmt-fun affx-fun updt-cmd list-cmd expl-cmd
        exec-cmd list-buf out-name err-name dlt-errb dlt-name mod-name
        ctxt-fun crt-spec prf-keys)

    ;; Process keyword arguments.
    (while (keywordp (car commands))
      (setq keyword (pop commands))
      (cond
       ((eq keyword :namespaced) (setq namespaced (pop commands)))
       ((eq keyword :create)     (setq crt-spec   (pop commands)))
       ((eq keyword :prefix)     (setq prf-keys   (pop commands)))
       ((eq keyword :plural)     (setq plrl-var   (pop commands)))
       ;; FIXME: Add error for unknown keywords.
       (t (pop commands))))

    (setq list-var (intern (format "kubed-%S"               plrl-var))
          ents-var (intern (format "kubed--%S-entries"      plrl-var))
          hook-var (intern (format "kubed-update-%S-hook"   plrl-var))
          proc-var (intern (format "kubed-%S-process"       plrl-var))
          frmt-var (intern (format "kubed-%S-columns"       plrl-var))
          read-crm (intern (format "kubed-read-%S"          plrl-var))
          sure-fun (intern (format "kubed-ensure-%S"        plrl-var))
          ents-fun (intern (format "kubed-%S-entries"       plrl-var))
          buff-fun (intern (format "kubed-%S-buffer"        plrl-var))
          frmt-fun (intern (format "kubed-%S-format"        plrl-var))
          affx-fun (intern (format "kubed-%S-affixation"    plrl-var))
          updt-cmd (intern (format "kubed-update-%S"        plrl-var))
          list-cmd (intern (format "kubed-list-%S"          plrl-var))
          expl-cmd (intern (format "kubed-explain-%S"       plrl-var))
          exec-cmd (intern (format "kubed-%S-execute"       plrl-var))
          list-buf         (format "*kubed-%S*"             plrl-var)
          out-name         (format " *kubed-get-%S*"        plrl-var)
          err-name         (format " *kubed-get-%S-stderr*" plrl-var)
          dlt-errb         (format " *kubed-%S-execute-stderr*" plrl-var)
          dlt-name (intern (format "kubed-delete-%S"        plrl-var))
          mod-name (intern (format "kubed-%S-mode"          plrl-var))
          ctxt-fun (intern (format "kubed-%S-context-menu"  plrl-var)))

    ;; Extend `commands' with standard commands.
    ;; Commands appear in reverse order in context menu.
    (dolist (c `((patch "P" "Patch"
                        (kubed-patch ,(symbol-name plrl-var) ,resource
                                     (kubed-read-patch)
                                     . ,(when namespaced '(k8sns))))
                 (display "C-o" "Display description of"
                          (display-buffer
                           (,desc-fun ,resource . ,(when namespaced '(k8sns)))))
                 (get-in-other-window
                  "o" "Pop to buffer showing description of"
                  (switch-to-buffer-other-window
                   (,desc-fun ,resource . ,(when namespaced '(k8sns)))))
                 (delete "D" "Delete"
                         ,(if namespaced
                              `(if k8sns
                                   (when (y-or-n-p
                                          (format ,(concat "Delete Kubernetes "
                                                           (symbol-name resource)
                                                           " `%s' in namespace `%s'?")
                                                  ,resource k8sns))
                                     (,dlt-name (list (list ,resource k8sns))))
                                 (when (y-or-n-p
                                        (format ,(concat "Delete Kubernetes "
                                                         (symbol-name resource)
                                                         " `%s'?")
                                                ,resource))
                                   (,dlt-name (list ,resource))))
                            `(when (y-or-n-p
                                    (format ,(concat "Delete Kubernetes "
                                                     (symbol-name resource)
                                                     " `%s'?")
                                            ,resource))
                               (,dlt-name (list ,resource)))))
                 (edit "e" "Edit"
                       ,(if namespaced
                            `(,edt-name ,resource k8sns)
                          `(,edt-name ,resource)))
                 (get "RET" "Switch to buffer showing description of"
                      (switch-to-buffer
                       (,desc-fun ,resource . ,(when namespaced '(k8sns)))))))
      (push c commands))

    ;; Generate code.
    `(progn
       (defvar ,hist-var nil
         ,(format "History list for `%S'." read-fun))
       (defvar ,list-var nil
         ,(format "List of Kubernetes resources of type `%S'." resource))
       (defvar ,hook-var nil
         ,(format "List of functions to run after updating `%S'." list-var))
       (defvar ,proc-var nil
         ,(format "Process that updates Kubernetes resources of type `%S'." resource))

       (defun ,sure-fun ()
         ,(format "Populate `%S', if not already populated." list-var)
         (unless (or ,list-var (process-live-p ,proc-var)) (,updt-cmd)))

       (defun ,updt-cmd (&optional silent)
         ,(format "Update `%S'.
Non-nil optional argument SILENT says to inhibit progress messages."
                  list-var)
         (interactive)
         (when (process-live-p ,proc-var) (delete-process ,proc-var))
         (with-current-buffer (get-buffer-create ,out-name)
           (erase-buffer))
         (setq ,proc-var
               (make-process
                :name ,(format "*kubed-get-%S*" plrl-var)
                :buffer ,out-name
                :stderr ,err-name
                :command (list
                          kubed-kubectl-program
                          "get" ,(format "%S" plrl-var)
                          ,@(when namespaced
                              `((concat "--all-namespaces="
                                        (if kubed-all-namespaces-mode "true" "false"))))
                          (format "--output=custom-columns=%s"
                                  (string-join
                                   (cons "NAME:.metadata.name"
                                         ,(if namespaced
                                              `(append
                                                (when kubed-all-namespaces-mode
                                                  '("NAMESPACE:.metadata.namespace"))
                                                ',(mapcar (lambda (p)
                                                            (concat (upcase (symbol-name (car p)))
                                                                    ":"
                                                                    (cadr p)))
                                                          properties))
                                            `',(mapcar (lambda (p)
                                                         (concat (upcase (symbol-name (car p)))
                                                                 ":"
                                                                 (cadr p)))
                                                       properties)))
                                   ",")))
                :sentinel
                (lambda (_proc status)
                  (cond
                   ((string= status "finished\n")
                    (let (new offsets eol)
                      (with-current-buffer ,out-name
                        (goto-char (point-min))
                        (setq eol (pos-eol))
                        (while (re-search-forward "[^ ]+" eol t)
                          (push (1- (match-beginning 0)) offsets))
                        (setq offsets (nreverse offsets))
                        (forward-char 1)
                        (while (not (eobp))
                          (let ((cols nil)
                                (beg (car offsets))
                                (ends (append (cdr offsets) (list (- (pos-eol) (point))))))
                            ,@(let ((read-col
                                     (lambda (p)
                                       ;; Fresh list to avoid circles.
                                       (list `(push ,(if-let ((f (nth 4 p)))
                                                         `(funcall ,f (string-trim (buffer-substring
                                                                                    (+ (point) beg)
                                                                                    (+ (point) (car ends)))))
                                                       `(string-trim (buffer-substring
                                                                      (+ (point) beg)
                                                                      (+ (point) (car ends)))))
                                                    cols)
                                             '(setq beg (pop ends))))))
                                (if namespaced
                                    ;; Resource is namespaced, generate
                                    ;; code that is sensitive to
                                    ;; `kubed-all-namespaces-mode'.
                                    `((if kubed-all-namespaces-mode
                                          (progn
                                            ,@(mapcan
                                               read-col
                                               ;; Two nils, one for the
                                               ;; name column, another
                                               ;; for the namespace.
                                               `(nil nil . ,properties)))
                                        ,@(mapcan read-col `(nil . ,properties))))
                                  ;; Non-namespaced.
                                  (mapcan read-col `(nil . ,properties))))
                            (push (nreverse cols) new))
                          (forward-line 1)))
                      (setq ,list-var new
                            ,proc-var nil)
                      (run-hooks ',hook-var)
                      (unless silent
                        (message ,(format "Updated Kubernetes %S." plrl-var)))))
                   ((string= status "exited abnormally with code 1\n")
                    (with-current-buffer ,err-name
                      (goto-char (point-max))
                      (insert "\n" status))
                    (display-buffer ,err-name))))))
         (unless silent
           (minibuffer-message ,(format "Updating Kubernetes %S..." plrl-var))))

       (defun ,affx-fun (,plrl-var)
         ,(format "Return Kubernetes %s with completion affixations."
                  (upcase (symbol-name plrl-var)))
         (let ((max (seq-max (cons 0 (mapcar #'string-width ,plrl-var)))))
           (mapcar (lambda (,resource)
                     (list ,resource ""
                           (concat (make-string (1+ (- max (string-width ,resource))) ?\s)
                                   (propertize (or (cadr (assoc ,resource ,list-var)) "")
                                               'face 'completions-annotations))))
                   ,plrl-var)))

       (defun ,read-fun (prompt &optional default multi
                                . ,(when namespaced '(k8sns)))
         ,(format "Prompt with PROMPT for a Kubernetes %S name.

Optional argument DEFAULT is the minibuffer default argument.

Non-nil optional argument MULTI says to read and return a list
of %S, instead of just one." resource plrl-var)
         (minibuffer-with-setup-hook
             #',sure-fun
           (funcall
            (if multi #'completing-read-multiple #'completing-read)
            (format-prompt prompt default)
            ,(if namespaced
                 `(if (or (null k8sns) (string= k8sns (kubed-current-namespace)))
                      ;; Current namespace.
                      (lambda (s p a)
                        (if (eq a 'metadata)
                            '(metadata
                              (category . ,(intern (format "kubernetes-%S" resource)))
                              ,@(when properties
                                  `((affixation-function . ,affx-fun))))
                          (while (and (process-live-p ,proc-var)
                                      (null ,list-var))
                            (accept-process-output ,proc-var 1))
                          (complete-with-action a ,list-var s p)))
                    ;; Different namespace.
                    (let ((table 'unset))
                      (lambda (s p a)
                        (if (eq a 'metadata)
                            '(metadata
                              (category . ,(intern (format "kubernetes-%S" resource))))
                          (when (eq table 'unset)
                            (setq table (kubed-resource-names
                                         ,(symbol-name plrl-var) k8sns)))
                          (complete-with-action a table s p)))))
               `(lambda (s p a)
                  (if (eq a 'metadata)
                      '(metadata
                        (category . ,(intern (format "kubernetes-%S" resource)))
                        ,@(when properties
                            `((affixation-function . ,affx-fun))))
                    (while (and (process-live-p ,proc-var)
                                (null ,list-var))
                      (accept-process-output ,proc-var 1))
                    (complete-with-action a ,list-var s p))))
            nil 'confirm nil ',hist-var default)) )

       (defun ,read-crm (prompt &optional default)
         ,(format "Prompt with PROMPT for Kubernetes %S names.

Optional argument DEFAULT is the minibuffer default argument." resource)
         (,read-fun prompt default t))

       (defun ,desc-fun (,resource . ,(when namespaced '(&optional k8sns)))
         ,(format "Return buffer describing Kubernetes %S %s"
                  resource (upcase (symbol-name resource)))
         (let* ((buf (get-buffer-create ,buf-name))
                (fun (lambda (&optional _ _)
                       (let ((inhibit-read-only t)
                             (target (current-buffer)))
                         (buffer-disable-undo)
                         (with-temp-buffer
                           (unless (zerop
                                    (call-process
                                     kubed-kubectl-program nil t nil "get"
                                     ,(symbol-name resource) "--output=yaml" ,resource
                                     . ,(when namespaced
                                          '((if k8sns
                                                (concat "--namespace=" k8sns)
                                              "--all-namespaces=false")))))
                             (error ,(format "`kubectl get %S' failed" resource)))
                           (let ((source (current-buffer)))
                             (with-current-buffer target
                               (replace-buffer-contents source)
                               (set-buffer-modified-p nil)
                               (buffer-enable-undo))))))))
           (with-current-buffer buf
             (funcall fun)
             (goto-char (point-min))
             (run-hooks 'kubed-yaml-setup-hook)
             (setq-local revert-buffer-function fun))
           buf))

       ,@(when namespaced
           `((defun ,read-nms (prompt &optional default multi)
               (let* ((choice
                       (funcall
                        (if multi #'completing-read-multiple #'completing-read)
                        (format-prompt prompt default)
                        (lambda (s p a)
                          (if (eq a 'metadata)
                              '(metadata
                                (category
                                 . ,(intern (format "kubernetes-namespaced-%S" resource))))
                            (while (and (process-live-p ,proc-var)
                                        (null ,list-var))
                              (accept-process-output ,proc-var 1))
                            (complete-with-action a (mapcar (pcase-lambda (`(,name ,space . ,_))
                                                              (concat name " " space))
                                                            ,list-var)
                                                  s p)))
                        nil 'confirm nil ',hist-var default))
                      (split (mapcar (lambda (c)
                                       (split-string c " "))
                                     (ensure-list choice))))
                 (if multi split (car split))))))

       (defun ,dsp-name (,resource . ,(when namespaced '(&optional k8sns)))
         ,(format "Display Kubernetes %S %s."
                  resource (upcase (symbol-name resource)))
         (interactive ,(if namespaced
                           `(if kubed-all-namespaces-mode
                                (,read-nms "Display")
                              (list (,read-fun "Display")))
                         `(list (,read-fun "Display"))))
         (display-buffer (,desc-fun ,resource . ,(when namespaced '(k8sns)))))

       (add-hook 'kubed-update-hook #',updt-cmd)

       ,@(when namespaced
           `((add-hook 'kubed-all-namespaces-mode-hook
                       (lambda ()
                         (setq ,list-var nil)
                         (,updt-cmd)))))

       (defun ,edt-name (,resource . ,(when namespaced '(&optional k8sns)))
         ,(format "Edit Kubernetes %S %s."
                  resource (upcase (symbol-name resource)))
         (interactive ,(if namespaced
                           `(if kubed-all-namespaces-mode
                                (,read-nms "Edit")
                              (list (,read-fun "Edit")))
                         `(list (,read-fun "Edit"))))
         (unless (bound-and-true-p server-process) (server-start))
         (let ((process-environment
                (cons ,(if (<= 30 emacs-major-version)
                           '(concat "KUBE_EDITOR=" emacsclient-program-name)
                         "KUBE_EDITOR=emacsclient")
                      process-environment)))
           (start-process ,(format "*kubed-%S-edit*" plrl-var) nil
                          kubed-kubectl-program "edit"
                          ,(symbol-name resource) ,resource
                          . ,(when namespaced
                               `((if k8sns
                                     (concat "--namespace=" k8sns)
                                   "-o=yaml"))))))

       ,(if namespaced
            `(defun ,dlt-name (,plrl-var)
               ,(format "Delete Kubernetes %S %s."
                        plrl-var (upcase (symbol-name plrl-var)))
               (interactive (if kubed-all-namespaces-mode
                                (,read-nms "Delete" nil t)
                              (list (,read-crm "Delete"))))
               (unless ,plrl-var
                 (user-error ,(format "You didn't specify %S to delete" plrl-var)))
               (if kubed-all-namespaces-mode
                   (pcase-dolist (`(,name ,space) ,plrl-var)
                     (message ,(concat "Deleting Kubernetes "
                                       (symbol-name resource)
                                       " `%s' in namespace `%s'...")
                              name space)
                     (if (zerop (call-process
                                 kubed-kubectl-program nil nil nil
                                 "delete" "--namespace" space
                                 ,(symbol-name plrl-var) name))
                         (message ,(concat "Deleting Kubernetes "
                                           (symbol-name resource)
                                           " `%s' in namespace `%s'... Done.")
                                  name space)
                       (error ,(concat "Failed to delete Kubernetes"
                                       (symbol-name resource)
                                       " `%s' in namespace `%s'")
                              name space)))
                 (message ,(concat "Deleting Kubernetes "
                                   (symbol-name plrl-var)
                                   " `%s'...")
                          (string-join ,plrl-var "', `"))
                 (if (zerop (apply #'call-process
                                   kubed-kubectl-program nil nil nil
                                   "delete" ,(symbol-name plrl-var) ,plrl-var))
                     (message ,(concat "Deleting Kubernetes "
                                       (symbol-name plrl-var)
                                       " `%s'... Done.")
                              (string-join ,plrl-var "', `"))
                   (error ,(concat "Failed to delete Kubernetes"
                                   (symbol-name plrl-var)
                                   " `%s'")
                          (string-join ,plrl-var "', `")))))
          `(defun ,dlt-name (,plrl-var)
             ,(format "Delete Kubernetes %S %s." plrl-var
                      (upcase (symbol-name plrl-var)))
             (interactive (list (,read-crm "Delete")))
             (unless ,plrl-var
               (user-error ,(format "You didn't specify %S to delete" plrl-var)))
             (message ,(concat "Deleting Kubernetes "
                               (symbol-name plrl-var)
                               " `%s'...")
                      (string-join ,plrl-var "', `"))
             (if (zerop (apply #'call-process
                               kubed-kubectl-program nil nil nil
                               "delete" ,(symbol-name plrl-var) ,plrl-var))
                 (message ,(concat "Deleting Kubernetes "
                                   (symbol-name plrl-var)
                                   " `%s'... Done.")
                          (string-join ,plrl-var "', `"))
               (error ,(concat "Failed to delete Kubernetes"
                               (symbol-name plrl-var)
                               " `%s'")
                      (string-join ,plrl-var "', `")))))

       (defvar-local ,ents-var nil)

       (defun ,ents-fun ()
         ,(format "`tabulated-list-entries' function for `%s'." mod-name)
         (mapcar
          (lambda (c) (list ,(if namespaced
                                 `(if kubed-all-namespaces-mode
                                      (concat (car c) " " (cadr c))
                                    (car c))
                               `(car c))
                            (apply #'vector c)))
          ,ents-var))

       (defun ,exec-cmd ()
         ,(format "Delete marked Kubernetes %S." plrl-var)
         (interactive "" ,mod-name)
         (let (delete-list)
           (save-excursion
             (goto-char (point-min))
             (while (not (eobp))
               (when (eq (char-after) ?D)
                 (push (tabulated-list-get-id) delete-list))
               (forward-line)))
           (if delete-list
               (when (y-or-n-p (format ,(concat "Delete %d marked Kubernetes "
                                                (symbol-name plrl-var) "?")
                                       (length delete-list)))
                 ,@(if namespaced
                       `((if kubed-all-namespaces-mode
                             (save-excursion
                               (goto-char (point-min))
                               (while (not (eobp))
                                 (when (member (tabulated-list-get-id) delete-list)
                                   (tabulated-list-put-tag
                                    (propertize "K" 'help-echo "Deletion in progress"))
                                   (let* ((k8sent (tabulated-list-get-entry))
                                          (name (aref k8sent 0))
                                          (space (aref k8sent 1)))
                                     (make-process
                                      :name ,(format "*kubed-%S-execute*" plrl-var)
                                      :stderr ,dlt-errb
                                      :command (list kubed-kubectl-program
                                                     "delete"
                                                     "--namespace" space
                                                     ,(symbol-name plrl-var)
                                                     name)
                                      :sentinel (lambda (_proc status)
                                                  (cond
                                                   ((string= status "finished\n")
                                                    (message (format ,(concat "Deleted Kubernetes "
                                                                              (symbol-name resource)
                                                                              " `%s' in namespace `%s'.")
                                                                     name space))
                                                    (,updt-cmd t))
                                                   ((string= status "exited abnormally with code 1\n")
                                                    (with-current-buffer ,dlt-errb
                                                      (goto-char (point-max))
                                                      (insert "\n" status))
                                                    (display-buffer ,dlt-errb)))))))
                                 (forward-line)))
                           (save-excursion
                             (goto-char (point-min))
                             (while (not (eobp))
                               (when (member (tabulated-list-get-id) delete-list)
                                 (tabulated-list-put-tag
                                  (propertize "K" 'help-echo "Deletion in progress")))
                               (forward-line)))
                           (make-process
                            :name ,(format "*kubed-%S-execute*" plrl-var)
                            :stderr ,dlt-errb
                            :command (append
                                      (list kubed-kubectl-program
                                            "delete" ,(symbol-name plrl-var))
                                      delete-list)
                            :sentinel (lambda (_proc status)
                                        (cond
                                         ((string= status "finished\n")
                                          (message (format ,(concat "Deleted %d marked Kubernetes "
                                                                    (symbol-name plrl-var) ".")
                                                           (length delete-list)))
                                          (,updt-cmd t))
                                         ((string= status "exited abnormally with code 1\n")
                                          (with-current-buffer ,dlt-errb
                                            (goto-char (point-max))
                                            (insert "\n" status))
                                          (display-buffer ,dlt-errb)))))))
                     `((save-excursion
                         (goto-char (point-min))
                         (while (not (eobp))
                           (when (member (tabulated-list-get-id) delete-list)
                             (tabulated-list-put-tag
                              (propertize "K" 'help-echo "Deletion in progress")))
                           (forward-line)))
                       (make-process
                        :name ,(format "*kubed-%S-execute*" plrl-var)
                        :stderr ,dlt-errb
                        :command (append
                                  (list kubed-kubectl-program
                                        "delete" ,(symbol-name plrl-var))
                                  delete-list)
                        :sentinel (lambda (_proc status)
                                    (cond
                                     ((string= status "finished\n")
                                      (message (format ,(concat "Deleted %d marked Kubernetes "
                                                                (symbol-name plrl-var) ".")
                                                       (length delete-list)))
                                      (,updt-cmd t))
                                     ((string= status "exited abnormally with code 1\n")
                                      (with-current-buffer ,dlt-errb
                                        (goto-char (point-max))
                                        (insert "\n" status))
                                      (display-buffer ,dlt-errb))))))))
             (user-error ,(format "No Kubernetes %S marked for deletion" plrl-var)))))

       ,(if crt-spec `(defun ,crt-name . ,crt-spec)
          `(defun ,crt-name (definition)
             ,(format "Create Kubernetes %s from definition file DEFINITION."
                      (symbol-name resource))
             (interactive (list (kubed-read-resource-definition-file-name
                                 ,(symbol-name resource))))
             (kubed-create definition ,(symbol-name resource))
             (,updt-cmd t)))

       ,@(mapcar
          (pcase-lambda (`(,suffix ,_key ,desc . ,body))
            `(defun ,(intern (format "kubed-%S-%S" plrl-var suffix)) (click)
               ,(format "%s Kubernetes %S at point." desc resource)
               (interactive (list last-nonmenu-event) ,mod-name)
               (if-let ((pos (mouse-set-point click))
                        . ,(if namespaced
                               `((k8sent (tabulated-list-get-entry pos))
                                 (,resource (aref k8sent 0)))
                             `((,resource (tabulated-list-get-id pos)))))
                   ,(if namespaced
                        `(let ((k8sns (when kubed-all-namespaces-mode
                                        (aref (tabulated-list-get-entry pos) 1))))
                           ,@body)
                      `(progn ,@body))
                 (user-error ,(format "No Kubernetes %S at point" resource)))))
          commands)

       (defvar-keymap ,(intern (format "kubed-%S-mode-map" plrl-var))
         :doc ,(format "Keymap for `%S" mod-name)
         "G"   #',updt-cmd
         "x"   #',exec-cmd
         "+"   #',crt-name
         ,@(mapcan
            (pcase-lambda (`(,suffix ,key ,_desc . ,_body))
              (when key
                (list key `#',(intern (format "kubed-%S-%S" plrl-var suffix)))))
            commands))

       (defvar ,frmt-var
         ',(let ((i 0)
                 (res nil))
             (dolist (p properties)
               (setq i (1+ i))
               (push
                (append
                 (list (capitalize (symbol-name (car p)))
                       (caddr p)
                       (if-let ((sorter (cadddr p)))
                           `(lambda (l r)
                              ,(if namespaced
                                   `(let ((c (+ ,i (if kubed-all-namespaces-mode 1 0))))
                                      (funcall ,sorter (aref (cadr l) c) (aref (cadr r) c)))
                                 `(funcall ,sorter (aref (cadr l) ,i) (aref (cadr r) ,i))))
                         t))
                 (nthcdr 5 p))
                res))
             (reverse res)))

       (defun ,frmt-fun ()
         (apply #'vector
                (cons
                 kubed-name-column
                 ,(if namespaced
                      `(append
                        (when kubed-all-namespaces-mode
                          (list kubed-namespace-column))
                        ,frmt-var)
                    frmt-var))))

       (defun ,ctxt-fun (menu click)
         (when (tabulated-list-get-entry (posn-point (event-start click)))
           ,@(mapcar
              (pcase-lambda (`(,suffix ,_key ,desc . ,_body))
                `(define-key
                  menu [,(intern (format "kubed-%S-%S" plrl-var suffix))]
                  (list 'menu-item ,(format "%s this %S" desc resource)
                        #',(intern (format "kubed-%S-%S" plrl-var suffix)))))
              (reverse commands)))
         menu)

       (define-derived-mode ,mod-name kubed-list-mode
         (list ,(format "Kubernetes %ss" (capitalize (symbol-name resource)))
               (list ',proc-var
                     (list :propertize "[...]" 'help-echo "Updating...")))
         ,(format "Major mode for listing Kubernetes %S." plrl-var)
         :interactive nil
         (setq tabulated-list-format (,frmt-fun))
         (setq tabulated-list-entries #',ents-fun)
         (setq tabulated-list-padding 2)
         (add-hook 'context-menu-functions #',ctxt-fun nil t)
         (tabulated-list-init-header))

       (defun ,buff-fun (,plrl-var &optional buffer frozen)
         (with-current-buffer (or buffer (get-buffer-create ,list-buf))
           (,mod-name)
           (let* ((buf (current-buffer))
                  (fun (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (unless kubed-frozen
                               (setq ,ents-var ,list-var)
                               (setq tabulated-list-format (,frmt-fun))
                               (tabulated-list-init-header)
                               (revert-buffer)))))))
             (add-hook ',hook-var fun)
             (add-hook 'kill-buffer-hook
                       (lambda () (remove-hook ',hook-var fun))
                       nil t))
           (setq kubed-frozen frozen)
           (setq ,ents-var ,plrl-var)
           (tabulated-list-print)
           (current-buffer)))

       (defun ,list-cmd ()
         ,(format "List Kubernetes %S." plrl-var)
         (interactive)
         (,sure-fun)
         (pop-to-buffer (,buff-fun ,list-var)))

       (defun ,expl-cmd ()
         ,(format "Show help buffer with explanation about Kubernetes %S." plrl-var)
         (interactive)
         (kubed-explain ,(symbol-name plrl-var)))

       (defvar-keymap ,map-name
         :doc ,(format "Prefix keymap for Kubed %S commands." resource)
         :prefix ',map-name
         "l" #',list-cmd
         "+" #',crt-name
         "e" #',edt-name
         "d" #',dlt-name
         "g" #',dsp-name
         "u" #',updt-cmd
         "E" #',expl-cmd
         ,@(mapcan
            (pcase-lambda (`(,key ,_label ,cmd))
              (list key `#',cmd))
            prf-keys))

       (defvar-keymap ,menu-map
         :doc ,(format "Keymap with Kubernetes %S related menu entries." resource)
         :prefix ',menu-map
         "<list>"    '("List"           . ,list-cmd)
         "<create>"  '("Create"         . ,crt-name)
         "<edit>"    '("Edit"           . ,edt-name)
         "<delete>"  '("Delete"         . ,dlt-name)
         "<display>" '("Display"        . ,dsp-name)
         "<update>"  '("Update"         . ,updt-cmd)
         "<explain>" '("Explain Fields" . ,expl-cmd)
         ,@(mapcan
            (pcase-lambda (`(,key ,label ,cmd))
              (list key `'(,label . ,cmd)))
            prf-keys)))))

(setf
 ;; Teach Imenu about `kubed-define-resource'.
 (alist-get "KubedResource" lisp-imenu-generic-expression nil nil #'equal)
 (list (concat "^\\s-*(kubed-define-resource\\s-+\\("
               lisp-mode-symbol-regexp
               "\\)")
       1))

(defmacro kubed--static-if (condition then-form &rest else-forms)
  "A conditional compilation macro.
Evaluate CONDITION at macro-expansion time.  If it is non-nil, expand
the macro to THEN-FORM.  Otherwise expand it to ELSE-FORMS enclosed in a
‘progn’ form.  ELSE-FORMS may be empty.

This is the same as `static-if' from Emacs 30, defined here for
compatibility with earlier Emacs versions."
  (declare (indent 2)
           (debug (sexp sexp &rest sexp)))
  (if (eval condition lexical-binding)
      then-form
    (cons 'progn else-forms)))

;;;###autoload (autoload 'kubed-display-pod "kubed" nil t)
;;;###autoload (autoload 'kubed-edit-pod "kubed" nil t)
;;;###autoload (autoload 'kubed-delete-pods "kubed" nil t)
;;;###autoload (autoload 'kubed-list-pods "kubed" nil t)
;;;###autoload (autoload 'kubed-create-pod "kubed" nil t)
;;;###autoload (autoload 'kubed-pod-prefix-map "kubed" nil t 'keymap)
(kubed-define-resource pod
    ((phase ".status.phase" 10
            nil                         ; sorting function
            (lambda (ph)
              (if-let ((face (pcase ph
                               ;; TODO: Define/derive bespoke faces.
                               ("Pending"   'warning)
                               ("Running"   'success)
                               ("Succeeded" 'shadow)
                               ("Failed"    'error))))
                  (propertize ph 'face face)
                ph)))
     (ready ".status.containerStatuses[?(.ready==true)].name" 6
            (lambda (l r) (< (string-to-number l) (string-to-number r)))
            (lambda (cs)
              (if (string= cs "<none>") "0"
                (number-to-string (1+ (seq-count (lambda (c) (= c ?,)) cs)))))
            :right-align t)
     (total ".status.containerStatuses[*].name" 6
            (lambda (l r) (< (string-to-number l) (string-to-number r)))
            (lambda (cs)
              (if (string= cs "<none>") "0"
                (number-to-string (1+ (seq-count (lambda (c) (= c ?,)) cs)))))
            :right-align t)
     (starttime ".status.startTime" 20))
  :prefix (("L" "Show Logs"    kubed-logs)
           ("A" "Attach"       kubed-attach)
           ("X" "Execute"      kubed-exec)
           ("F" "Forward Port" kubed-forward-port-to-pod))
  (dired "C-d" "Start Dired in home directory of first container of"
         ;; Explicit namespace in Kuberenetes remote file names
         ;; introduced in Emacs 31.  See Bug#59797.
         (kubed--static-if (<= 31 emacs-major-version)
             (let ((ns (when k8sns (concat "%" k8sns))))
               (dired (concat "/kubernetes:" pod ns ":")))
           (when k8sns
             (if (y-or-n-p
                  (format "Starting Dired in a pod in a different namespace \
requires Emacs 31 or later.
You can proceed by first switching your current namespace.
Switch to namespace `%s' and proceed?" k8sns))
                 (kubed-set-namespace k8sns)
               (user-error
                "Cannot start Dired in a pod in different namespace `%s'"
                k8sns)))
           (dired (concat "/kubernetes:" pod ":"))))
  (shell "s" "Start shell in home directory of first container of"
         (kubed--static-if (<= 31 emacs-major-version)
             (let* ((ns (when k8sns (concat "%" k8sns)))
                    (default-directory (concat "/kubernetes:" pod ns ":")))
               (shell (format "*kubed-pod-%s-shell*" pod)))
           (when k8sns
             (if (y-or-n-p
                  (format "Starting Shell in a pod in a different namespace \
requires Emacs 31 or later.
You can proceed by first switching your current namespace.
Switch to namespace `%s' and proceed?" k8sns))
                 (kubed-set-namespace k8sns)
               (user-error
                "Cannot start Dired in a pod in different namespace `%s'"
                k8sns)))
           (let* ((default-directory (concat "/kubernetes:" pod ":")))
             (shell (format "*kubed-pod-%s-shell*" pod)))))
  (attach "a" "Attach to remote process running on"
          (kubed-attach pod (kubed-read-container pod "Container" t k8sns)
                        k8sns t t))
  (exec "X" "Execute command in"
        (let ((container (kubed-read-container pod "Container" t k8sns))
              (cmd-args (split-string-and-unquote
                         (read-string "Execute command: "))))
          (kubed-exec pod (car cmd-args) container k8sns t t (cdr cmd-args))))
  (logs "l" "Show logs for a container of"
        (kubed-logs pod (kubed-read-container pod "Container" t k8sns)))
  (forward-port "F" "Forward local network port to remote port of"
                (let ((local-port (read-number "Forward local port: ")))
                  (kubed-forward-port-to-pod
                   pod local-port
                   (read-number (format "Forward local port %d to remote port: "
                                        local-port))
                   k8sns))))

;;;###autoload (autoload 'kubed-display-namespace "kubed" nil t)
;;;###autoload (autoload 'kubed-edit-namespace "kubed" nil t)
;;;###autoload (autoload 'kubed-delete-namespaces "kubed" nil t)
;;;###autoload (autoload 'kubed-list-namespaces "kubed" nil t)
;;;###autoload (autoload 'kubed-create-namespace "kubed" nil t)
;;;###autoload (autoload 'kubed-namespace-prefix-map "kubed" nil t 'keymap)
(kubed-define-resource namespace
    ((phase ".status.phase" 10
            nil                         ; sorting function
            (lambda (ph)
              (if-let ((face (pcase ph
                               ;; TODO: Define/derive bespoke faces.
                               ("Active"      'success)
                               ("Terminating" 'shadow))))
                  (propertize ph 'face face)
                ph)))
     (creationtimestamp ".metadata.creationTimestamp" 20))
  :namespaced nil
  :prefix (("S" "Set" kubed-set-namespace))
  :create
  ((name) "Create Kubernetes namespace with name NAME."
   (interactive (list (read-string "Create namespace with name: ")))
   (unless (zerop
            (call-process
             kubed-kubectl-program nil nil nil
             "create" "namespace" name))
     (user-error "Failed to create Kubernetes namespace with name `%s'" name))
   (message "Created Kubernetes namespace with name `%s'." name)
   (kubed-update-namespaces t))
  (set "s" "Set current namespace to"
       (save-excursion
         (goto-char (point-min))
         (while (not (eobp))
           (when (eq (char-after) ?*)
             (tabulated-list-put-tag " "))
           (forward-line)))
       (kubed-set-namespace namespace)
       (tabulated-list-put-tag
        (propertize "*" 'help-echo "Current namespace"))))

;;;###autoload (autoload 'kubed-display-persistentvolume "kubed" nil t)
;;;###autoload (autoload 'kubed-edit-persistentvolume "kubed" nil t)
;;;###autoload (autoload 'kubed-delete-persistentvolumes "kubed" nil t)
;;;###autoload (autoload 'kubed-list-persistentvolumes "kubed" nil t)
;;;###autoload (autoload 'kubed-create-persistentvolume "kubed" nil t)
;;;###autoload (autoload 'kubed-persistentvolume-prefix-map "kubed" nil t 'keymap)
(kubed-define-resource persistentvolume () :namespaced nil)

;;;###autoload (autoload 'kubed-display-service "kubed" nil t)
;;;###autoload (autoload 'kubed-edit-service "kubed" nil t)
;;;###autoload (autoload 'kubed-delete-services "kubed" nil t)
;;;###autoload (autoload 'kubed-list-services "kubed" nil t)
;;;###autoload (autoload 'kubed-create-service "kubed" nil t)
;;;###autoload (autoload 'kubed-service-prefix-map "kubed" nil t 'keymap)
(kubed-define-resource service
    ((type ".spec.type" 12)
     (clusterip ".spec.clusterIP" 16)
     (externalip ".spec.externalIPs[*]" 16)
     (ports ".spec.ports[*].port" 6)
     (creationtimestamp ".metadata.creationTimestamp" 20)))

;;;###autoload (autoload 'kubed-display-secret "kubed" nil t)
;;;###autoload (autoload 'kubed-edit-secret "kubed" nil t)
;;;###autoload (autoload 'kubed-delete-secrets "kubed" nil t)
;;;###autoload (autoload 'kubed-list-secrets "kubed" nil t)
;;;###autoload (autoload 'kubed-create-secret "kubed" nil t)
;;;###autoload (autoload 'kubed-secret-prefix-map "kubed" nil t 'keymap)
(kubed-define-resource secret
    ((type ".type" 32) (creationtimestamp ".metadata.creationTimestamp" 20)))

(defun kubed-create-job-from-cronjob (name cronjob &optional namespace)
  "Create Kubernetes job with name NAME from cronjob CRONJOB.

Optional argument NAMESPACE is the namespace to use for the job,
defaulting to the current namespace."
  (interactive
   (let ((name (read-string "Create job with name: "))
         (namespace (seq-some
                     (lambda (arg)
                       (when (string-match "--namespace=\\(.+\\)" arg)
                         (match-string 1 arg)))
                     (kubed-transient-args 'kubed-transient-create-job))))
     (list name
           (kubed-read-cronjob
            (format "Create job `%s' from cronjob" name) nil nil namespace)
           namespace)))
  (unless (zerop
           (apply #'call-process
                  kubed-kubectl-program nil nil nil
                  "create" "job" name "--from" (concat "cronjob/" cronjob)
                  (when namespace (list "-n" namespace))))
    (user-error "Failed to create Kubernetes job `%s'" name))
  (message "Created Kubernetes job `%s'." name)
  (kubed-update-jobs t))

;;;###autoload (autoload 'kubed-display-job "kubed" nil t)
;;;###autoload (autoload 'kubed-edit-job "kubed" nil t)
;;;###autoload (autoload 'kubed-delete-jobs "kubed" nil t)
;;;###autoload (autoload 'kubed-list-jobs "kubed" nil t)
;;;###autoload (autoload 'kubed-create-job "kubed" nil t)
;;;###autoload (autoload 'kubed-job-prefix-map "kubed" nil t 'keymap)
(kubed-define-resource job
    ((status ".status.conditions[0].type" 10) (starttime ".status.startTime" 20))
  :prefix (("c" "Create from Cron" kubed-create-job-from-cronjob))
  :create
  ((name image &optional namespace command)
   "Create Kubernetes job with name NAME executing COMMAND in IMAGE.

Optional argument NAMESPACE is the namespace to use for the job,
defaulting to the current namespace."
   (interactive
    (let ((name (read-string "Create job with name: "))
          (image nil) (namespace nil) (command nil))
      (dolist (arg (kubed-transient-args 'kubed-transient-create-job))
        (cond
         ((string-match "--image=\\(.+\\)" arg)
          (setq image (match-string 1 arg)))
         ((string-match "--namespace=\\(.+\\)" arg)
          (setq namespace (match-string 1 arg)))
         ((string-match "-- =\\(.+\\)" arg)
          (setq command (split-string-and-unquote (match-string 1 arg))))))
      (unless image
        (setq image (kubed-read-container-image "Image to run in job")))
      (list name image namespace command)))
   (unless (zerop
            (apply #'call-process
                   kubed-kubectl-program nil nil nil
                   "create" "job" name "--image" image
                   (append
                    (when namespace (list "-n" namespace))
                    (when command (cons "--" command)))))
     (user-error "Failed to create Kubernetes job `%s'" name))
   (message "Created Kubernetes job `%s'." name)
   (kubed-update-jobs t)))

;;;###autoload
(defun kubed-watch-deployment-status (dep &optional namespace)
  "Show and update status of Kubernetes deployment DEP in a dedicate buffer.

Optional argument NAMESPACE is the namespace of DEP, defaulting to the
current namespace."
  (interactive
   (let ((namespace (seq-some
                     (lambda (arg)
                       (when (string-match "--namespace=\\(.+\\)" arg)
                         (match-string 1 arg)))
                     (kubed-transient-args 'kubed-transient-rollout))))
     (list (kubed-read-deployment "Watch deployment status" nil nil namespace)
           namespace)))
  (let ((buf (get-buffer-create "*kubed-deployment-status*")))
    (with-current-buffer buf (erase-buffer))
    (make-process
     :name "*kubed-watch-deployment-status*"
     :buffer buf
     :command `(,kubed-kubectl-program "rollout" "status" "deployment" ,dep
                                       ,@(when namespace `("-n" ,namespace)))
     :sentinel
     (lambda (_proc status)
       (when (member status
                     '("finished\n" "exited abnormally with code 1\n"))
         (kubed-update-deployments))))
    (display-buffer buf)))

(defcustom kubed-restart-deployment-watch-status t
  "Whether to pop up a progress buffer when restarting Kubernetes deployments."
  :type 'boolean)

;;;###autoload
(defun kubed-restart-deployment (dep &optional namespace)
  "Restart Kubernetes deployment DEP in namespace NAMESPACE.
If NAMESPACE is nil or omitted, it defaults to the current namespace."
  (interactive
   (let ((namespace (seq-some
                     (lambda (arg)
                       (when (string-match "--namespace=\\(.+\\)" arg)
                         (match-string 1 arg)))
                     (kubed-transient-args 'kubed-transient-rollout))))
     (list (kubed-read-deployment "Restart deployment" nil nil namespace)
           namespace)))
  (unless (zerop
           (apply #'call-process
                  kubed-kubectl-program nil nil nil
                  "rollout" "restart" "deployment" dep
                  (when namespace (list "-n" namespace))))
    (user-error "Failed to restart Kubernetes deployment `%s'" dep))
  (message "Restarting Kubernetes deployment `%s'." dep)
  (when kubed-restart-deployment-watch-status
    (kubed-watch-deployment-status dep namespace)))

;;;###autoload (autoload 'kubed-display-deployment "kubed" nil t)
;;;###autoload (autoload 'kubed-edit-deployment "kubed" nil t)
;;;###autoload (autoload 'kubed-delete-deployments "kubed" nil t)
;;;###autoload (autoload 'kubed-list-deployments "kubed" nil t)
;;;###autoload (autoload 'kubed-create-deployment "kubed" nil t)
;;;###autoload (autoload 'kubed-deployment-prefix-map "kubed" nil t 'keymap)
(kubed-define-resource deployment
    (( ready ".status.readyReplicas" 6
       (lambda (l r) (< (string-to-number l) (string-to-number r)))
       (lambda (s) (if (string= s "<none>") "0" s))
       :right-align t)
     ( updated ".status.updatedReplicas" 8
       (lambda (l r) (< (string-to-number l) (string-to-number r)))
       (lambda (s) (if (string= s "<none>") "0" s))
       :right-align t)
     ( available ".status.availableReplicas" 10
       (lambda (l r) (< (string-to-number l) (string-to-number r)))
       (lambda (s) (if (string= s "<none>") "0" s))
       :right-align t)
     ( reps ".status.replicas" 4
       (lambda (l r) (< (string-to-number l) (string-to-number r)))
       nil                              ; formatting function
       :right-align t)
     ( creationtimestamp ".metadata.creationTimestamp" 20))
  :prefix (("R" "Restart" kubed-restart-deployment)
           ("W" "Watch"   kubed-watch-deployment-status))
  :create
  ((name images &optional namespace replicas port command)
   "Deploy IMAGES to Kubernetes in deployment with name NAME.

Optional argument NAMESPACE is the namespace to use for the deployment,
defaulting to the current namespace, REPLICAS in the number of replicas
to create for each image, PORT is the port to expose, and COMMAND is an
optional command to run in the images."
   (interactive
    (let ((name (read-string "Create deployment with name: "))
          (images nil)
          (replicas (prefix-numeric-value current-prefix-arg))
          (port nil)
          (command nil)
          (namespace nil))
      (dolist (arg (kubed-transient-args 'kubed-transient-create-deployment))
        (cond
         ((string-match "--replicas=\\(.+\\)" arg)
          (setq replicas (string-to-number (match-string 1 arg))))
         ((string-match "--image=\\(.+\\)" arg)
          (push (match-string 1 arg) images))
         ((string-match "--port=\\(.+\\)" arg)
          (setq port (string-to-number (match-string 1 arg))))
         ((string-match "--namespace=\\(.+\\)" arg)
          (setq namespace (match-string 1 arg)))
         ((string-match "-- =\\(.+\\)" arg)
          (setq command (split-string-and-unquote (match-string 1 arg))))))
      (unless images
        (setq images (kubed-read-container-image "Images to deploy" nil t)))
      (list name images namespace replicas port command)))
   (unless (zerop
            (apply #'call-process
                   kubed-kubectl-program nil nil nil
                   "create" "deployment" name
                   (append
                    (mapcar (lambda (image) (concat "--image=" image)) images)
                    (when namespace (list (concat "--namespace=" namespace)))
                    (when replicas (list (format "--replicas=%d" replicas)))
                    (when port (list (format "--port=%d" port)))
                    (when command (cons "--" command)))))
     (user-error "Failed to create Kubernetes deployment `%s'" name))
   (message "Created Kubernetes deployment `%s'." name)
   (kubed-update-deployments t))
  (restart "R" "Restart"
           (kubed-restart-deployment deployment k8sns)
           (unless kubed-restart-deployment-watch-status
             (kubed-update-deployments t)))
  (watch "W" "Watch" (kubed-watch-deployment-status deployment k8sns)))

;;;###autoload (autoload 'kubed-display-replicaset "kubed" nil t)
;;;###autoload (autoload 'kubed-edit-replicaset "kubed" nil t)
;;;###autoload (autoload 'kubed-delete-replicasets "kubed" nil t)
;;;###autoload (autoload 'kubed-list-replicasets "kubed" nil t)
;;;###autoload (autoload 'kubed-create-replicaset "kubed" nil t)
;;;###autoload (autoload 'kubed-replicaset-prefix-map "kubed" nil t 'keymap)
(kubed-define-resource replicaset
    ((reps ".status.replicas" 4
           (lambda (l r) (< (string-to-number l) (string-to-number r)))
           nil                           ; formatting function
           :right-align t)
     (ownerkind ".metadata.ownerReferences[0].kind" 12)
     (ownername ".metadata.ownerReferences[0].name" 16)
     (creationtimestamp ".metadata.creationTimestamp" 20)))

;;;###autoload (autoload 'kubed-display-statefulset "kubed" nil t)
;;;###autoload (autoload 'kubed-edit-statefulset "kubed" nil t)
;;;###autoload (autoload 'kubed-delete-statefulsets "kubed" nil t)
;;;###autoload (autoload 'kubed-list-statefulsets "kubed" nil t)
;;;###autoload (autoload 'kubed-create-statefulset "kubed" nil t)
;;;###autoload (autoload 'kubed-statefulset-prefix-map "kubed" nil t 'keymap)
(kubed-define-resource statefulset
    ((reps ".status.replicas" 4
           (lambda (l r) (< (string-to-number l) (string-to-number r)))
           nil                          ; formatting function
           :right-align t)
     (ownerkind ".metadata.ownerReferences[0].kind" 12)
     (ownername ".metadata.ownerReferences[0].name" 16)
     (creationtimestamp ".metadata.creationTimestamp" 20)))

(defun kubed-cronjob-suspended-p (cj &optional ns)
  "Return non-nil if cronjob CJ in namespace NS is currently suspended."
  (equal (car (apply #'process-lines
                     kubed-kubectl-program
                     "get" "cronjobs" cj
                     "-o" "custom-columns=SUSPENDED:.spec.suspend" "--no-headers"
                     (when ns (list "-n" ns))))
         "true"))

;;;###autoload (autoload 'kubed-display-cronjob "kubed" nil t)
;;;###autoload (autoload 'kubed-edit-cronjob "kubed" nil t)
;;;###autoload (autoload 'kubed-delete-cronjobs "kubed" nil t)
;;;###autoload (autoload 'kubed-list-cronjobs "kubed" nil t)
;;;###autoload (autoload 'kubed-create-cronjob "kubed" nil t)
;;;###autoload (autoload 'kubed-cronjob-prefix-map "kubed" nil t 'keymap)
(kubed-define-resource cronjob
    ((schedule ".spec.schedule" 20)
     (suspend ".spec.suspend" 20)
     (lastschedule ".status.lastScheduleTime" 20)
     (lastsuccess ".status.lastSuccessfulTime" 20)
     (activejob ".status.active[0].name" 36))
  :create
  ((name image schedule &optional namespace command)
   "Schedule IMAGE to run in a cronjob with name NAME according to SCHEDULE.

Optional argument NAMESPACE is the namespace to use for the cronjob,
defaulting to the current namespace.  COMMAND is a list of strings that
represent a program followed by its arguments, if it non-nil then it
overrides the default command IMAGE runs."
   (interactive
    (let ((name (read-string "Create cronjob with name: "))
          (image nil)
          (schedule nil)
          (command nil)
          (namespace nil))
      (dolist (arg (kubed-transient-args 'kubed-transient-create-cronjob))
        (cond
         ((string-match "--image=\\(.+\\)" arg)
          (setq image (match-string 1 arg)))
         ((string-match "--schedule=\\(.+\\)" arg)
          (setq schedule (match-string 1 arg)))
         ((string-match "--namespace=\\(.+\\)" arg)
          (setq namespace (match-string 1 arg)))
         ((string-match "-- =\\(.+\\)" arg)
          (setq command (split-string-and-unquote (match-string 1 arg))))))
      (unless image
        (setq image (kubed-read-container-image "Image to run")))
      (unless schedule
        (setq schedule (read-string "Cron schedule: " "* * * * *")))
      (list name image schedule namespace command)))
   (unless (zerop
            (apply #'call-process
                   kubed-kubectl-program nil nil nil
                   "create" "cronjob" name
                   "--image" image "--schedule" schedule
                   (append
                    (when namespace (list "--namespace" namespace))
                    (when command (cons "--" command)))))
     (user-error "Failed to create Kubernetes cronjob `%s'" name))
   (message "Created Kubernetes cronjob `%s'." name)
   (kubed-update-cronjobs t))
  ;; Commands in *kubed-cronjobs* buffer.
  ( toggle-suspension "T" "Toggle suspension of"
    (kubed-patch "cronjobs" cronjob
                 (format
                  "{\"spec\": {\"suspend\": %s}}"
                  (if (kubed-cronjob-suspended-p cronjob k8sns) "false" "true"))
                 k8sns)
    (kubed-update-cronjobs t))
  ( create-job "j" "Create job from"
    (kubed-create-job-from-cronjob
     (read-string "Create job with name: ") cronjob k8sns)
    (kubed-update-jobs t)
    (kubed-update-cronjobs t)))

;;;###autoload (autoload 'kubed-display-ingressclass "kubed" nil t)
;;;###autoload (autoload 'kubed-edit-ingressclass "kubed" nil t)
;;;###autoload (autoload 'kubed-delete-ingressclasses "kubed" nil t)
;;;###autoload (autoload 'kubed-list-ingressclasss "kubed" nil t)
;;;###autoload (autoload 'kubed-create-ingressclass "kubed" nil t)
;;;###autoload (autoload 'kubed-ingressclass-prefix-map "kubed" nil t 'keymap)
(kubed-define-resource ingressclass
    ((controller ".spec.controller" 32)
     (creationtimestamp ".metadata.creationTimestamp" 20))
  :namespaced nil
  :plural ingressclasses)

;;;###autoload (autoload 'kubed-display-ingress "kubed" nil t)
;;;###autoload (autoload 'kubed-edit-ingress "kubed" nil t)
;;;###autoload (autoload 'kubed-delete-ingresses "kubed" nil t)
;;;###autoload (autoload 'kubed-list-ingresss "kubed" nil t)
;;;###autoload (autoload 'kubed-create-ingress "kubed" nil t)
;;;###autoload (autoload 'kubed-ingress-prefix-map "kubed" nil t 'keymap)
(kubed-define-resource ingress
    ((class ".spec.ingressClassName" 8)
     (creationtimestamp ".metadata.creationTimestamp" 20))
  :plural ingresses
  :create
  ((name rules &optional namespace class default-backend annotations)
   "Create Kubernetes ingress with name NAME and rules RULES.

Optional argument NAMESPACE is the namespace to use for the ingress,
defaulting to the current namespace.  CLASS is the ingress class,
ANNOTATIONS are a list of annotations for the created ingress, and
DEFAULT-BACKEND is the service to use as a backend for unhandled URLs."
   (interactive
    (let ((name (read-string "Create ingress with name: "))
          (rules nil)
          (namespace nil)
          (class nil)
          (annotations nil)
          (default-backend nil))
      (dolist (arg (kubed-transient-args 'kubed-transient-create-ingress))
        (cond
         ((string-match "--rule=\\(.+\\)" arg)
          (push (match-string 1 arg) rules))
         ((string-match "--namespace=\\(.+\\)" arg)
          (setq namespace (match-string 1 arg)))
         ((string-match "--class=\\(.+\\)" arg)
          (setq class (match-string 1 arg)))
         ((string-match "--default-backend=\\(.+\\)" arg)
          (setq default-backend (match-string 1 arg)))
         ((string-match "--annotation=\\(.+\\)" arg)
          (push (match-string 1 arg) annotations))))
      (unless rules (setq rules (kubed-read-ingress-rules)))
      (list name rules namespace class default-backend annotations)))
   (unless (zerop
            (apply #'call-process
                   kubed-kubectl-program nil nil nil
                   "create" "ingress" name
                   (append
                    (mapcan (lambda (rule) (list "--rule" rule)) rules)
                    (when namespace (list "--namespace" namespace))
                    (when class (list "--class" class))
                    (when default-backend
                      (list "--default-backend" default-backend))
                    (mapcan (lambda (ann) (list "--annotation" ann))
                            annotations))))
     (user-error "Failed to create Kubernetes ingress `%s'" name))
   (message "Created Kubernetes ingress `%s'." name)
   (kubed-update-ingresses t)))

;; TODO: Events may be numerous.  Need to only get a few.
;; ;;;###autoload (autoload 'kubed-list-events "kubed" nil t)
;; ;;;###autoload (autoload 'kubed-event-prefix-map "kubed" nil t 'keymap)
;; (kubed-define-resource event
;;     ((last ".lastTimestamp" 20)
;;      (type ".type" 10)
;;      (reason ".reason" 20)
;;      (objectkind ".involvedObject.kind" 12)
;;      (objectname ".involvedObject.name" 16)
;;      (message ".message" 36)))

(defun kubed-contexts ()
  "Return list of Kubernetes contexts."
  (process-lines kubed-kubectl-program "config" "get-contexts" "-o" "name"))

(defun kubed-current-context ()
  "Return current Kubernetes context."
  (car (process-lines kubed-kubectl-program "config" "current-context")))

(defvar kubed-context-history nil
  "History list for `kubed-read-context'.")

(defun kubed-read-context (prompt &optional default)
  "Prompt with PROMPT for a Kubernetes context.

Optional argument DEFAULT is the minibuffer default argument."
  (completing-read (format-prompt prompt default)
                   (kubed-contexts)
                   nil 'confirm nil 'kubed-context-history default))

;;;###autoload
(defun kubed-use-context (context)
  "Set current Kubernetes context to CONTEXT."
  (interactive
   (list (kubed-read-context "Use context" (kubed-current-context))))
  (unless (zerop
           (call-process
            kubed-kubectl-program nil nil nil
            "config" "use-context" context))
    (user-error "Failed to use Kubernetes context `%s'" context))
  (message "Now using Kubernetes context `%s'." context)
  (kubed-update-all))

;;;###autoload
(defun kubed-rename-context (old new)
  "Rename Kubernetes context OLD to NEW."
  (interactive
   (let ((old (kubed-read-context "Rename context" (kubed-current-context))))
     (list old (read-string (format-prompt "Rename context to" old)
                            nil 'kubed-context-history old))))
  (unless (zerop
           (call-process
            kubed-kubectl-program nil nil nil
            "config" "rename-context" old new))
    (user-error "Failed to rename Kubernetes context `%s' to `%s'" old new))
  (message "Renamed Kubernetes context `%s' to `%s'." old new))

;;;###autoload
(defun kubed-display-config ()
  "Display current Kubernetes client settings in a YAML buffer."
  (interactive)
  (let* ((buf (get-buffer-create "*kubed-config*"))
         (fun (lambda (&optional _ _)
                (let ((inhibit-read-only t)
                      (target (current-buffer)))
                  (buffer-disable-undo)
                  (with-temp-buffer
                    (unless (zerop
                             (call-process
                              kubed-kubectl-program nil t nil "config" "view"))
                      (error "`kubectl config view'"))
                    (let ((source (current-buffer)))
                      (with-current-buffer target
                        (replace-buffer-contents source)
                        (set-buffer-modified-p nil)
                        (buffer-enable-undo))))))))
    (with-current-buffer buf
      (funcall fun)
      (goto-char (point-min))
      (run-hooks 'kubed-yaml-setup-hook)
      (setq-local revert-buffer-function fun))
    (display-buffer buf)))

(defun kubed-current-namespace (&optional context)
  "Return current Kubernetes namespace for context CONTEXT."
  (car (process-lines
        kubed-kubectl-program
        "config" "view" "-o"
        (format "jsonpath={.contexts[?(.name==\"%s\")].context.namespace}"
                (or context (kubed-current-context))))))

;;;###autoload
(defun kubed-set-namespace (ns)
  "Set current Kubernetes namespace to NS."
  (interactive
   (list (kubed-read-namespace "Set namespace" (kubed-current-namespace))))
  (unless (zerop
           (call-process
            kubed-kubectl-program nil nil nil
            "config" "set-context" "--current" "--namespace" ns))
    (user-error "Failed to set Kubernetes namespace to `%s'" ns))
  (message "Kubernetes namespace is now `%s'." ns)
  (kubed-update-all))

(defcustom kubed-read-resource-definition-filter-files-by-kind t
  "Whether to filter file completion candidates by their Kubernetes \"kind\".

If this is non-nil, `kubed-read-resource-definition-file-name' only
suggests files with the right \"kind\" as completion candidates when you
call it with non-nil KIND argument.  This is useful because you get more
relevant completions, but it may become slow in directories with many
large JSON and YAML files, in which case you can set this option to nil."
  :type 'boolean)

(defun kubed-read-resource-definition-file-name (&optional kind)
  "Prompt for Kubernetes resource definition file name.

Optional argument KIND is the kind of resource the file should define.
If `kubed-read-resource-definition-filter-files-by-kind' is non-nil,
this function suggests only files that define resources of kind KIND as
completion candidates."
  (read-file-name
   (format "%s definition file: " (or kind "Resource")) nil nil t nil
   (if (and kind kubed-read-resource-definition-filter-files-by-kind
            (executable-find "grep"))
       (let ((cache (make-hash-table :test 'equal)))
         (lambda (f)
           (or (file-directory-p f)
               (when-let ((ext (and (string-match "\\.[^.]*\\'" f)
                                    (substring f (1+ (match-beginning 0))))))
                 (or (and (member ext '("yaml" "yml"))
                          (pcase (gethash (expand-file-name f) cache 'noval)
                            ('noval
                             (puthash (expand-file-name f)
                                      (zerop (call-process
                                              "grep" f nil nil "-i"
                                              (format "^kind: %s$" kind)))
                                      cache))
                            (val val)))
                     (and (equal ext "json")
                          (pcase (gethash (expand-file-name f) cache 'noval)
                            ('noval
                             (puthash (expand-file-name f)
                                      (zerop (call-process
                                              "grep" f nil nil "-i"
                                              (format "^kind: %s$" kind)))
                                      cache))
                            (val val))))))))
     (lambda (f)
       (or (file-directory-p f)
           (when (string-match "\\.[^.]*\\'" f)
             (member (substring f (1+ (match-beginning 0)))
                     '("yaml" "yml" "json"))))))))

;;;###autoload
(defun kubed-apply (config &optional kind)
  "Apply CONFIG to Kubernetes resource of kind KIND."
  (interactive
   (list (or (seq-some
              (lambda (arg)
                (when (string-match "--filename=\\(.+\\)" arg)
                  (match-string 1 arg)))
              (kubed-transient-args 'kubed-transient-apply))
             (kubed-read-resource-definition-file-name))))
  (let ((kind (or kind "resource")))
    (message "Applying Kubernetes %s configuration `%s'..." kind config)
    (call-process kubed-kubectl-program nil nil nil
                  "apply" "-f" (expand-file-name config))
    (message "Applying Kubernetes %s configuration `%s'... Done." kind config)))

;;;###autoload
(defun kubed-create (definition &optional kind)
  "Create Kubernetes resource of kind KIND with definition DEFINITION."
  (interactive
   (list (or (seq-some
              (lambda (arg)
                (when (string-match "--filename=\\(.+\\)" arg)
                  (match-string 1 arg)))
              (kubed-transient-args 'kubed-transient-create))
             (kubed-read-resource-definition-file-name))))
  (let ((kind (or kind "resource")))
    (message "Creating Kubernetes %s with definition `%s'..." kind definition)
    (message "Creating Kubernetes %s with definition `%s'... Done.  New %s name is `%s'."
             kind definition kind
             (car (process-lines kubed-kubectl-program
                                 "create" "-f" (expand-file-name definition)
                                 "-o" "jsonpath={.metadata.name}")))))

;;;###autoload
(defun kubed-run
    (pod image &optional namespace port attach stdin tty rm envs command args)
  "Run IMAGE in Kubernetes POD.

Optional argument NAMESPACE is the namespace to use for the created pod,
defaulting to the current namespace.  PORT is the port to expose,
defaulting to none.  If ATTACH is non-nil, then attach to the created
image with a `comint-mode' buffer, and pop to that buffer.  Non-nil
STDIN says to keep the standard input of the container open; non-nil TTY
says to allocate a TTY for the container; and non-nil RM says to remove
the container after it exits.  ENVS is a list of strings \"VAR=VAL\"
which specify environment variables VAR and values VAL to give them in
the created container.  ARGS are command line arguments for the
container command.  If COMMAND is non-nil, ARGS consist of a complete
command line, that overrides the container command instead of just
providing it with arguments."
  (interactive
   (let ((pod (read-string "Run image in pod with name: "))
         (image nil) (port nil) (namespace nil) (attach nil) (stdin nil)
         (tty nil) (rm nil) (envs nil) (command nil) (args nil))
     (dolist (arg (kubed-transient-args 'kubed-transient-run))
       (cond
        ((string-match "--image=\\(.+\\)" arg)
         (setq image (match-string 1 arg)))
        ((string-match "--port=\\(.+\\)" arg)
         (setq port (string-to-number (match-string 1 arg))))
        ((string-match "--namespace=\\(.+\\)" arg)
         (setq namespace (match-string 1 arg)))
        ((equal "--attach" arg) (setq attach t))
        ((equal "--stdin" arg) (setq stdin t))
        ((equal "--tty" arg) (setq tty t))
        ((equal "--rm" arg) (setq rm t))
        ((equal "--command" arg) (setq command t))
        ((string-match "--env=\\(.+\\)" arg)
         (push (match-string 1 arg) envs))
        ((string-match "-- =\\(.+\\)" arg)
         (setq args (split-string-and-unquote (match-string 1 arg))))))
     (unless image
       (setq image (read-string "Image to run: " nil 'kubed-container-image-history)))
     (list pod image namespace port attach stdin tty rm envs command args)))
  (if attach
      (pop-to-buffer
       (apply #'make-comint "kubed-run" kubed-kubectl-program nil
              "run" pod (concat "--image=" image) "--attach"
              (append
               (mapcar (lambda (env) (concat "--env=" env))
                       (cons "TERM=dumb" envs))
               (when namespace (list (concat "--namespace=" namespace)))
               (when stdin '("-i"))
               (when tty '("-t"))
               (when rm '("--rm"))
               (when port (list (format "--port=%d" port)))
               (when command '("--command"))
               (when args (cons "--" args)))))
    (unless (zerop
             (apply #'call-process
                    kubed-kubectl-program nil nil nil
                    "run" pod (concat "--image=" image)
                    (append
                     (mapcar (lambda (env) (concat "--env=" env)) envs)
                     (when namespace (list (concat "--namespace=" namespace)))
                     (when stdin '("-i"))
                     (cond
                      (attach '("--attach"))
                      (stdin  '("--attach=false")))
                     (when tty '("-t"))
                     (when rm '("--rm"))
                     (when port (list (format "--port=%d" port)))
                     (when command '("--command"))
                     (when args (cons "--" args)))))
      (user-error "Failed to run image `%s'" image))
    (message "Image `%s' is now running in pod `%s'." image pod))
  (kubed-update-pods t))

(defun kubed-pod-containers (pod &optional k8sns)
  "Return list of containers in Kubernetes pod POD in namespace K8SNS."
  (string-split
   (car (process-lines
         kubed-kubectl-program "get"
         (if k8sns (concat "--namespace=" k8sns) "--all-namespaces=false")
         "pod" pod "-o" "jsonpath={.spec.containers[*].name}"))
   " "))

(defun kubed-pod-default-container (pod &optional k8sns)
  "Return default container of Kubernetes pod POD in namespace K8SNS, or nil."
  (car (process-lines
        kubed-kubectl-program
        "get"
        (if k8sns (concat "--namespace=" k8sns) "--all-namespaces=false")
        "pod" pod "-o"
        "jsonpath={.metadata.annotations.kubectl\\.kubernetes\\.io/default-container}")))

(defun kubed-read-container (pod prompt &optional guess k8sns)
  "Prompt with PROMPT for a container in POD and return its name.

Non-nil optional argument GUESS says to try and guess which container to
use without prompting: if the pod has a
\"kubectl.kubernetes.id/default-container\" annotation, use the
container that this annotation specifes; if there's just one container,
use it; otherwise, fall back to prompting."
  (let ((default (kubed-pod-default-container pod k8sns))
        (all 'unset))
    (or
     ;; There's a default container, so that's our guess.
     (and guess default)
     ;; No default, but we're allowed to guess, so check if there's just
     ;; one container, and if so that's our guess.
     (and guess (setq all (kubed-pod-containers pod k8sns))
          (null (cdr all))
          (car all))
     ;; No guessing, prompt.
     (completing-read (format-prompt prompt default)
                      (completion-table-dynamic
                       (lambda (_)
                         (if (eq all 'unset)
                             (setq all (kubed-pod-containers pod k8sns))
                           all)))
                      nil t nil nil default))))

;;;###autoload
(defun kubed-logs (pod container &optional k8sns)
  "Show logs for container CONTAINER in Kubernetes pod POD."
  (interactive
   (if kubed-all-namespaces-mode
       (let* ((p-s (kubed-read-namespaced-pod "Show logs for pod"))
              (p (car p-s))
              (s (cadr p-s)))
         (list p (kubed-read-container p "Container" nil s) s))
     (let* ((p (kubed-read-pod "Show logs for pod"))
            (c (kubed-read-container p "Container")))
       (list p c))))
  (let ((buf (generate-new-buffer (format "*kubed-logs %s[%s] in %s*" pod container
                                          (or k8sns "current namespace")))))
    (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
    (if k8sns
        (message "Getting logs for container `%s' in pod `%s' in namespace `%s'..." container pod k8sns)
      (message "Getting logs for container `%s' in pod `%s'..." container pod))
    (start-process "*kubed-logs*" buf
                   kubed-kubectl-program "logs"
                   (if k8sns (concat "--namespace=" k8sns) "--tail=-1")
                   "-f" "-c" container pod)
    (display-buffer buf)))

(defvar kubed-port-forward-process-alist nil
  "Alist of current port-forwarding descriptors and corresponding processes.")

(defun kubed-port-forward-process-alist (&optional _ignored)
  "Update and return value of variable `kubed-port-forward-process-alist'."
  (setq kubed-port-forward-process-alist
        (seq-filter (lambda (pair)
                      (process-live-p (cdr pair)))
                    kubed-port-forward-process-alist)))

;;;###autoload
(defun kubed-forward-port-to-pod (pod local-port remote-port &optional k8sns)
  "Forward LOCAL-PORT to REMOTE-PORT of Kubernetes pod POD."
  (interactive
   (if kubed-all-namespaces-mode
       (let* ((p-s (kubed-read-namespaced-pod "Forward port to pod"))
              (p (car p-s))
              (s (cadr p-s)))
         (list p (read-number "Local port: ") (read-number "Remote port: ") s))
     (let* ((p (kubed-read-pod "Forward port to pod"))
            (l (read-number "Local port: "))
            (r (read-number "Remote port: ")))
       (list p l r))))
  (if k8sns
      (message "Forwarding local port %d to remote port %d of pod `%s'..."
               local-port remote-port pod)
    (message "Forwarding local port %d to remote port %d of pod `%s' in namespace `%s'..."
             local-port remote-port pod k8sns))
  (push
   (cons
    (format "pod %s %d:%d%s"
            pod local-port remote-port
            (if k8sns (concat " in " k8sns) ""))
    (start-process "*kubed-port-forward*" nil
                   kubed-kubectl-program "port-forward"
                   (if k8sns (concat "--namespace=" k8sns) "--address=localhost")
                   pod (format "%d:%d" local-port remote-port)))
   kubed-port-forward-process-alist))

(defun kubed-stop-port-forward (descriptor)
  "Stop Kubernetes port-forwarding with descriptor DESCRIPTOR.

DESCRIPTOR is a string that says which port-forwarding process to stop,
it has the format \"pod POD LOCAL-PORT:REMOTE-PORT\", where POD is the
name of the pod that is the target of the port-forwarding, LOCAL-PORT is
the local port that is being forwarded, and REMOTE-PORT is the
correspoding remote port of POD.

Interactively, prompt for DESCRIPTOR with completion.  If there is only
one port-forwarding process, stop that process without prompting."
  (interactive
   (list
    (cond
     ((cdr (kubed-port-forward-process-alist))
      (completing-read "Stop port-forwarding: "
                       (completion-table-dynamic
                        #'kubed-port-forward-process-alist)
                       nil t))
     ((caar kubed-port-forward-process-alist))
     (t (user-error "No port-forwarding to Kubernetes in progress")))))
  (if-let ((pair (assoc descriptor kubed-port-forward-process-alist)))
      (delete-process (cdr pair))
    (error "No port-forwarding for %s" descriptor))
  (message "Stopped port-forwarding for %s" descriptor))

(defvar kubed-container-image-history nil
  "Minibuffer history for `kubed-read-container-image'.")

(defun kubed-read-container-image (prompt &optional default multi)
  "Prompt with PROMPT for names of container images.

Optional argument DEFAULT is the minibuffer default argument.  Non-nil
optional argument MULTI says to read multiple image names and return
them as list."
  (funcall (if multi #'completing-read-multiple #'completing-read)
           (format-prompt prompt default) nil nil nil nil
           'kubed-container-images-history default))

(defvar kubed-ingress-rule-history nil
  "Minibuffer history for `kubed-read-ingress-rules'.")

(defun kubed-read-ingress-rules ()
  "Prompt with PROMPT for Kubernetes ingress rules."
  (let ((rules (list (read-string "Ingress rule: "
                                  nil 'kubed-ingress-rule-history))))
    (while (not
            (string-empty-p
             (car (push (read-string (format-prompt "Additional rules" "done")
                                     nil 'kubed-ingress-rule-history)
                        rules)))))
    (nreverse (cdr rules))))

(defvar transient-current-command)

(defun kubed-transient-args (&optional prefix)
  "Return current arguments from transient PREFIX.

If PREFIX nil, it defaults to the value of `transient-current-command'."
  (when-let ((prefix (or prefix (bound-and-true-p transient-current-command))))
    (and (featurep 'kubed-transient)
         (fboundp 'transient-args)
         (transient-args prefix))))

;;;###autoload
(defun kubed-attach (pod &optional container namespace stdin tty)
  "Attach to running process in CONTAINER in Kubernetes POD.

Optional argument NAMESPACE is the namespace in which to look for POD.
Non-nil STDIN says to connect local standard input to remote process.
Non-nil TTY says to use a TTY for standard input.

Interactively, prompt for POD; if there are multiple pod containers,
prompt for CONTAINER as well; STDIN is t unless you call this command
with \\[universal-argument] \\[universal-argument]; and TTY is t unless\
 you call this command with \\[universal-argument]."
  (interactive
   (let ((namespace nil) (stdin t) (tty t))
     (when (<= 4  (prefix-numeric-value current-prefix-arg)) (setq tty   nil))
     (when (<= 16 (prefix-numeric-value current-prefix-arg)) (setq stdin nil))
     (dolist (arg (kubed-transient-args 'kubed-transient-attach))
       (cond
        ((string-match "--namespace=\\(.+\\)" arg)
         (setq namespace (match-string 1 arg)))
        ((equal "--stdin" arg) (setq stdin t))
        ((equal "--tty" arg) (setq tty t))))
     (if (and kubed-all-namespaces-mode (not namespace))
         (let* ((p-s (kubed-read-namespaced-pod "Attach to pod"))
                (p (car p-s))
                (s (cadr p-s)))
           (list p (kubed-read-container p "Container" t s) s stdin tty))
       (let* ((p (kubed-read-pod "Attach to pod" nil nil namespace))
              (c (kubed-read-container p "Container" t namespace)))
         (list p c namespace stdin tty)))))
  (pop-to-buffer
   (apply #'make-comint "kubed-attach" kubed-kubectl-program nil
          "attach" pod
          (append
           (when namespace (list "-n" namespace))
           (when container (list "-c" container))
           (when stdin '("-i"))
           (when tty   '("-t"))))))

;;;###autoload
(defun kubed-diff (definition &optional include-managed)
  "Display difference between Kubernetes resource DEFINITION and current state.

Non-nil optional argument INCLUDE-MANAGED (interactively, the prefix
argument) says to include managed fields in the comparison."
  (interactive
   (let ((definition nil) (include-managed nil))
     (dolist (arg (when (and (fboundp 'transient-args)
                             (fboundp 'kubed-transient-diff))
                    (transient-args 'kubed-transient-diff)))
       (cond
        ((string-match "--filename=\\(.+\\)" arg)
         (setq definition (match-string 1 arg)))
        ((equal "--show-managed-fields" arg) (setq include-managed t))))
     (list (or definition (kubed-read-resource-definition-file-name))
           (or include-managed current-prefix-arg))))
  (let ((buf (get-buffer-create "*kubed-diff*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (fundamental-mode)
      (call-process kubed-kubectl-program nil t nil "diff"
                    (concat "--show-managed-fields="
                            (if include-managed "true" "false"))
                    "-f" (expand-file-name definition))
      (setq buffer-read-only t)
      (diff-mode)
      (goto-char (point-min)))
    (display-buffer buf)))

;;;###autoload
(defun kubed-exec (pod command &optional container namespace stdin tty args)
  "Execute COMMAND with ARGS in CONTAINER in Kubernetes POD.

Optional argument NAMESPACE is the namespace in which to look for POD.
Non-nil STDIN says to connect local standard input to remote process.
Non-nil TTY says to use a TTY for standard input.

Interactively, prompt for POD; if there are multiple pod containers,
prompt for CONTAINER as well; STDIN is t unless you call this command
with \\[universal-argument] \\[universal-argument]; and TTY is t unless\
 you call this command with \\[universal-argument]."
  (interactive
   (let ((namespace nil) (stdin t) (tty t) (command nil) (args nil))
     (when (<= 4  (prefix-numeric-value current-prefix-arg)) (setq tty   nil))
     (when (<= 16 (prefix-numeric-value current-prefix-arg)) (setq stdin nil))
     (dolist (arg (kubed-transient-args 'kubed-transient-exec))
       (cond
        ((string-match "--namespace=\\(.+\\)" arg)
         (setq namespace (match-string 1 arg)))
        ((equal "--stdin" arg) (setq stdin t))
        ((equal "--tty" arg) (setq tty t))
        ((string-match "-- =\\(.+\\)" arg)
         (setq args    (split-string-and-unquote (match-string 1 arg))
               command (car args)
               args    (cdr args)))))
     (if (and kubed-all-namespaces-mode (not namespace))
         (let* ((p-s (kubed-read-namespaced-pod "Execute command in pod"))
                (p (car p-s))
                (s (cadr p-s))
                (c (kubed-read-container p "Container" t s)))
           (unless command
             (setq args    (split-string-and-unquote
                            (read-string "Execute command: "))
                   command (car args)
                   args    (cdr args)))
           (list p command c s stdin tty args))
       (let* ((p (kubed-read-pod "Execute command in pod" nil nil namespace))
              (c (kubed-read-container p "Container" t namespace)))
         (unless command
           (setq args    (split-string-and-unquote
                          (read-string "Execute command: "))
                 command (car args)
                 args    (cdr args)))
         (list p command c namespace stdin tty args)))))
  (pop-to-buffer
   (apply #'make-comint "kubed-exec" kubed-kubectl-program nil
          "exec" pod
          (append
           (when namespace (list "-n" namespace))
           (when container (list "-c" container))
           (when stdin '("-i"))
           (when tty '("-t"))
           (list "--" command)
           args))))

(defvar kubed-patch-history nil
  "Minibuffer history for `kubed-read-patch'.")

(defun kubed-read-patch ()
  "Prompt for a Kubernetes resource patch in JSON or YAML format."
  (read-string "Patch (JSON or YAML): " nil 'kubed-patch-history))

;;;###autoload
(defun kubed-patch (type name patch &optional namespace strategy)
  "Patch Kubernetes resource NAME of TYPE with patch PATCH.

Optional argument NAMESPACE is the namespace in which to look for NAME.
STRATEGY is the patch type to use, one of \"json\", \"merge\" and
\"strategic\", defaulting to \"strategic\".

Interactively, prompt for TYPE, NAME and PATCH."
  (interactive
   (let ((type (kubed-read-resource-type "Resource type to patch"))
         (namespace nil) (strategy nil))
     (dolist (arg (kubed-transient-args 'kubed-transient-apply))
       (cond
        ((string-match "--namespace=\\(.+\\)" arg)
         (setq namespace (match-string 1 arg)))
        ((string-match "--type=\\(.+\\)" arg)
         (setq strategy (match-string 1 arg)))))
     (list type
           (kubed-read-resource-name type "Resource to patch")
           (kubed-read-patch) namespace strategy)))
  (message "Applying patch to `%s'..." name)
  (unless (zerop
           (apply #'call-process
                  kubed-kubectl-program nil nil nil
                  "patch" type name "-p" patch
                  (append
                   (when namespace (list "-n" namespace))
                   (when strategy (list "--type" strategy)))))
    (user-error "Patching `%s' failed" name))
  (message "Applying patch to `%s'...  Done." name))

(with-eval-after-load 'help-mode
  ;; Wait for `help-mode' to define `help-xref'.  It's always loaded by
  ;; the time we actually need it in `kubed-explain'.
  (define-button-type 'kubed-explain
    :supertype 'help-xref
    'help-function 'kubed-explain
    'help-echo "mouse-2, RET: explain"))

(defvar kubed-resource-field-history nil
  "Minibuffer history for `kubed-read-resource-field'.")

(defun kubed-api-resources ()
  "Return list of resource types in the current Kubernetes context."
  (mapcar
   (lambda (line)
     (car (split-string line)))
   (process-lines
    kubed-kubectl-program
    "api-resources" "--no-headers")))

(defun kubed-resource-names (type &optional namespace)
  "Return list of Kuberenetes resource names of type TYPE in NAMESPACE."
  (apply #'process-lines
         kubed-kubectl-program "get" type
         "-o" "custom-columns=NAME:.metadata.name" "--no-headers"
         (when namespace (list "-n" namespace))))

(defun kubed-read-resource-name (type prompt &optional default namespace)
  "Prompt with PROMPT for Kubernetes resource name of type TYPE.

Optional argument DEFAULT is the minibuffer default argument.  Non-nil
optional argument NAMESPACE says to use names from NAMESPACE as
completion candidates instead of the current namespace."
  (completing-read
   (format-prompt prompt default)
   (kubed-resource-names type namespace)
   nil 'confirm nil nil default))

(defun kubed-read-resource-type (prompt &optional default)
  "Prompt with PROMPT for Kubernetes resource type.

Optional argument DEFAULT is the minibuffer default argument."
  (completing-read
   (format-prompt prompt default)
   (kubed-api-resources)
   nil 'confirm nil nil default))

(defun kubed-read-resource-field (prompt &optional default)
  "Prompt with PROMPT for Kubernetes resource type or field name.

Optional argument DEFAULT is the minibuffer default argument."
  (completing-read
   (format-prompt prompt default)
   (lambda (s p a)
     (unless (eq a 'metadata)
       (let ((start 0))
         (while (string-match "\\." s start)
           (setq start (match-end 0)))
         (if (eq (car-safe a) 'boundaries)
             `(boundaries ,start . ,(and (string-match "\\." (cdr a))
                                         (match-beginning 0)))
           (let ((table
                  (if (zerop start)
                      ;; Complete resource type.
                      (kubed-api-resources)
                    ;; Complete (sub-)field name.
                    (with-temp-buffer
                      (call-process
                       kubed-kubectl-program nil t nil
                       "explain" (substring s 0 start))
                      (goto-char (point-min))
                      (let ((res nil))
                        (while (re-search-forward
                                (rx line-start (+ " ") (group-n 1 (* alnum)) "\t")
                                nil t)
                          (push (match-string 1) res))
                        res)))))
             (if a (complete-with-action a table (substring s start) p)
               ;; `try-completion'.
               (let ((comp (complete-with-action a table (substring s start) p)))
                 (if (stringp comp) (concat (substring s 0 start) comp) comp))))))))
   nil 'confirm nil 'kubed-resource-field-history default))

;;;###autoload
(defun kubed-explain (field)
  "Show help buffer with explanation about Kubernetes resource FIELD."
  (interactive
   (list (kubed-read-resource-field "Explain type or field")))
  (let ((help-buffer-under-preparation t))
    (help-setup-xref (list #'kubed-explain field)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (insert (substitute-quotes
                 (concat "`kubed explain " field "' says:\n\n")))
        (save-excursion
          ;; Add button that goes to parent.
          (goto-char (point-min))
          (when (re-search-forward (rx " "
                                       (group-n 1 (* graph))
                                       "."
                                       (+ (not (any ?.)))
                                       line-end)
                                   nil t)
            (help-xref-button 1 'kubed-explain (match-string 1))))

        (call-process kubed-kubectl-program nil t nil "explain" field)
        ;; Buttonize references to other fields.
        (goto-char (point-min))
        (while (re-search-forward (rx line-start
                                      (+ " ")
                                      (group-n 1 (* alnum))
                                      "\t")
                                  nil t)
          (help-xref-button 1 'kubed-explain
                            (concat field "." (match-string 1))))))))

(defvar kubed-kubectl-command-history nil
  "Minibuffer history for `kubed-kubectl-command'.")

(declare-function cobra-read-command-line "cobra"
                  (prompt initial &optional hist))

;;;###autoload
(defun kubed-kubectl-command (command)
  "Execute `kubectl' COMMAND.

This function calls `shell-command' (which see) to do the work.

Interactively, prompt for COMMAND with completion for `kubectl' arguments."
  (interactive
   (list (cobra-read-command-line
          "Command: "
          (concat
           kubed-kubectl-program " "
           (let* ((args (kubed-transient-args))
                  (prefix (and (fboundp 'transient-prefix-object)
                               (transient-prefix-object)))
                  (scope (and prefix (fboundp 'eieio-oref)
                              (eieio-oref prefix 'scope))))
             (when (or args scope)
               (concat (string-join (append scope args) " ") " "))))
          'kubed-kubectl-command-history)))
  (shell-command command))

;;;###autoload (autoload 'kubed-prefix-map "kubed" nil t 'keymap)
(defvar-keymap kubed-prefix-map
  :doc "Prefix keymap for Kubed commands."
  :prefix 'kubed-prefix-map
  "p" 'kubed-pod-prefix-map
  "N" 'kubed-namespace-prefix-map
  "s" 'kubed-service-prefix-map
  "S" 'kubed-secret-prefix-map
  "j" 'kubed-job-prefix-map
  "d" 'kubed-deployment-prefix-map
  "i" 'kubed-ingress-prefix-map
  "c" 'kubed-cronjob-prefix-map
  "C" #'kubed-use-context
  "U" #'kubed-update-all
  "A" #'kubed-all-namespaces-mode
  "+" #'kubed-create
  "*" #'kubed-apply
  "R" #'kubed-run
  "=" #'kubed-diff
  "E" #'kubed-explain
  "P" #'kubed-patch
  "!" #'kubed-kubectl-command)

(defvar-keymap kubed-menu-map
  :doc "Keymap with Kubed menu entries."
  :prefix 'kubed-menu-map
  "<namespace>"        '("Namespaces..."         . kubed-namespace-menu-map)
  "<pod>"              '("Pods..."               . kubed-pod-menu-map)
  "<persistentvolume>" '("Persistent Volumes..." . kubed-persistentvolume-menu-map)
  "<service>"          '("Services..."           . kubed-service-menu-map)
  "<secret>"           '("Secrets..."            . kubed-secret-menu-map)
  "<job>"              '("Jobs..."               . kubed-job-menu-map)
  "<deployment>"       '("Deployments..."        . kubed-deployment-menu-map)
  "<replicaset>"       '("Replica Sets..."       . kubed-replicaset-menu-map)
  "<statefulset>"      '("Stateful Sets..."      . kubed-statefulset-menu-map)
  "<cronjob>"          '("Cron Jobs..."          . kubed-cronjob-menu-map)
  "<ingressclass>"     '("Ingress Classes..."    . kubed-ingressclass-menu-map)
  "<ingress>"          '("Ingresses..."          . kubed-ingress-menu-map)
  "<patch>"            '("Patch Resource"        . kubed-patch)
  "<diff>"             '("Diff Config with Live" . kubed-diff)
  "<kubectl-command>"  '("Invoke kubectl"        . kubed-kubectl-command)
  "<all-namespaces>"   '("Toggle Namespacing"    . kubed-all-namespaces-mode)
  "<update-all>"       '("Update Resource Lists" . kubed-update-all)
  "<explain>"          '("Explain Type or Field" . kubed-explain)
  "<use-context>"      '("Set Current Context"   . kubed-use-context)
  "<run>"              '("Run Image"             . kubed-run)
  "<apply>"            '("Apply Config"          . kubed-apply)
  "<create>"           '("Create Resource"       . kubed-create))

;;;###autoload
(define-minor-mode kubed-menu-bar-mode
  "Add \"Kubernetes\" menu to your menu bar."
  :global t
  (if kubed-menu-bar-mode
      (keymap-set-after (current-global-map)
        "<menu-bar> <kubed>" '("Kubernetes" . kubed-menu-map))
    (keymap-global-unset "<menu-bar> <kubed>")))

(defvar reporter-prompt-for-summary-p)

(defun kubed-submit-bug-report ()
  "Report a Kubed to the maintainers via mail."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     "Kubed Development <~eshel/kubed-devel@lists.sr.ht>"
     (format "Kubed")
     '(kubed-kubectl-program)
     nil nil
     (propertize " "
                 'display
                 (propertize
                  "Insert your bug report below.
If possible, specify where you got Emacs, kubectl and Kubed,
and include a recipe for reproducing your issue.
[This line and the above text are not included in your report.]"
                  'face 'italic)))))

(provide 'kubed)
;;; kubed.el ends here
