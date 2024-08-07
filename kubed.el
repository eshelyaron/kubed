;;; kubed.el --- Kubernetes, Emacs, done!   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Maintainer: Eshel Yaron <~eshel/kubed-devel@lists.sr.ht>
;; Keywords: tools kubernetes containers
;; URL: https://eshelyaron.com/kubed.html
;; Package-Version: 0.3.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This library defines commands for interacting with Kubernetes
;; resources, such as Kuberenetes pods, services, deployments, and more.
;;
;; Use `kubed-display-pod' to display a Kuberenetes pod,
;; `kubed-edit-pod' to edit it, `kubed-delete-pods' to delete it, and
;; `kubed-list-pods' to see a menu of all pods.  You can create new pods
;; from YAML or JSON files with `kubed-create-pod'.
;;
;; Similar commands are defined for other types of resources as well.
;;
;; This library interacts with Kuberenetes via `kubectl', and uses the
;; current `kubectl' context and namespace by default.  To change your
;; current context or namespace, use commands `kubed-use-context' and
;; `kubed-set-namespace' respectively; you can also interact with
;; resources in other namespaces without changing your default
;; namespace, for example you can call `kubed-list-pods' with a prefix
;; argument to choose another namespace for listing pods.  The prefix
;; keymap `kubed-prefix-map' gives you quick access to these and other
;; useful commands, you may want to bind it globally to a convenient key
;; with `keymap-global-set':
;;
;;   (keymap-global-set "C-c k" 'kubed-prefix-map)
;;
;; In addition, the command `kubed-transient' lets you explore various
;; Kubernetes operations with a transient menu interface.  You may also
;; want to enable `kubed-menu-bar-mode', which add a "Kubernetes" menu
;; to your menu-bar with many useful entries.
;;
;; If you want to work with more or different types of Kubernetes
;; resources, use the macro `kubed-define-resource'.  This macro defines
;; some common functions and commands that'll get you started with ease.
;;
;; For more information, see the Kubed manual at (info "(kubed)Top"), or
;; online at https://eshelyaron.com/kubed.html

;;; Code:

(defgroup kubed nil
  "Kubernetes interface."
  :group 'tools)

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

(defcustom kubed-name-column '("Name" 48 t)
  "Specification of resource name column in Kubernetes resource list buffers."
  :type '(list string natnum boolean))

(defvar kubed--data nil)

(defun kubed--alist (type context namespace)
  "Return information about resources in CONTEXT of type TYPE in NAMESPACE."
  (alist-get namespace
             (alist-get type
                        (alist-get context
                                   kubed--data nil nil #'string=)
                        nil nil #'string=)
             nil nil #'equal))

(gv-define-setter kubed--alist (value type context namespace)
  `(setf (alist-get ,namespace
                    (alist-get ,type
                               (alist-get ,context
                                          kubed--data nil nil #'string=)
                               nil nil #'string=)
                    nil nil #'equal)
         ,value))

(defvar kubed--columns nil)

(defun kubed-update (type context &optional namespace)
  "Update list of resources of type TYPE in CONTEXT and NAMESPACE."
  (when (process-live-p (alist-get 'process (kubed--alist type context namespace)))
    (user-error "Update in progress"))
  (let* ((out (get-buffer-create (format " *kubed-get-%s*"        type)))
         (err (get-buffer-create (format " *kubed-get-%s-stderr*" type)))
         (columns (alist-get type kubed--columns nil nil #'string=)))
    (with-current-buffer out (erase-buffer))
    (setf (alist-get 'process (kubed--alist type context namespace))
          (make-process
           :name (format "*kubed-get-%s*" type)
           :buffer out
           :stderr err
           :command (append
                     (list kubed-kubectl-program "get" type
                           "--context" context
                           (format "--output=custom-columns=%s"
                                   (mapconcat #'car columns ",")))
                     (when namespace (list "--namespace" namespace)))
           :sentinel
           (lambda (_proc status)
             (cond
              ((string= status "finished\n")
               (let (new offsets eol)
                 (with-current-buffer out
                   (goto-char (point-min))
                   (setq eol (pos-eol))
                   (while (re-search-forward "[^ ]+" eol t)
                     (push (1- (match-beginning 0)) offsets))
                   (setq offsets (nreverse offsets))
                   (forward-char 1)
                   (while (not (eobp))
                     (let ((cols nil)
                           (beg (car offsets))
                           (ends (append (cdr offsets)
                                         (list (- (pos-eol) (point))))))
                       (dolist (column columns)
                         (let ((str (string-trim (buffer-substring
                                                  (+ (point) beg)
                                                  (+ (point) (car ends))))))
                           (push (if-let ((f (cdr column))) (funcall f str) str)
                                 cols)
                           (setq beg (pop ends))))
                       (push (nreverse cols) new))
                     (forward-line 1)))
                 (setf (kubed--alist type context namespace)
                       (list (cons 'resources
                                   (mapcar (lambda (c) (list (car c) (apply #'vector c)))
                                           new))))
                 (let ((bufs nil))
                   (dolist (buf (buffer-list))
                     (and (equal (buffer-local-value 'kubed-list-type buf) type)
                          (equal (buffer-local-value 'kubed-list-context buf) context)
                          (equal (buffer-local-value 'kubed-list-namespace buf) namespace)
                          (with-current-buffer buf
                            (when (derived-mode-p 'kubed-list-mode)
                              (revert-buffer)
                              (when-let ((win (get-buffer-window)))
                                (set-window-point win (point))
                                (push buf bufs))))))
                   (walk-windows
                    (lambda (win)
                      (let ((buf (window-buffer win)))
                        (when (memq buf bufs)
                          (set-window-point
                           win (with-current-buffer buf (point))))))))
                 (message (format "Updated Kubernetes %S." type))))
              ((string= status "exited abnormally with code 1\n")
               (with-current-buffer err
                 (goto-char (point-max))
                 (insert "\n" status))
               (display-buffer err))))))))

(defvar-local kubed-display-resource-info nil
  "Information about Kubernetes resource that current buffer displays.

The value is a list (TYPE NAME CONTEXT NAMESPACE), where TYPE is the
type of the resource, NAME is the name of the resource, CONTEXT is the
`kubectl' context to use for accessing the resource, and NAMESPACE is
the namespace of the resource, or nil if TYPE is not namespaced.")

(put 'kubed-display-resource-info 'permanent-local t)

(defun kubed-display-resource-revert (&optional _ _)
  "Clear and populate current Kuberenetes resource buffer."
  (seq-let (type name context namespace)
      kubed-display-resource-info
    (let ((inhibit-read-only t)
          (target (current-buffer)))
      (buffer-disable-undo)
      (with-temp-buffer
        (unless (zerop
                 (apply
                  #'call-process
                  kubed-kubectl-program nil t nil "get"
                  type "--output=yaml" name
                  (append (when namespace (list "-n" namespace))
                          (when context (list "--context" context)))))
          (error "Failed to display Kubernetes resource `%s'" name))
        (let ((source (current-buffer)))
          (with-current-buffer target
            (replace-buffer-contents source)
            (set-buffer-modified-p nil)
            (buffer-enable-undo)))))))

(defun kubed-display-resource-in-buffer
    (buffer type resource &optional context namespace)
  "Display Kubernetes RESOURCE of type TYPE in BUFFER."
  (let ((info (list type resource context namespace)))
    (with-current-buffer (get-buffer-create buffer)
      (setq-local kubed-display-resource-info info)
      (kubed-display-resource-revert)
      (goto-char (point-min))
      (run-hooks 'kubed-yaml-setup-hook)
      (kubed-display-resource-mode)
      (current-buffer))))

(defun kubed-display-resource-short-description
    (type resource context namespace)
  "Return short string to use as a label for RESOURCE of type TYPE."
  (concat type "/" resource
          (when namespace (concat "@" namespace))
          (when context   (concat "[" context "]"))))

(defun kubed-namespaced-p (type &optional context)
  "Return non-nil if TYPE is a namespaced resource type in context CONTEXT."
  (member type (kubed-api-resources context t)))

;;;###autoload
(defun kubed-display-resource
    (type resource context &optional namespace)
  "Display Kubernetes RESOURCE of type TYPE in BUFFER."
  (interactive
   (let* ((type (kubed-read-resource-type "Resource type to display"))
          (namespace
           (when (kubed-namespaced-p type)
             (or (seq-some
                  (lambda (arg)
                    (when (string-match "--namespace=\\(.+\\)" arg)
                      (match-string 1 arg)))
                  (kubed-transient-args 'kubed-transient-display))
                 (let ((cur (kubed-current-namespace)))
                   (if current-prefix-arg
                       (kubed-read-namespace "Namespace" cur)
                     cur))))))
     (list type (kubed-read-resource-name type "Display" nil namespace)
           (kubed-current-context) namespace)))
  (display-buffer
   (kubed-display-resource-in-buffer
    (concat "*Kubed "
            (kubed-display-resource-short-description
             type resource context namespace)
            "*")
    type resource context namespace)))

(declare-function bookmark-prop-get                 "bookmark")
(declare-function bookmark-get-front-context-string "bookmark")
(declare-function bookmark-get-rear-context-string  "bookmark")
(declare-function bookmark-make-record-default      "bookmark")

;;;###autoload
(defun kubed-display-resource-handle-bookmark (bookmark)
  "Display Kubernetes resource according to BOOKMARK."
  (require 'bookmark)
  (seq-let (type name context namespace)
      (bookmark-prop-get bookmark 'resource)
    (set-buffer
     (kubed-display-resource-in-buffer
      (concat "*Kubed "
              (kubed-display-resource-short-description
               type name context namespace)
              "*")
      type name context namespace))
    (when-let ((str (bookmark-get-front-context-string bookmark))
               ((search-forward str (point-max) t)))
      (goto-char (match-beginning 0)))
    (when-let ((str (bookmark-get-rear-context-string bookmark))
               ((search-backward str (point-min) t)))
      (goto-char (match-end 0)))))

(put 'kubed-display-resource-handle-bookmark 'bookmark-handler-type "KubedResource")

(defun kubed-display-resource-make-bookmark ()
  "Return bookmark pointing to currently displayed Kubernetes resource."
  (require 'bookmark)
  (seq-let (type name context namespace) kubed-display-resource-info
    (cons
     (kubed-display-resource-short-description type name context namespace)
     (append
      (list
       (cons 'handler #'kubed-display-resource-handle-bookmark)
       (cons 'resource kubed-display-resource-info))
      (bookmark-make-record-default t)))))

(defun kubed-display-resource-jump-to-list ()
  "Jump to line in resources list that corresponds to the displayed resource."
  (interactive)
  (seq-let (type name context namespace) kubed-display-resource-info
    (apply (intern (concat "kubed-list-" type)) context
           (when namespace (list namespace)))
    (kubed-list-go-to-line name)))

(defun kubed-display-resource-diff ()
  "Show diff of current buffer with current state of the displayed resource."
  (interactive)
  (kubed-diff (current-buffer) nil (nth 2 kubed-display-resource-info)))

(defun kubed-display-resource-replace ()
  "Replace displayed Kubernetes resource with current buffer contents."
  (interactive)
  (let (choice)
    (while (= ?d (setq choice
                       (car (read-multiple-choice
                             "Replace resource with changes in current buffer?"
                             '((?y "yes") (?n "no") (?d "diff"))))))
      (kubed-display-resource-diff))
    (when (= ?y choice)
      (let ((err-buf (get-buffer-create " *kubed-replace*")))
        (with-current-buffer err-buf (erase-buffer))
        (unless (zerop (call-process-region nil nil kubed-kubectl-program
                                            nil err-buf nil "replace" "-f" "-"))
          (display-buffer err-buf)
          (user-error "`kubectl replace' failed!")))
      (message "Replaced Kubernetes resource with changes in current buffer.")
      (revert-buffer))))

(defun kubed-display-resource-p (_symbol buffer)
  "Return non-nil if `kubed-display-resource-mode' is enabled in BUFFER.

The first argument, SYMBOL, is ignored.  You can use this function as
the `completion-predicate' property of commands that you define that
should only be available in buffers that display Kuberenetes resources."
  (buffer-local-value 'kubed-display-resource-mode buffer))

(dolist (cmd '(kubed-display-resource-jump-to-list
               'kubed-display-resource-diff
               'kubed-display-resource-replace))
  (put cmd 'completion-predicate #'kubed-display-resource-p))

(defvar-keymap kubed-display-resource-mode-map
  :doc "Keymap buffers that display Kubernetes resource."
  "C-c C-j" #'kubed-display-resource-jump-to-list
  "C-c C-=" #'kubed-display-resource-diff
  "C-c C-c" #'kubed-display-resource-replace)

(define-minor-mode kubed-display-resource-mode
  "Minor mode for buffers that display a Kuberenetes resource."
  :interactive nil
  :lighter " Kubed"
  (when kubed-display-resource-mode
    (setq-local
     revert-buffer-function        #'kubed-display-resource-revert
     bookmark-make-record-function #'kubed-display-resource-make-bookmark)))

(defun kubed-list-filter-lt-operator (v s)
  "Return non-nil if S is less than V as a number or as a string."
  (let ((l (string-to-number s)) (r (string-to-number v)))
    (if (= l r) (string< s v) (< l r))))

(defun kubed-list-filter-gt-operator (v s)
  "Return non-nil if S is greater than V as a number or as a string."
  (let ((l (string-to-number s)) (r (string-to-number v)))
    (if (= l r) (string> s v) (> l r))))

(defcustom kubed-list-filter-operator-alist
  '((= . string=)
    (~ . string-match-p)
    (< . kubed-list-filter-lt-operator)
    (> . kubed-list-filter-gt-operator))
  "Association list of filter operators and functions that implement them.

Elements of the list are cons cells (OP . FN), where OP is a symbol that
is used as a filter operator and FN is a function that implements OP.
FN takes two arguments, a string STR and a parameter VAL.  FN should
return non-nil if STR and VAL are related according to OP: to determine
if a line in which column COL is STR satisfies the filter (OP COL VAL),
Kubed checks if the form (FN VAL STR) evaluates to non-nil."
  :type '(alist :key-type (symbol :tag "Operator") :value-type function))

(defun kubed-list-interpret-atomic-filter (atom)
  "Return function that implements atomic filter ATOM."
  (if (eq (car-safe atom) 'quote)
      (let ((p (kubed-list-interpret-atomic-filter (cadr atom))))
        (lambda (x) (not (funcall p x))))
    (let* ((column-number (tabulated-list--column-number (symbol-name (nth 1 atom))))
           (value (nth 2 atom))
           (value (if (stringp value) value (prin1-to-string value)))
           (op (alist-get (car atom) kubed-list-filter-operator-alist)))
      (unless op (user-error "Unknown filter operator `%S'" (car atom)))
      (lambda (x) (funcall op value (aref (cadr x) column-number))))))

(defvar-local kubed-list-filter nil "Filter in effect in the current buffer.")

(defun kubed-list-interpret-filter (&optional filter)
  "Return function that implements FILTER.

If FILTER is omitted or nil, it defaults to `kubed-list-filter'."
  (let ((conjunction (or filter kubed-list-filter)))
    (if (listp (car conjunction))
        (let ((conjuncts
               (mapcar (lambda (disjunction)
                         (if (listp (car disjunction))
                             (let ((disjuncts
                                    (mapcar #'kubed-list-interpret-atomic-filter
                                            disjunction)))
                               (lambda (entry)
                                 (catch 'keep-it
                                   (dolist (pred disjuncts)
                                     (when (funcall pred entry)
                                       (throw 'keep-it t)))
                                   nil)))
                           ;; Single atomic disjunct.
                           (kubed-list-interpret-atomic-filter disjunction)))
                       (or conjunction kubed-list-filter))))
          (lambda (entry)
            (catch 'keep-it
              (dolist (pred conjuncts)
                (unless (funcall pred entry)
                  (throw 'keep-it nil)))
              t)))
      ;; Single atomic conjunct.
      (kubed-list-interpret-atomic-filter conjunction))))

(defun kubed-list-validate-atomic-filter (atom)
  "Return string explaining why ATOM is invalid, or nil if it is valid."
  (unless (consp atom)
    (throw 'validation-error
           (format (substitute-quotes
                    "Invalid atomic filter `%S', must be a list")
                   atom)))
  (if (eq (car-safe atom) 'quote)
      (kubed-list-validate-atomic-filter (cadr atom))
    (unless (length= atom 3)
      (throw 'validation-error
             (format (substitute-quotes
                      "Invalid atomic filter `%S', must have three elements")
                     atom)))
    (unless (assq (car atom) kubed-list-filter-operator-alist)
      (throw 'validation-error
             (format (substitute-quotes
                      "No operator `%S' in `kubed-list-filter-operator-alist'")
                     (car atom))))
    (unless (ignore-errors
              (tabulated-list--column-number (symbol-name (nth 1 atom))))
      (throw 'validation-error (format (substitute-quotes
                                        "Invalid column name `%S'")
                                       (nth 1 atom))))))

(defun kubed-list-validate-filter (filter)
  "Return string explaining why FILTER is invalid, or nil if it is valid."
  (catch 'validation-error
    (if (listp (car-safe filter))
        (dolist (disjunction filter)
          (if (and (consp disjunction) (listp (car disjunction)))
              (dolist (disjunct disjunction)
                (kubed-list-validate-atomic-filter disjunct))
            (kubed-list-validate-atomic-filter disjunction)))
      (kubed-list-validate-atomic-filter filter))))

(defvar-local kubed-list-filter-history-variable nil
  "History list variable to use for filter history in the current buffer.")

(defvar-local kubed--list-read-filter-target-buffer nil
  "Resource list buffer for which this minibuffer is reading a filter.")

(defvar-local kubed-list-type nil)
(defvar-local kubed-list-context nil)
(defvar-local kubed-list-namespace nil)

(defun kubed-list-go-to-line (id)
  "Go to beginning of table line with ID."
  (let ((pos nil))
    (save-excursion
      ;; Wait for refresh to finish, if currently underway.
      (while (process-live-p
              (alist-get 'process (kubed--alist kubed-list-type
                                                kubed-list-context
                                                kubed-list-namespace)))
        (accept-process-output
         (alist-get 'process (kubed--alist kubed-list-type
                                           kubed-list-context
                                           kubed-list-namespace))
         1))
      (goto-char (point-min))
      (while (not (or pos (eobp)))
        (if (equal id (tabulated-list-get-id))
            (setq pos (point))
          (forward-line))))
    (goto-char pos)))

(defun kubed-list-try-read-filter ()
  "Try to read a resource list filter in the minibuffer.

Exit the minibuffer if successful, else report the error and move point
to the location of the error.  If point is not already at the location
of the error, push a mark before moving point."
  (interactive "" minibuffer-mode)
  (let* ((prompt-end (minibuffer-prompt-end))
         (contents (minibuffer-contents))
         (error-point nil) (error-message nil) (form nil) (inval nil))
    (with-temp-buffer
      (condition-case err
          (progn
            ;; FIXME: There is a small edge case here that could get
            ;; better treatment: when `contents' ends with " ?", it
            ;; espaces the terminating closing parenthesis and leads us
            ;; to incorrectly report the input as incomplete.
            (insert "(" contents ")")
            (goto-char (point-min))
            (setq form (read (current-buffer))))
        (error (setq error-point (+ prompt-end (1- (point)))
                     error-message err))))
    (cond
     (error-point
      (unless (= (point) error-point) (push-mark))
      (goto-char error-point)
      (minibuffer-message (error-message-string error-message)))
     ((setq inval (with-current-buffer kubed--list-read-filter-target-buffer
                    (kubed-list-validate-filter form)))
      (minibuffer-message inval))
     (t (exit-minibuffer)))))

(defvar-keymap kubed-list-read-filter-map
  :parent minibuffer-local-map
  "TAB" #'completion-at-point
  "M-?" #'completion-help-at-point
  "<remap> <exit-minibuffer>" #'kubed-list-try-read-filter)

(defun kubed-list-read-filter (prompt)
  "Prompt with PROMPT for a filter for the current buffer."
  (let* ((buf (current-buffer))
         (cols (seq-map #'car tabulated-list-format))
         (vals (let ((tmp nil))
                 (dolist (ent (alist-get 'resources
                                         (kubed--alist kubed-list-type
                                                       kubed-list-context
                                                       kubed-list-namespace)))
                   (let ((i 0))
                     (dolist (col cols)
                       (push (aref (cadr ent) i)
                             (alist-get col tmp nil nil #'string=))
                       (setq i (1+ i)))))
                 (mapcar #'delete-dups tmp)))
         (filter
          (minibuffer-with-setup-hook
              (lambda ()
                (set-syntax-table emacs-lisp-mode-syntax-table)
                (setq-local kubed--list-read-filter-target-buffer buf)
                (add-hook
                 'completion-at-point-functions
                 (lambda ()
                   (let ((cont (buffer-substring
                                (minibuffer-prompt-end) (point)))
                         (bounds (or (bounds-of-thing-at-point 'symbol)
                                     (cons (point) (point)))))
                     (with-temp-buffer
                       (set-syntax-table emacs-lisp-mode-syntax-table)
                       (insert "(" cont)
                       (when-let ((fn-argi (elisp--fnsym-in-current-sexp))
                                  (argi (cadr fn-argi)))
                         (if (= argi 0)
                             ;; Complete operators.
                             (list
                              (car bounds) (cdr bounds)
                              (mapcar #'car kubed-list-filter-operator-alist))
                           (when (car fn-argi)
                             (cond
                              ((= argi 1)
                               ;; Complete column names.
                               (list (car bounds) (cdr bounds) cols))
                              ((= argi 2)
                               ;; Complete column values.
                               (when-let ((beg (nth 1 (syntax-ppss)))
                                          ;; Grab preceding symbol.
                                          (col (save-excursion
                                                 (goto-char beg)
                                                 (forward-char 1)
                                                 (forward-sexp 2)
                                                 (thing-at-point 'symbol))))
                                 (list (car bounds) (cdr bounds)
                                       (alist-get col vals
                                                  nil nil #'string=)))))))))))
                 nil t))
            (read-from-minibuffer
             (format-prompt prompt "disable")
             (mapconcat #'prin1-to-string kubed-list-filter " ")
             kubed-list-read-filter-map nil
             kubed-list-filter-history-variable ""))))
    (car (ignore-errors (read-from-string (format "(%s)" filter))))))

(defun kubed-list-set-filter (filter)
  "Set the filter of the current buffer to FILTER.

FILTER determines which resources to keep.  FILTER can be an atomic
filter, which is a list (OP COL VAL), where OP is an operator defined in
`kubed-list-filter-operator-alist' (which see), COL is a symbol whose
name is a column name, and VAL is a string or an object whose printed
representation is compared to the value of the column COL according to
OP.  For example, the atomic filter (= Name foobar) keeps only resources
whose name is \"foobar\".  (= Name \"foobar\") does exactly the same.
To negate an atomic filter, quote it.  E.g. use \\='(~ Namespace kube)
to hide all resources in namespaces whose name contains \"kube\".

FILTER can also be a list of sub-filters (SUB1 SUB2 ...) where each
sub-filter is either an atomic filter or a list of atomic filters.  If a
sub-filter is a list of atomic filters, then that sub-filter denotes the
disjunction of those atomic filters.  FILTER denotes the conjunction of
the sub-filters.  In particular, FILTER nil denotes the empty
conjunction which is always true (keeps all resources).

More examples:

- `((= Name foobar) (~ Namespace kube))': keep only resources named
  \"foobar\" in namespaces that contain \"kube\".
- `(((= Name foobar) (~ Namespace kube)))': keep resources that are
  either named \"foobar\" or in a namespace that contains \"kube\".
- `(((= Name foobar) (~ Namespace kube)) \\='(~ Starttime 2024-07))':
  keep only resources that are either named \"foobar\" or in a namespace
  that contains \"kube\", and were not started during July 2024.

Interactively, prompt for FILTER sans the outermost set of parenthesis.
For example, enter \"= Name foobar\" in the minibuffer to specify the
atomic FILTER (= Name foobar).

See also Info node \"(kubed) List Filter\"."
  (interactive (list (kubed-list-read-filter "Set filter")) kubed-list-mode)
  (when-let ((validation-error (kubed-list-validate-filter filter)))
    (user-error validation-error))
  (setq-local kubed-list-filter filter)
  (revert-buffer))

(defun kubed-list-mark-for-deletion ()
  "Mark Kubernetes resource at point for deletion."
  (interactive "" kubed-list-mode)
  (tabulated-list-put-tag (propertize "D" 'help-echo "Marked for deletion") t))

(defun kubed-list-unmark ()
  "Remove mark from Kubernetes resource at point."
  (interactive "" kubed-list-mode)
  (tabulated-list-put-tag " " t))

(defun kubed-list-previous-column (&optional n)
  "Move backward N columns.

Interactively, N is the numeric value of the prefix argument, defaulting
to 1."
  (interactive "p" kubed-list-mode)
  (kubed-list-next-column (- n)))

(defun kubed-list-next-column (&optional n)
  "Move forward N columns.

Interactively, N is the numeric value of the prefix argument, defaulting
to 1."
  (interactive "p" kubed-list-mode)
  (let ((next (point))
        (times (abs (or n 1)))
        (dir-fn (if (< 0 n)
                    #'next-single-property-change
                  #'previous-single-property-change)))
    (dotimes (_ times)
      (setq next (funcall dir-fn next 'tabulated-list-column-name))
      (when (= (char-after next) ?\n)
        ;; At line boundary, go to first/last column of next line.
        (setq next (funcall dir-fn next 'tabulated-list-column-name)))
      (unless next (user-error "End of table")))
    (goto-char next)))

(defun kubed-list-copy-as-kill (click)
  "Copy name of Kubernetes resource at CLICK into the kill ring."
  (interactive (list last-nonmenu-event) kubed-list-mode)
  (if-let ((ent (tabulated-list-get-entry (mouse-set-point click)))
           (new (aref ent 0)))
      (progn
        (kill-new new)
        (message "Copied resource name `%s'" new))
    (user-error "No Kubernetes resource at point")))

(defun kubed-list-context-menu (menu click)
  "Extend MENU with common actions on Kubernetes resource at CLICK."
  (when (tabulated-list-get-entry (posn-point (event-start click)))
    (define-key menu [kubed-list-select-resource]
                '(menu-item "Select" kubed-list-select-resource))
    (define-key menu [kubed-list-display-resource]
                '(menu-item "Display" kubed-list-display-resource))
    (define-key menu [kubed-list-delete]
                '(menu-item "Delete" kubed-list-delete))
    (define-key menu [kubed-list-patch]
                '(menu-item "Patch" kubed-list-patch))
    (define-key menu [kubed-list-edit]
                '(menu-item "Edit" kubed-list-edit))
    (define-key menu [kubed-list-kubectl-command]
                '(menu-item "Execute `kubectl' command" kubed-list-kubectl-command))
    (define-key menu [kubed-list-copy-as-kill]
                '(menu-item "Copy name" kubed-list-copy-as-kill)))
  menu)

(defun kubed-list-update (&optional quiet)
  "Update list of Kubernetes resources.

If optional argument QUIET is non-nil, do not emit a message when
starting to update.  Display a message when the update is done
regardless of QUIET."
  (interactive "" kubed-list-mode)
  (kubed-update kubed-list-type kubed-list-context kubed-list-namespace)
  (force-mode-line-update)
  (unless quiet (minibuffer-message (format "Updating Kubernetes %S..." kubed-list-type))))

(defun kubed-list-delete-marked ()
  "Delete marked Kubernetes resources."
  (interactive "" kubed-list-mode)
  (let (delete-list)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?D)
          (push (tabulated-list-get-id) delete-list))
        (forward-line)))
    (if delete-list
        (when (y-or-n-p (format "Delete %d marked Kubernetes resources?"
                                (length delete-list)))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (member (tabulated-list-get-id) delete-list)
                (tabulated-list-put-tag
                 (propertize "K" 'help-echo "Deletion in progress")))
              (forward-line)))
          (let ((errb (generate-new-buffer " *kubed-list-delete-marked-stderr*")))
            (make-process
             :name "*kubed-list-delete-marked*"
             :stderr errb
             :command (append
                       (list kubed-kubectl-program "delete" kubed-list-type)
                       delete-list)
             :sentinel (lambda (_proc status)
                         (cond
                          ((string= status "finished\n")
                           (message (format "Deleted %d marked Kubernetes resources."
                                            (length delete-list)))
                           (kubed-list-update t))
                          ((string= status "exited abnormally with code 1\n")
                           (with-current-buffer errb
                             (goto-char (point-max))
                             (insert "\n" status))
                           (display-buffer errb)))))))
      (user-error "No Kubernetes resources marked for deletion"))))

(defun kubed-list-display-resource (click)
  "Display Kubernetes resource at CLICK in another window."
  (interactive (list last-nonmenu-event) kubed-list-mode)
  (if-let ((resource (tabulated-list-get-id (mouse-set-point click))))
      (kubed-display-resource
       kubed-list-type resource kubed-list-context kubed-list-namespace)
    (user-error "No Kubernetes resource at point")))

(defun kubed-list-select-resource (click)
  "Display Kubernetes resource at CLICK in current window."
  (interactive (list last-nonmenu-event) kubed-list-mode)
  (if-let ((resource (tabulated-list-get-id (mouse-set-point click))))
      (switch-to-buffer
       (kubed-display-resource-in-buffer
        (concat "*Kubed "
                (kubed-display-resource-short-description
                 kubed-list-type resource kubed-list-context kubed-list-namespace)
                "*")
        kubed-list-type resource kubed-list-context kubed-list-namespace))
    (user-error "No Kubernetes resource at point")))

(defun kubed-list-select-resource-other-window (click)
  "Display Kubernetes resource at CLICK in other window and select that window."
  (interactive (list last-nonmenu-event) kubed-list-mode)
  (if-let ((resource (tabulated-list-get-id (mouse-set-point click))))
      (switch-to-buffer-other-window
       (kubed-display-resource-in-buffer
        (concat "*Kubed "
                (kubed-display-resource-short-description
                 kubed-list-type resource kubed-list-context kubed-list-namespace)
                "*")
        kubed-list-type resource kubed-list-context kubed-list-namespace))
    (user-error "No Kubernetes resource at point")))

(defun kubed-list-delete (click)
  "Delete Kubernetes resource at CLICK."
  (interactive (list last-nonmenu-event) kubed-list-mode)
  (if-let ((resource (tabulated-list-get-id (mouse-set-point click))))
      (when (y-or-n-p (format "Delete `%s'?" resource))
        (kubed-delete-resources kubed-list-type (list resource)
                                kubed-list-context kubed-list-namespace))
    (user-error "No Kubernetes resource at point")))

(defun kubed-list-patch (click)
  "Patch Kubernetes resource at CLICK."
  (interactive (list last-nonmenu-event) kubed-list-mode)
  (if-let ((resource (tabulated-list-get-id (mouse-set-point click))))
      (kubed-patch kubed-list-type resource
                   (kubed-read-patch) kubed-list-context kubed-list-namespace)
    (user-error "No Kubernetes resource at point")))

(defun kubed-list-edit (click)
  "Edit Kubernetes resource at CLICK."
  (interactive (list last-nonmenu-event) kubed-list-mode)
  (if-let ((resource (tabulated-list-get-id (mouse-set-point click))))
      (kubed-edit-resource kubed-list-type resource
                           kubed-list-context kubed-list-namespace)
    (user-error "No Kubernetes resource at point")))

(defun kubed-list-kubectl-command (click)
  "Use Kubernetes resource at CLICK as argument for `kubectl' command."
  (interactive (list last-nonmenu-event) kubed-list-mode)
  (if-let ((resource (tabulated-list-get-id (mouse-set-point click))))
      (kubed-kubectl-command
       (kubed-read-kubectl-command
        "Execute command: "
        (cons (concat
               " "
               (when kubed-list-namespace (concat "-n " kubed-list-namespace " "))
               (when kubed-list-context (concat "--context " kubed-list-context " "))
               kubed-list-type " " resource)
              ;; Put point after "kubectl ".
              0)))
    (user-error "No Kubernetes resource at point")))

(defun kubed-list-create (definition &optional kind)
  "Create Kubernetes resource of kind KIND from definition file DEFINITION."
  (interactive (list (kubed-read-resource-definition-file-name)))
  (kubed-create definition kind
                ;; This is also called from non-list buffers via
                ;; `kubed-create-FOO' commands, in which case context is
                ;; nil, which is means we default to current context.
                kubed-list-context)
  (kubed-list-update t))

(defun kubed-list-column-number-at-point ()
  "Return table column number at point."
  (let ((start (current-column))
        (nb-cols (1- (length tabulated-list-format)))
        (col-nb 0)
        (total-width tabulated-list-padding)
        (found nil))
    (while (and (not found) (< col-nb nb-cols))
      (if (>= start
              (setq total-width
                    (+ total-width
                       (cadr (aref tabulated-list-format col-nb))
                       (or (plist-get (nthcdr 3 (aref tabulated-list-format
                                                      col-nb))
                                      :pad-right)
                           1))))
          (setq col-nb (1+ col-nb))
        (setq found t)))
    col-nb))

(defun kubed-list-fit-column-width-to-content (n)
  "Fit width of Nth table column to its content.

If N is negative, fit all columns.  Interactively, N is the column
number at point, or the numeric prefix argument if you provide one."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (kubed-list-column-number-at-point)))
   kubed-list-mode)
  (if (< n 0)
      ;; Fit all columns.
      (let* ((num-cols (length tabulated-list-format))
             (widths (apply #'vector (seq-map
                                      ;; +2 to make room for sorting icon.
                                      (lambda (col) (+ 2 (length (car col))))
                                      tabulated-list-format))))
        (save-excursion
          (goto-char (point-min))
          (while-let ((entry (tabulated-list-get-entry)))
            (dotimes (i num-cols)
              (aset widths i (max (aref widths i) (length (aref entry i)))))
            (forward-line)))
        (setq tabulated-list-format (copy-tree tabulated-list-format t))
        (dotimes (i num-cols)
          (setf (cadr (aref tabulated-list-format i))
                (1+ (aref widths i)))))
    ;; Fit Nth column.
    (let* ((width (+ 2 (length (car (aref tabulated-list-format n))))))
      (save-excursion
        (goto-char (point-min))
        (while-let ((entry (tabulated-list-get-entry)))
          (setq width (max width (length (aref entry n))))
          (forward-line)))
      (setq tabulated-list-format (copy-tree tabulated-list-format t))
      (setf (cadr (aref tabulated-list-format n)) (1+ width))))
  (tabulated-list-print t)
  (tabulated-list-init-header))

(declare-function kubed-list-transient "kubed-transient" ())

(defvar-keymap kubed-list-mode-map
  :doc "Common keymap for Kubernetes resource list buffers."
  "RET" #'kubed-list-select-resource
  "o" #'kubed-list-select-resource-other-window
  "C-o" #'kubed-list-display-resource
  "D" #'kubed-list-delete
  "P" #'kubed-list-patch
  "x" #'kubed-list-delete-marked
  "e" #'kubed-list-edit
  "!" #'kubed-list-kubectl-command
  "G" #'kubed-list-update
  "/" #'kubed-list-set-filter
  "|" #'kubed-list-fit-column-width-to-content
  "d" #'kubed-list-mark-for-deletion
  "u" #'kubed-list-unmark
  "w" #'kubed-list-copy-as-kill
  "C-i" #'kubed-list-next-column
  "TAB" #'kubed-list-next-column
  "C-S-i" #'kubed-list-previous-column
  "S-TAB" #'kubed-list-previous-column
  "<backtab>" #'kubed-list-previous-column
  "+" #'kubed-list-create
  "?" #'kubed-list-transient)

(defun kubed-list-entries ()
  "`tabulated-list-entries' function for `kubed-list-mode'."
  (let ((pred (kubed-list-interpret-filter))
        (ents nil))
    (dolist (ent (alist-get 'resources (kubed--alist kubed-list-type
                                                     kubed-list-context
                                                     kubed-list-namespace)))
      (when (funcall pred ent) (push ent ents)))
    (reverse ents)))

;;;###autoload
(defun kubed-list-handle-bookmark (bookmark)
  "Display Kubernetes resource according to BOOKMARK."
  (require 'bookmark)
  (let* ((type (bookmark-prop-get bookmark 'type))
         (context (bookmark-prop-get bookmark 'context))
         (namespace (bookmark-prop-get bookmark 'namespace))
         (current (bookmark-prop-get bookmark 'current))
         (buff-fn (intern (format "kubed-%s-buffer" type)))
         (buff (apply buff-fn context (when namespace (list namespace)))))
    (set-buffer buff)
    (when current (kubed-list-go-to-line current))))

(put 'kubed-list-handle-bookmark 'bookmark-handler-type "KubedList")

(defun kubed-list-make-bookmark ()
  "Return bookmark pointing to currently displayed Kubernetes resource."
  (require 'bookmark)
  (cons
   (concat kubed-list-type
           (when kubed-list-namespace (concat "@" kubed-list-namespace))
           (when kubed-list-context   (concat "[" kubed-list-context "]")))
   (append
    (list
     (cons 'handler #'kubed-list-handle-bookmark)
     (cons 'filter kubed-list-filter)
     (cons 'type kubed-list-type)
     (cons 'context kubed-list-context)
     (cons 'current (tabulated-list-get-id))
     (cons 'namespace kubed-list-namespace)))))

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
                      (let ((id (tabulated-list-get-id)))
                        (when-let ((mark (alist-get id marks nil nil #'equal)))
                          (tabulated-list-put-tag mark)))
                      (forward-line))))))
            nil t)
  (setq-local truncate-string-ellipsis (propertize ">" 'face 'shadow))
  (setq-local tabulated-list-entries #'kubed-list-entries)
  (setq-local bookmark-make-record-function #'kubed-list-make-bookmark)
  (add-hook 'context-menu-functions #'kubed-list-context-menu nil t))

(defun kubed-delete-resources (type resources context &optional namespace)
  "Delete Kubernetes RESOURCES of type TYPE."
  (interactive
   (let* ((type (kubed-read-resource-type "Resource type to delete"))
          (context (kubed-current-context))
          (namespace
           (when (kubed-namespaced-p type)
             (or (seq-some
                  (lambda (arg)
                    (when (string-match "--namespace=\\(.+\\)" arg)
                      (match-string 1 arg)))
                  (kubed-transient-args 'kubed-transient-delete))
                 (let ((cur (kubed-current-namespace)))
                   (if current-prefix-arg
                       (kubed-read-namespace "Namespace" cur)
                     cur))))))
     (list type (kubed-read-resource-name type "Delete" nil t nil namespace)
           context namespace)))
  (unless resources (user-error "You didn't specify %s to delete" type))
  (message (format "Deleting Kubernetes %s `%s'..."
                   type (string-join resources "', `")))
  (if (zerop (apply #'call-process
                    kubed-kubectl-program nil nil nil
                    "delete" type
                    (append (when namespace (list "-n" namespace))
                            (when context (list "--context" context))
                            resources)))
      (message (format "Deleting Kubernetes %s `%s'...  Done."
                       type (string-join resources "', `")))
    (error (format "Failed to delete Kubernetes %s `%s'"
                   type (string-join resources "', `")))))

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

(defun kubed-edit-resource (type resource &optional context namespace)
  "Edit Kubernetes RESOURCE of type TYPE."
  (unless (bound-and-true-p server-process) (server-start))
  (let ((process-environment
         (cons (kubed--static-if (<= 30 emacs-major-version)
                   (concat "KUBE_EDITOR=" emacsclient-program-name)
                 "KUBE_EDITOR=emacsclient")
               process-environment)))
    (apply #'start-process
           (format "*kubed-%S-edit*" type) nil
           kubed-kubectl-program "edit" type resource
           (append
            (when namespace (list "-n" namespace))
            (when context (list "--context" context))))))

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
RESOURCE at point.  For example, if RESOURCE is `pod', the following
COMMANDS element defines a command `kubed-pods-frob' and binds it to the
key \"f\" in `kubed-pods-mode-map':

  (frob \"f\" \"Frobnicate\"
        (message \"Preparing...\")
        (frobnicate-pod pod)
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
  (let ((plrl-var (intern (format "%Ss"                         resource)))
        (read-fun (intern (format "kubed-read-%S"               resource)))
        (dsp-name (intern (format "kubed-display-%S"            resource)))
        (edt-name (intern (format "kubed-edit-%S"               resource)))
        (crt-name (intern (format "kubed-create-%S"             resource)))
        (map-name (intern (format "kubed-%S-prefix-map"         resource)))
        (menu-map (intern (format "kubed-%S-menu-map"           resource)))
        (namespaced t)
        (keyword nil)
        frmt-var buff-fun list-cmd expl-cmd dlt-name mod-name
        ctxt-fun crt-spec prf-keys hist-var)

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

    (setq frmt-var (intern (format "kubed-%S-columns"       plrl-var))
          hist-var (intern (format "kubed-%S-history"       plrl-var))
          buff-fun (intern (format "kubed-%S-buffer"        plrl-var))
          list-cmd (intern (format "kubed-list-%S"          plrl-var))
          expl-cmd (intern (format "kubed-explain-%S"       plrl-var))
          dlt-name (intern (format "kubed-delete-%S"        plrl-var))
          mod-name (intern (format "kubed-%S-mode"          plrl-var))
          ctxt-fun (intern (format "kubed-%S-context-menu"  plrl-var)))
    ;; Generate code.
    `(progn
       (setf (alist-get ,(symbol-name plrl-var) kubed--columns nil nil #'string=)
             (list
              '("NAME:.metadata.name")
              ,@(mapcar (lambda (p)
                          `(cons ,(concat (upcase (symbol-name (car p)))
                                          ":"
                                          (cadr p))
                                 ,(nth 4 p)))
                        properties)))

       (defvar ,hist-var nil
         ,(format "History list for `%S'." read-fun))

       (defun ,read-fun (prompt &optional default multi context
                                . ,(when namespaced '(namespace)))
         ,(format "Prompt with PROMPT for a Kubernetes %S name.

Optional argument DEFAULT is the minibuffer default argument.

Non-nil optional argument MULTI says to read and return a list
of %S, instead of just one." resource plrl-var)
         (kubed-read-resource-name ,(symbol-name plrl-var) prompt default multi context . ,(when namespaced '(namespace))))

       (defun ,dsp-name (,resource &optional context . ,(when namespaced '(namespace)))
         ,(format "Display Kubernetes %S %s."
                  resource (upcase (symbol-name resource)))
         (interactive
          ,(if namespaced
               `(let ((namespace
                       ;; Consult transient for --namespace.
                       (or (seq-some
                            (lambda (arg)
                              (when (string-match "--namespace=\\(.+\\)" arg)
                                (match-string 1 arg)))
                            (kubed-transient-args 'kubed-transient-display))
                           (and current-prefix-arg (kubed-read-namespace "Namespace" (kubed-current-namespace))))))
                  (list (,read-fun "Display" nil nil nil namespace) nil namespace))
             `(list (,read-fun "Display"))))
         (kubed-display-resource
          ,(symbol-name plrl-var) ,resource (or context (kubed-current-context))
          . ,(when namespaced '(namespace))))

       (defun ,edt-name (,resource &optional context . ,(when namespaced '(namespace)))
         ,(format "Edit Kubernetes %S %s." resource (upcase (symbol-name resource)))
         (interactive ,(if namespaced
                           `(let ((namespace (and current-prefix-arg
                                                  (kubed-read-namespace
                                                   "Namespace" (kubed-current-namespace)))))
                              (list (,read-fun "Edit" nil nil nil namespace) nil namespace))
                         `(list (,read-fun "Edit"))))
         (kubed-edit-resource ,(symbol-name plrl-var) ,resource context
                              . ,(when namespaced '(namespace))))

       (defun ,dlt-name (,plrl-var &optional context
                                   . ,(when namespaced '(namespace)))
         ,(format "Delete Kubernetes %S %s." plrl-var
                  (upcase (symbol-name plrl-var)))
         (interactive ,(if namespaced
                           `(let ((namespace (and current-prefix-arg
                                                  (kubed-read-namespace
                                                   "Namespace" (kubed-current-namespace)))))
                              (list (,read-fun "Delete" nil t nil namespace) nil namespace))
                         `(list (,read-fun "Delete" nil t))))
         (unless ,plrl-var
           (user-error ,(format "You didn't specify %S to delete" plrl-var)))
         (kubed-delete-resources ,(symbol-name plrl-var) ,plrl-var context
                                 . ,(when namespaced '(namespace))))

       ,(if crt-spec `(defun ,crt-name . ,crt-spec)
          `(defun ,crt-name (definition)
             ,(format "Create Kubernetes %s from definition file DEFINITION."
                      (symbol-name resource))
             (interactive (list (kubed-read-resource-definition-file-name
                                 ,(symbol-name resource))))
             (kubed-list-create definition ,(symbol-name resource))))

       ,@(let ((click-var (gensym "click")))
           (mapcar
            (pcase-lambda (`(,suffix ,_key ,desc . ,body))
              `(defun ,(intern (format "kubed-%S-%S" plrl-var suffix)) (,click-var)
                 ,(format "%s Kubernetes %S at point." desc resource)
                 (interactive (list last-nonmenu-event) ,mod-name)
                 (if-let ((,resource (tabulated-list-get-id (mouse-set-point ,click-var))))
                     (progn ,@body)
                   (user-error ,(format "No Kubernetes %S at point" resource)))))
            commands))

       (defvar-keymap ,(intern (format "kubed-%S-mode-map" plrl-var))
         :doc ,(format "Keymap for `%S" mod-name)
         "+" #',crt-name
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
                              (funcall ,sorter (aref (cadr l) ,i) (aref (cadr r) ,i)))
                         t))
                 (nthcdr 5 p))
                res))
             (reverse res)))

       (defun ,ctxt-fun (menu . ,(if commands '(click) '(_click)))
         ,@(when commands
             `((when (tabulated-list-get-entry (posn-point (event-start click)))
                 ,@(mapcar
                    (pcase-lambda (`(,suffix ,_key ,desc . ,_body))
                      `(define-key
                        menu [,(intern (format "kubed-%S-%S" plrl-var suffix))]
                        (list 'menu-item ,(format "%s this %S" desc resource)
                              #',(intern (format "kubed-%S-%S" plrl-var suffix)))))
                    (reverse commands)))))
         menu)

       (define-derived-mode ,mod-name kubed-list-mode
         (list ,(format "Kubernetes %ss" (capitalize (symbol-name resource)))
               (list '(:eval (if (process-live-p
                                  (alist-get 'process
                                             (kubed--alist kubed-list-type
                                                           kubed-list-context
                                                           kubed-list-namespace)))
                                 (propertize "[...]" 'help-echo "Updating...")
                               (when kubed-list-filter
                                 (propertize
                                  (concat "[" (mapconcat #'prin1-to-string kubed-list-filter " ") "]")
                                  'help-echo "Current filter"))))))
         ,(format "Major mode for listing Kubernetes %S." plrl-var)
         :interactive nil
         (setq kubed-list-filter-history-variable
               ',(intern (format "kubed-%S-filter-history" plrl-var))
               kubed-list-type ,(symbol-name plrl-var)
               tabulated-list-padding 2
               tabulated-list-format (apply #'vector (cons kubed-name-column ,frmt-var)))
         (add-hook 'context-menu-functions #',ctxt-fun nil t)
         (tabulated-list-init-header))

       (defun ,buff-fun (context . ,(when namespaced '(namespace)))
         (let ((buf-name (format ,(format "*Kubed %S%%s*" plrl-var)
                                 ,(if namespaced
                                      `(concat "@" namespace "[" context "]")
                                    `(concat "[" context "]")))))
           (if-let ((buf (get-buffer buf-name))) buf
             (with-current-buffer (get-buffer-create buf-name)
               (,mod-name)
               (setq kubed-list-context context
                     ,@(when namespaced
                         '(kubed-list-namespace namespace)))
               (kubed-list-update)
               (tabulated-list-print)
               (current-buffer)))))

       (defun ,list-cmd (context . ,(when namespaced '(namespace)))
         ,(format "List Kubernetes %S." plrl-var)
         (interactive (list (kubed-current-context)
                            . ,(when namespaced
                                 '((let ((cur (kubed-current-namespace)))
                                     (if current-prefix-arg
                                         (kubed-read-namespace "Namespace" cur)
                                       cur))))))
         (pop-to-buffer (,buff-fun context . ,(when namespaced '(namespace)))))

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
         "RET" #',dsp-name
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
  (dired "C-d" "Start Dired in"
         ;; Explicit namespace in Kuberenetes remote file names
         ;; introduced in Emacs 31.  See Bug#59797.
         (kubed--static-if (<= 31 emacs-major-version)
             (dired (concat "/kubernetes:" pod "%" kubed-list-namespace ":"))
           (unless (string= kubed-list-namespace (kubed-current-namespace))
             (if (y-or-n-p
                  (format "Starting Dired in a pod in a different namespace \
requires Emacs 31 or later.
You can proceed by first switching your current namespace.
Switch to namespace `%s' and proceed?" kubed-list-namespace))
                 (kubed-set-namespace kubed-list-namespace)
               (user-error
                "Cannot start Dired in a pod in different namespace `%s'"
                kubed-list-namespace)))
           (dired (concat "/kubernetes:" pod ":"))))
  (shell "s" "Start shell in"
         (kubed--static-if (<= 31 emacs-major-version)
             (let* ((default-directory (concat "/kubernetes:" pod "%" kubed-list-namespace ":")))
               (shell (format "*kubed-pod-%s-shell*" pod)))
           (unless (string= kubed-list-namespace (kubed-current-namespace))
             (if (y-or-n-p
                  (format "Starting Shell in a pod in a different namespace \
requires Emacs 31 or later.
You can proceed by first switching your current namespace.
Switch to namespace `%s' and proceed?" kubed-list-namespace))
                 (kubed-set-namespace kubed-list-namespace)
               (user-error
                "Cannot start Shell in a pod in different namespace `%s'"
                kubed-list-namespace)))
           (let* ((default-directory (concat "/kubernetes:" pod ":")))
             (shell (format "*kubed-pod-%s-shell*" pod)))))
  (attach "a" "Attach to remote process running on"
          (kubed-attach pod (kubed-read-container pod "Container" t
                                                  kubed-list-context
                                                  kubed-list-namespace)
                        kubed-list-context kubed-list-namespace t t))
  (exec "X" "Execute command in"
        (let ((container (kubed-read-container pod "Container" t
                                               kubed-list-context
                                               kubed-list-namespace))
              (cmd-args (split-string-and-unquote
                         (read-string "Execute command: "))))
          (kubed-exec pod (car cmd-args) container kubed-list-context
                      kubed-list-namespace t t (cdr cmd-args))))
  (logs "l" "Show logs for a container of"
        (kubed-logs pod (kubed-read-container pod "Container" t
                                              kubed-list-context kubed-list-namespace)
                    kubed-list-context kubed-list-namespace))
  (forward-port "F" "Forward local network port to remote port of"
                (let ((local-port (read-number "Forward local port: ")))
                  (kubed-forward-port-to-pod
                   pod local-port
                   (read-number (format "Forward local port %d to remote port: "
                                        local-port))
                   kubed-list-context
                   kubed-list-namespace))))

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
  ((name &optional context) "Create Kubernetes namespace NAME in CONTEXT."
   (interactive (list (read-string "Create namespace with name: ")))
   (unless (zerop
            (apply #'call-process
                   kubed-kubectl-program nil nil nil
                   "create" "namespace" name
                   (when context (list "--context" context))))
     (user-error "Failed to create Kubernetes namespace with name `%s'" name))
   (kubed-update "namespaces" (or context (kubed-current-context)))
   (message "Created Kubernetes namespace with name `%s'." name))
  (set "s" "Set current namespace to"
       (save-excursion
         (goto-char (point-min))
         (while (not (eobp))
           (when (eq (char-after) ?*)
             (tabulated-list-put-tag " "))
           (forward-line)))
       (kubed-set-namespace namespace kubed-list-context)
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

(defun kubed-create-job-from-cronjob (name cronjob &optional context namespace)
  "Create Kubernetes job with name NAME from cronjob CRONJOB.

Optional argument CONTEXT is the `kubectl' context to use, defaulting to
the current context; NAMESPACE is the namespace to use for the job,
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
            (format "Create job `%s' from cronjob" name) nil nil nil namespace)
           nil namespace)))
  (let ((context (or context (kubed-current-context)))
        (namespace (or namespace (kubed-current-namespace context))))
    (unless (zerop
             (call-process
              kubed-kubectl-program nil nil nil
              "create" "job" name "--from" (concat "cronjob/" cronjob)
              "-n" namespace "--context" context))
      (user-error "Failed to create Kubernetes job `%s'" name))
    (kubed-update "jobs" context namespace)
    (message "Created Kubernetes job `%s'." name)))

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
  ((name image &optional context namespace command)
   "Create Kubernetes job with name NAME executing COMMAND in IMAGE.

Optional argument CONTEXT is the `kubectl' context to use, defaulting to
the current context; NAMESPACE is the namespace to use for the job,
defaulting to the current namespace."
   (interactive
    (let ((name (read-string "Create job with name: "))
          (image nil) (context nil) (namespace nil) (command nil))
      (dolist (arg (kubed-transient-args 'kubed-transient-create-job))
        (cond
         ((string-match "--image=\\(.+\\)" arg)
          (setq image (match-string 1 arg)))
         ((string-match "--context=\\(.+\\)" arg)
          (setq context (match-string 1 arg)))
         ((string-match "--namespace=\\(.+\\)" arg)
          (setq namespace (match-string 1 arg)))
         ((string-match "-- =\\(.+\\)" arg)
          (setq command (split-string-and-unquote (match-string 1 arg))))))
      (unless image
        (setq image (kubed-read-container-image "Image to run in job")))
      (list name image context namespace command)))
   (unless (zerop
            (apply #'call-process
                   kubed-kubectl-program nil nil nil
                   "create" "job" name "--image" image
                   (append
                    (when namespace (list "-n" namespace))
                    (when context (list "--context" context))
                    (when command (cons "--" command)))))
     (user-error "Failed to create Kubernetes job `%s'" name))
   (message "Created Kubernetes job `%s'." name)))

;;;###autoload
(defun kubed-watch-deployment-status (dep &optional context namespace)
  "Show and update status of Kubernetes deployment DEP in a dedicate buffer.

Optional argument CONTEXT is the `kubectl' context to use, defaulting to
the current context; NAMESPACE is the namespace of DEP, defaulting to
the current namespace."
  (interactive
   (let ((namespace (seq-some
                     (lambda (arg)
                       (when (string-match "--namespace=\\(.+\\)" arg)
                         (match-string 1 arg)))
                     (kubed-transient-args 'kubed-transient-rollout))))
     (list (kubed-read-deployment "Watch deployment status" nil nil nil namespace)
           nil namespace)))
  (let ((buf (get-buffer-create "*kubed-deployment-status*"))
        (context (or context (kubed-current-context)))
        (namespace (or namespace (kubed-current-namespace context))))
    (with-current-buffer buf (erase-buffer))
    (make-process
     :name "*kubed-watch-deployment-status*"
     :buffer buf
     :command (list kubed-kubectl-program "rollout" "status"
                    "deployment" dep "-n" namespace "--context" context)
     :sentinel
     (lambda (_proc status)
       (when (member status
                     '("finished\n" "exited abnormally with code 1\n"))
         (message "Deployment complete")
         (kubed-update "deployments" context namespace))))
    (display-buffer buf)))

(defcustom kubed-restart-deployment-watch-status t
  "Whether to pop up a progress buffer when restarting Kubernetes deployments."
  :type 'boolean)

;;;###autoload
(defun kubed-restart-deployment (dep &optional context namespace)
  "Restart Kubernetes deployment DEP in namespace NAMESPACE via CONTEXT.
If NAMESPACE is nil or omitted, it defaults to the current namespace."
  (interactive
   (let ((namespace (seq-some
                     (lambda (arg)
                       (when (string-match "--namespace=\\(.+\\)" arg)
                         (match-string 1 arg)))
                     (kubed-transient-args 'kubed-transient-rollout))))
     (list (kubed-read-deployment "Restart deployment" nil nil nil namespace)
           nil namespace)))
  (unless (zerop
           (apply #'call-process
                  kubed-kubectl-program nil nil nil
                  "rollout" "restart" "deployment" dep
                  (append
                   (when namespace (list "-n" namespace))
                   (when context (list "--context" context)))))
    (user-error "Failed to restart Kubernetes deployment `%s'" dep))
  (message "Restarting Kubernetes deployment `%s'." dep)
  (when kubed-restart-deployment-watch-status
    (kubed-watch-deployment-status dep context namespace)))

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
  ((name images &optional context namespace replicas port command)
   "Deploy IMAGES to Kubernetes in deployment with name NAME.

Optional argument NAMESPACE is the namespace to use for the deployment,
defaulting to the current namespace, REPLICAS in the number of replicas
to create for each image, PORT is the port to expose, and COMMAND is an
optional command to run in the images."
   (interactive
    (let ((name (read-string "Create deployment with name: "))
          (images nil)
          (replicas (prefix-numeric-value current-prefix-arg))
          (port nil) (command nil) (context nil) (namespace nil))
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
         ((string-match "--context=\\(.+\\)" arg)
          (setq context (match-string 1 arg)))
         ((string-match "-- =\\(.+\\)" arg)
          (setq command (split-string-and-unquote (match-string 1 arg))))))
      (unless images
        (setq images (kubed-read-container-image "Images to deploy" nil t)))
      (list name images context namespace replicas port command)))
   (unless (zerop
            (apply #'call-process
                   kubed-kubectl-program nil nil nil
                   "create" "deployment" name
                   (append
                    (mapcar (lambda (image) (concat "--image=" image)) images)
                    (when namespace (list (concat "--namespace=" namespace)))
                    (when context (list (concat "--context=" context)))
                    (when replicas (list (format "--replicas=%d" replicas)))
                    (when port (list (format "--port=%d" port)))
                    (when command (cons "--" command)))))
     (user-error "Failed to create Kubernetes deployment `%s'" name))
   (message "Created Kubernetes deployment `%s'." name)
   (kubed-list-update t))
  (restart "R" "Restart"
           (kubed-restart-deployment deployment kubed-list-context kubed-list-namespace)
           (unless kubed-restart-deployment-watch-status
             (message "Deployment restarting")
             (kubed-list-update t)))
  (watch "W" "Watch" (kubed-watch-deployment-status
                      deployment kubed-list-context kubed-list-namespace)))

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

(defun kubed-cronjob-suspended-p (cj &optional context ns)
  "Return non-nil if cronjob CJ in CONTEXT and namespace NS is suspended."
  (equal (car (apply #'process-lines
                     kubed-kubectl-program
                     "get" "cronjobs" cj
                     "-o" "custom-columns=SUSPENDED:.spec.suspend" "--no-headers"
                     (append
                      (when ns (list "-n" ns))
                      (when context (list "--context" context)))))
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
  ((name image schedule &optional context namespace command)
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
          (context nil)
          (namespace nil))
      (dolist (arg (kubed-transient-args 'kubed-transient-create-cronjob))
        (cond
         ((string-match "--image=\\(.+\\)" arg)
          (setq image (match-string 1 arg)))
         ((string-match "--schedule=\\(.+\\)" arg)
          (setq schedule (match-string 1 arg)))
         ((string-match "--namespace=\\(.+\\)" arg)
          (setq namespace (match-string 1 arg)))
         ((string-match "--context=\\(.+\\)" arg)
          (setq context (match-string 1 arg)))
         ((string-match "-- =\\(.+\\)" arg)
          (setq command (split-string-and-unquote (match-string 1 arg))))))
      (unless image
        (setq image (kubed-read-container-image "Image to run")))
      (unless schedule
        (setq schedule (read-string "Cron schedule: " "* * * * *")))
      (list name image schedule context namespace command)))
   (unless (zerop
            (apply #'call-process
                   kubed-kubectl-program nil nil nil
                   "create" "cronjob" name
                   "--image" image "--schedule" schedule
                   (append
                    (when context (list "--context" context))
                    (when namespace (list "--namespace" namespace))
                    (when command (cons "--" command)))))
     (user-error "Failed to create Kubernetes cronjob `%s'" name))
   (message "Created Kubernetes cronjob `%s'." name))
  ;; Commands in *kubed-cronjobs* buffer.
  ( toggle-suspension "T" "Toggle suspension of"
    (kubed-patch "cronjobs" cronjob
                 (format
                  "{\"spec\": {\"suspend\": %s}}"
                  (if (kubed-cronjob-suspended-p
                       cronjob kubed-list-context kubed-list-namespace)
                      "false" "true"))
                 kubed-list-context kubed-list-namespace))
  ( create-job "j" "Create job from"
    (kubed-create-job-from-cronjob
     (read-string "Create job with name: ") cronjob
     kubed-list-context kubed-list-namespace)))

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
  ((name rules &optional context namespace class default-backend annotations)
   "Create Kubernetes ingress with name NAME and rules RULES.

Optional argument NAMESPACE is the namespace to use for the ingress,
defaulting to the current namespace.  CLASS is the ingress class,
ANNOTATIONS are a list of annotations for the created ingress, and
DEFAULT-BACKEND is the service to use as a backend for unhandled URLs."
   (interactive
    (let ((name (read-string "Create ingress with name: "))
          (rules nil)
          (context nil)
          (namespace nil)
          (class nil)
          (annotations nil)
          (default-backend nil))
      (dolist (arg (kubed-transient-args 'kubed-transient-create-ingress))
        (cond
         ((string-match "--rule=\\(.+\\)" arg)
          (push (match-string 1 arg) rules))
         ((string-match "--context=\\(.+\\)" arg)
          (setq context (match-string 1 arg)))
         ((string-match "--namespace=\\(.+\\)" arg)
          (setq namespace (match-string 1 arg)))
         ((string-match "--class=\\(.+\\)" arg)
          (setq class (match-string 1 arg)))
         ((string-match "--default-backend=\\(.+\\)" arg)
          (setq default-backend (match-string 1 arg)))
         ((string-match "--annotation=\\(.+\\)" arg)
          (push (match-string 1 arg) annotations))))
      (unless rules (setq rules (kubed-read-ingress-rules)))
      (list name rules context namespace class default-backend annotations)))
   (unless (zerop
            (apply #'call-process
                   kubed-kubectl-program nil nil nil
                   "create" "ingress" name
                   (append
                    (mapcan (lambda (rule) (list "--rule" rule)) rules)
                    (when namespace (list "--namespace" namespace))
                    (when context (list "--context" context))
                    (when class (list "--class" class))
                    (when default-backend
                      (list "--default-backend" default-backend))
                    (mapcan (lambda (ann) (list "--annotation" ann))
                            annotations))))
     (user-error "Failed to create Kubernetes ingress `%s'" name))
   (message "Created Kubernetes ingress `%s'." name)))

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
  (message "Now using Kubernetes context `%s'." context))

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
                      (error "`kubectl config view' failed"))
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
(defun kubed-set-namespace (ns &optional context)
  "Set default Kubernetes namespace in CONTEXT to NS."
  (interactive
   (list (kubed-read-namespace "Set namespace" (kubed-current-namespace))))
  (unless (zerop
           (apply #'call-process
                  kubed-kubectl-program nil nil nil
                  "config" "set-context" "--current" "--namespace" ns
                  (when context (list "--context" context))))
    (user-error "Failed to set Kubernetes namespace to `%s'" ns))
  (message "Kubernetes namespace is now `%s'." ns))

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
(defun kubed-apply (config &optional kind context)
  "Apply CONFIG to Kubernetes resource of kind KIND via CONTEXT."
  (interactive
   (list (or (seq-some
              (lambda (arg)
                (when (string-match "--filename=\\(.+\\)" arg)
                  (match-string 1 arg)))
              (kubed-transient-args 'kubed-transient-apply))
             (kubed-read-resource-definition-file-name))))
  (let ((kind (or kind "resource")))
    (message "Applying Kubernetes %s configuration `%s'..." kind config)
    (apply #'call-process kubed-kubectl-program nil nil nil
           "apply" "-f" (expand-file-name config)
           (when context (list "--context" context)))
    (message "Applying Kubernetes %s configuration `%s'... Done." kind config)))

;;;###autoload
(defun kubed-create (definition &optional kind context)
  "Create resource of kind KIND with definition DEFINITION via CONTEXT."
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
             (car (apply #'process-lines kubed-kubectl-program
                         "create" "-f" (expand-file-name definition)
                         "-o" "jsonpath={.metadata.name}"
                         (when context (list "--context" context)))))))

;;;###autoload
(defun kubed-run
    (pod image &optional context namespace port attach stdin tty rm envs command args)
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
         (image nil) (port nil) (context nil) (namespace nil)
         (attach nil) (stdin nil) (tty nil) (rm nil) (envs nil)
         (command nil) (args nil))
     (dolist (arg (kubed-transient-args 'kubed-transient-run))
       (cond
        ((string-match "--image=\\(.+\\)" arg)
         (setq image (match-string 1 arg)))
        ((string-match "--port=\\(.+\\)" arg)
         (setq port (string-to-number (match-string 1 arg))))
        ((string-match "--namespace=\\(.+\\)" arg)
         (setq namespace (match-string 1 arg)))
        ((string-match "--context=\\(.+\\)" arg)
         (setq context (match-string 1 arg)))
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
     (list pod image context namespace port attach stdin tty rm envs command args)))
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
                     (when context (list (concat "--context=" context)))
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
    (message "Image `%s' is now running in pod `%s'." image pod)))

(defun kubed-pod-containers (pod &optional context namespace)
  "Return list of containers in Kubernetes pod POD in NAMESPACE in CONTEXT."
  (string-split
   (car (apply #'process-lines
               kubed-kubectl-program "get"
               "pod" pod "-o" "jsonpath={.spec.containers[*].name}"
               (append
                (when namespace (list "--namespace" namespace))
                (when context (list "--context" context)))))
   " "))

(defun kubed-pod-default-container (pod &optional context namespace)
  "Return default container of Kubernetes pod POD in NAMESPACE in CONTEXT."
  (car (apply #'process-lines
              kubed-kubectl-program
              "get" "pod" pod "-o"
              "jsonpath={.metadata.annotations.kubectl\\.kubernetes\\.io/default-container}"
              (append
               (when namespace (list "--namespace" namespace))
               (when context (list "--context" context))))))

(defun kubed-read-container
    (pod prompt &optional guess context namespace)
  "Prompt with PROMPT for a container in POD and return its name.

Non-nil optional argument GUESS says to try and guess which container to
use without prompting: if the pod has a
\"kubectl.kubernetes.id/default-container\" annotation, use the
container that this annotation specifes; if there's just one container,
use it; otherwise, fall back to prompting."
  (let ((default (kubed-pod-default-container pod context namespace))
        (all 'unset))
    (or
     ;; There's a default container, so that's our guess.
     (and guess default)
     ;; No default, but we're allowed to guess, so check if there's just
     ;; one container, and if so that's our guess.
     (and guess (setq all (kubed-pod-containers pod context namespace))
          (null (cdr all))
          (car all))
     ;; No guessing, prompt.
     (completing-read (format-prompt prompt default)
                      (completion-table-dynamic
                       (lambda (_)
                         (if (eq all 'unset)
                             (setq all (kubed-pod-containers pod context namespace))
                           all)))
                      nil t nil nil default))))

;;;###autoload
(defun kubed-logs (pod container context namespace)
  "Show logs for container CONTAINER in Kubernetes pod POD."
  (interactive
   (let* ((context (kubed-current-context))
          (n (kubed-current-namespace context))
          (n (if current-prefix-arg (kubed-read-namespace "Namespace" n nil context) n))
          (p (kubed-read-pod "Show logs for pod" nil nil context n))
          (c (kubed-read-container p "Container" nil context n)))
     (list p c context n)))
  (let ((buf (generate-new-buffer
              (format "*kubed-logs %s[%s] in %s*" pod container namespace))))
    (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
    (message "Getting logs for container `%s' in pod `%s' in namespace `%s'..." container pod namespace)
    (apply #'start-process
           "*kubed-logs*" buf
           kubed-kubectl-program "logs"
           "-f" "-c" container pod
           (append
            (when namespace (list "-n" namespace))
            (when context (list "--context" context))))
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
(defun kubed-forward-port-to-pod
    (pod local-port remote-port context namespace)
  "Forward LOCAL-PORT to REMOTE-PORT of Kubernetes pod POD."
  (interactive
   (let* ((c (kubed-current-context))
          (n (kubed-current-namespace c))
          (n (if current-prefix-arg (kubed-read-namespace "Namespace" n nil c) n))
          (p (kubed-read-pod "Forward port to pod" nil nil c n))
          (l (read-number "Local port: "))
          (r (read-number "Remote port: ")))
     (list p l r c n)))
  (message "Forwarding local port %d to remote port %d of pod `%s' in namespace `%s'..."
           local-port remote-port pod namespace)
  (push
   (cons
    (format "pod %s %d:%d in %s" pod local-port remote-port namespace)
    (apply #'start-process
           "*kubed-port-forward*" nil
           kubed-kubectl-program "port-forward"
           pod (format "%d:%d" local-port remote-port)
           (append
            (when namespace (list "-n" namespace))
            (when context (list "--context" context)))))
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
(defun kubed-attach (pod &optional container context namespace stdin tty)
  "Attach to running process in CONTAINER in Kubernetes POD.

Optional argument NAMESPACE is the namespace in which to look for POD.
Non-nil STDIN says to connect local standard input to remote process.
Non-nil TTY says to use a TTY for standard input.

Interactively, prompt for POD; if there are multiple pod containers,
prompt for CONTAINER as well; STDIN is t unless you call this command
with \\[universal-argument] \\[universal-argument]; and TTY is t unless\
 you call this command with \\[universal-argument]."
  (interactive
   (let ((context nil) (namespace nil) (stdin t) (tty t))
     (when (<= 4  (prefix-numeric-value current-prefix-arg)) (setq tty   nil))
     (when (<= 16 (prefix-numeric-value current-prefix-arg)) (setq stdin nil))
     (dolist (arg (kubed-transient-args 'kubed-transient-attach))
       (cond
        ((string-match "--namespace=\\(.+\\)" arg)
         (setq namespace (match-string 1 arg)))
        ((string-match "--context=\\(.+\\)" arg)
         (setq context (match-string 1 arg)))
        ((equal "--stdin" arg) (setq stdin t))
        ((equal "--tty" arg) (setq tty t))))
     (let* ((c (or context (kubed-current-context)))
            (n (or namespace
                   (when current-prefix-arg
                     (kubed-read-namespace "Namespace" (kubed-current-namespace) nil c))))
            (p (kubed-read-pod "Attach to pod" nil nil c n)))
       (list p (kubed-read-container p "Container" t c n) c n stdin tty))))
  (pop-to-buffer
   (apply #'make-comint "kubed-attach" kubed-kubectl-program nil
          "attach" pod
          (append
           (when namespace (list "-n" namespace))
           (when context (list "--context" context))
           (when container (list "-c" container))
           (when stdin '("-i"))
           (when tty   '("-t"))))))

;;;###autoload
(defun kubed-diff (definition &optional include-managed context)
  "Display difference between Kubernetes resource DEFINITION and current state.

DEFINITION is either a file name or a buffer.  Interactively, prompt for
a YAML or JSON file name to use as DEFINITION.

Optional argument CONTEXT is the `kubectl' context to use, defaulting to
the current context; non-nil INCLUDE-MANAGED (interactively, the prefix
argument) says to include managed fields in the comparison."
  (interactive
   (let ((definition nil) (context nil) (include-managed nil))
     (dolist (arg (when (and (fboundp 'transient-args)
                             (fboundp 'kubed-transient-diff))
                    (transient-args 'kubed-transient-diff)))
       (cond
        ((string-match "--filename=\\(.+\\)" arg)
         (setq definition (match-string 1 arg)))
        ((string-match "--context=\\(.+\\)" arg)
         (setq context (match-string 1 arg)))
        ((equal "--show-managed-fields" arg) (setq include-managed t))))
     (list (or definition (kubed-read-resource-definition-file-name))
           (or include-managed current-prefix-arg)
           context)))
  (let ((buf (get-buffer-create "*kubed-diff*"))
        (args (cons "diff"
                    (append
                     (when context (list "--context" context))
                     (when include-managed
                       (list "--show-managed-fields" "true"))
                     (list "-f" (if (bufferp definition) "-"
                                  (expand-file-name definition)))))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (fundamental-mode)
      (if (bufferp definition)
          (with-current-buffer definition
            (apply #'call-process-region nil nil kubed-kubectl-program
                   nil buf nil args))
        (apply #'call-process kubed-kubectl-program
               nil t nil args))
      (setq buffer-read-only t)
      (diff-mode)
      (goto-char (point-min)))
    (display-buffer buf)))

;;;###autoload
(defun kubed-exec
    (pod command &optional container context namespace stdin tty args)
  "Execute COMMAND with ARGS in CONTAINER in Kubernetes POD.

Optional argument NAMESPACE is the namespace in which to look for POD.
Non-nil STDIN says to connect local standard input to remote process.
Non-nil TTY says to use a TTY for standard input.

Interactively, prompt for POD; if there are multiple pod containers,
prompt for CONTAINER as well; STDIN is t unless you call this command
with \\[universal-argument] \\[universal-argument]; and TTY is t unless\
 you call this command with \\[universal-argument]."
  (interactive
   (let ((context nil) (namespace nil) (stdin t) (tty t) (command nil) (args nil))
     (when (<= 4  (prefix-numeric-value current-prefix-arg)) (setq tty   nil))
     (when (<= 16 (prefix-numeric-value current-prefix-arg)) (setq stdin nil))
     (dolist (arg (kubed-transient-args 'kubed-transient-exec))
       (cond
        ((string-match "--namespace=\\(.+\\)" arg)
         (setq namespace (match-string 1 arg)))
        ((string-match "--context=\\(.+\\)" arg)
         (setq context (match-string 1 arg)))
        ((equal "--stdin" arg) (setq stdin t))
        ((equal "--tty" arg) (setq tty t))
        ((string-match "-- =\\(.+\\)" arg)
         (setq args    (split-string-and-unquote (match-string 1 arg))
               command (car args)
               args    (cdr args)))))
     (let* ((context (or context (kubed-current-context)))
            (n (or namespace
                   (when current-prefix-arg
                     (kubed-read-namespace "Namespace" (kubed-current-namespace) nil context))))
            (p (kubed-read-pod "Attach to pod" nil nil context n))
            (c (kubed-read-container p "Container" t context n)))
       (unless command
         (setq args    (split-string-and-unquote
                        (read-string "Execute command: "))
               command (car args)
               args    (cdr args)))
       (list p command c context n stdin tty args))))
  (pop-to-buffer
   (apply #'make-comint "kubed-exec" kubed-kubectl-program nil
          "exec" pod
          (append
           (when namespace (list "-n" namespace))
           (when context (list "--context" context))
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
(defun kubed-patch (type name patch &optional context namespace strategy)
  "Patch Kubernetes resource NAME of TYPE with patch PATCH.

Optional argument NAMESPACE is the namespace in which to look for NAME.
STRATEGY is the patch type to use, one of \"json\", \"merge\" and
\"strategic\", defaulting to \"strategic\".

Interactively, prompt for TYPE, NAME and PATCH."
  (interactive
   (let ((type (kubed-read-resource-type "Resource type to patch"))
         (context nil) (namespace nil) (strategy nil))
     (dolist (arg (kubed-transient-args 'kubed-transient-apply))
       (cond
        ((string-match "--namespace=\\(.+\\)" arg)
         (setq namespace (match-string 1 arg)))
        ((string-match "--context=\\(.+\\)" arg)
         (setq context (match-string 1 arg)))
        ((string-match "--type=\\(.+\\)" arg)
         (setq strategy (match-string 1 arg)))))
     (list type
           (kubed-read-resource-name type "Resource to patch")
           (kubed-read-patch) context namespace strategy)))
  (message "Applying patch to `%s'..." name)
  (unless (zerop
           (apply #'call-process
                  kubed-kubectl-program nil nil nil
                  "patch" type name "-p" patch
                  (append
                   (when namespace (list "-n" namespace))
                   (when context (list "--context" context))
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

(defun kubed-api-resources (&optional context only-namespaced)
  "Return list of resource types in the context CONTEXT.

Non-nil optional argument ONLY-NAMESPACED says to return only namespaced
resource types."
  (mapcar
   (lambda (line)
     (car (split-string line)))
   (apply #'process-lines
          kubed-kubectl-program
          "api-resources" "--no-headers"
          (append
           (when only-namespaced '("--namespaced=true"))
           (when context (list (concat "--context=" context)))))))

(defun kubed-resource-names (type &optional context namespace)
  "Return list of Kuberenetes resources of type TYPE in NAMESPACE via CONTEXT."
  (let ((context (or context (kubed-current-context)))
        (namespace (or namespace (kubed-current-namespace context))))
    (unless (kubed--alist type context namespace)
      (let ((proc (kubed-update type context namespace)))
        (while (process-live-p proc)
          (accept-process-output proc 1))))
    (mapcar #'car (alist-get 'resources (kubed--alist type context namespace)))))

(defun kubed-read-resource-name (type prompt &optional default multi context namespace)
  "Prompt with PROMPT for Kubernetes resource name of type TYPE.

Optional argument DEFAULT is the minibuffer default argument.  Non-nil
optional argument NAMESPACE says to use names from NAMESPACE as
completion candidates instead of the current namespace."
  (funcall
   (if multi #'completing-read-multiple #'completing-read)
   (format-prompt prompt default)
   (let ((table 'unset))
     (lambda (s p a)
       (if (eq a 'metadata)
           `(metadata (category . ,(intern (concat "kubernetes-" type))))
         (when (eq table 'unset)
           (setq table (kubed-resource-names type context namespace)))
         (complete-with-action a table s p))))
   nil 'confirm nil (intern (concat "kubed-" type "-history")) default))

(defun kubed-read-resource-type (prompt &optional default context)
  "Prompt with PROMPT for Kubernetes resource type in context CONTEXT.

Optional argument DEFAULT is the minibuffer default argument."
  (completing-read
   (format-prompt prompt default)
   (kubed-api-resources context)
   nil 'confirm nil nil default))

(defun kubed-read-resource-field (prompt &optional default context)
  "Prompt with PROMPT for Kubernetes resource type or field name.

Optional argument DEFAULT is the minibuffer default argument, and
CONTEXT is the `kubectl' context to use."
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
                      (kubed-api-resources context)
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

(defun kubed-read-kubectl-command (prompt &optional initial)
  "Prompt with PROMPT for a `kubectl' command line.

Optional argument INITIAL added to the initial minibuffer input
following the value of `kubed-kubectl-program' and a space character."
  (let ((init (if (consp initial)
                  (cons    (concat kubed-kubectl-program " " (car initial))
                        (+ (length kubed-kubectl-program) 1  (cdr initial)))
                (concat kubed-kubectl-program " " initial))))
    (cobra-read-command-line prompt init 'kubed-kubectl-command-history)))

;;;###autoload
(defun kubed-kubectl-command (command)
  "Execute `kubectl' COMMAND.

This function calls `shell-command' (which see) to do the work.

Interactively, prompt for COMMAND with completion for `kubectl' arguments."
  (interactive
   (list (kubed-read-kubectl-command
          "Command: "
          (let* ((args (kubed-transient-args))
                 (prefix (and (fboundp 'transient-prefix-object)
                              (transient-prefix-object)))
                 (scope (and prefix (fboundp 'eieio-oref)
                             (eieio-oref prefix 'scope))))
            (when (or args scope)
              (concat (string-join (append scope args) " ") " "))))))
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
  "+" #'kubed-create
  "*" #'kubed-apply
  "R" #'kubed-run
  "=" #'kubed-diff
  "E" #'kubed-explain
  "P" #'kubed-patch
  "RET" #'kubed-display-resource
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
  "<explain>"          '("Explain Type or Field" . kubed-explain)
  "<use-context>"      '("Set Current Context"   . kubed-use-context)
  "<run>"              '("Run Image"             . kubed-run)
  "<apply>"            '("Apply Config"          . kubed-apply)
  "<create>"           '("Create Resource"       . kubed-create)
  "<display>"          '("Display Resource"      . kubed-display-resource))

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
