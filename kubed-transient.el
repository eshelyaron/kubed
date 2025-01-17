;;; kubed-transient.el --- Kubernetes transient menus   -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Keywords: tools

;;; Commentary:

;; This library extends Kubed with transient menus for various
;; Kubernetes operations.

;;; Code:

(require 'kubed)
(require 'transient)

(defun kubed-transient-read-date (prompt default _history)
  "Prompt with PROMPT for a date, defaulting to DEFAULT.

Return an RFC3339 string representation of the selected date."
  (require 'org)
  (when (fboundp 'org-read-date)
    (format-time-string "%Y-%m-%dT%H:%M:%S%:z"
                        (org-read-date 'with-time t nil prompt default))))

(defun kubed-transient-read-context (prompt _initial-input _history)
  "Prompt with PROMPT for Kubernetes context."
  (kubed-read-context prompt (kubed-local-context)))

(defun kubed-transient-read-namespace (prompt _initial-input _history)
  "Prompt with PROMPT for Kubernetes namespace."
  (let ((context (or (seq-some (lambda (s)
                                 (and (cl-typep s 'transient-infix)
                                      (equal (oref s argument) "--context=")
                                      (oref s value)))
                               transient--suffixes)
                     (kubed-local-context))))
    (kubed-read-namespace prompt (kubed--namespace context) nil context)))

(defun kubed-transient-read-ingressclass (prompt _initial-input _history)
  "Prompt with PROMPT for Kubernetes ingress class."
  (kubed-read-ingressclass prompt))

(defun kubed-transient-read-service-and-port (prompt _initial-input _history)
  "Prompt with PROMPT for Kubernetes service and port number."
  (let ((service (kubed-read-service prompt)))
    (concat service ":" (number-to-string (read-number "Port number: ")))))

(defun kubed-transient-read-resource-definition-file-name
    (_prompt _initial-input _history)
  "Read and return Kubernetes resource definition file name."
  (kubed-read-resource-definition-file-name))

;;;###autoload (autoload 'kubed-transient "kubed-transient" nil t)
(transient-define-prefix kubed-transient ()
  "Perform Kubernetes operation."
  ["Kubernetes"
   [ :pad-keys t
     ("RET" "Display" kubed-transient-display)
     ("+" "Create"  kubed-transient-create)
     ("*" "Apply"   kubed-transient-apply)
     ("D" "Delete"  kubed-transient-delete)]
   [("e" "Edit"    kubed-transient-edit)
    ("r" "Run"     kubed-transient-run)
    ("a" "Attach"  kubed-transient-attach)
    ("X" "Exec"    kubed-transient-exec)]
   [("l" "Logs"    kubed-transient-logs)
    ("d" "Diff"    kubed-transient-diff)
    ("P" "Patch"   kubed-transient-patch)
    ("R" "Rollout" kubed-transient-rollout)]
   [("$" "Scale" kubed-transient-scale-deployment)
    (":" "Proxy" kubed-transient-proxy)
    ("E" "Explain" kubed-explain)
    ("!" "Command line" kubed-kubectl-command)]])

(defmacro kubed-transient-logs-for-resource (resource &optional plural)
  "Define transient menu for showing logs for Kubernetes RESOURCE.

Optional argument PLURAL is the plural form of RESOURCE.  If nil, it
defaults to \"RESOURCEs\"."
  (let ((name (intern (concat "kubed-transient-logs-for-" resource))))
    `(transient-define-prefix ,name (&optional value)
       ,(format "Show logs for a Kubernetes %s." resource)
       [,(format "Kubernetes Logs for %s\n" (capitalize resource))
        ["Actions"
         ("l" "Show Logs" ,(intern (concat "kubed-logs-for-" resource)))
         ("!" "Command line" kubed-kubectl-command)]
        ["Options"
         ("-n" "Namespace" "--namespace="
          :prompt "Namespace" :reader kubed-transient-read-namespace)
         ("-C" "Context" "--context="
          :prompt "Context" :reader kubed-transient-read-context)
         ("-b" "Limit bytes" "--limit-bytes="
          :prompt "Byte limit: " :reader transient-read-number-N+)
         ("-t" "Limit lines" "--tail="
          :prompt "Line limit: " :reader transient-read-number-N+)
         ("-S" "Since time" "--since-time="
          :prompt "Since time: " :reader kubed-transient-read-date)]
        ["Switches"
         ("-A" "All containers" "--all-containers")
         ("-f" "Stream logs" "--follow")
         ("-P" "Add pod and container" "--prefix")
         ("-T" "Add timestamps" "--timestamps")]]
       (interactive
        (list (kubed-transient-args 'kubed-transient-logs)))
       (transient-setup ',name nil nil
                        :value value
                        :scope '("logs" ,(concat (or plural
                                                     (concat resource "s"))
                                                 "/"))))))

;;;###autoload (autoload 'kubed-transient-logs-for-pod "kubed-transient" nil t)
(kubed-transient-logs-for-resource "pod")

;;;###autoload (autoload 'kubed-transient-logs-for-deployment "kubed-transient" nil t)
(kubed-transient-logs-for-resource "deployment")

;;;###autoload (autoload 'kubed-transient-logs-for-service "kubed-transient" nil t)
(kubed-transient-logs-for-resource "service")

;;;###autoload (autoload 'kubed-transient-logs-for-job "kubed-transient" nil t)
(kubed-transient-logs-for-resource "job")

;;;###autoload (autoload 'kubed-transient-logs-for-replicaset "kubed-transient" nil t)
(kubed-transient-logs-for-resource "replicaset")

;;;###autoload (autoload 'kubed-transient-logs-for-statefulset "kubed-transient" nil t)
(kubed-transient-logs-for-resource "statefulset")

;;;###autoload (autoload 'kubed-transient-logs "kubed-transient" nil t)
(transient-define-prefix kubed-transient-logs ()
  "Show logs for containers running in Kubernetes."
  ["Kubernetes Logs\n"
   ["Kinds"
    ("p" "Pod" kubed-transient-logs-for-pod)
    ("d" "Deployment" kubed-transient-logs-for-deployment)
    ("j" "Job" kubed-transient-logs-for-job)
    ("s" "Service" kubed-transient-logs-for-service)
    ("l" "Any type" kubed-logs)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)
    ("-b" "Limit bytes" "--limit-bytes="
     :prompt "Byte limit: " :reader transient-read-number-N+)
    ("-t" "Limit lines" "--tail="
     :prompt "Byte limit: " :reader transient-read-number-N+)
    ("-S" "Since time" "--since-time="
     :prompt "Since time: " :reader kubed-transient-read-date)]
   ["Switches"
    ("-A" "All containers" "--all-containers")
    ("-f" "Stream logs" "--follow")
    ("-P" "Add pod and container" "--prefix")
    ("-T" "Add timestamps" "--timestamps")]]
  (interactive)
  (transient-setup 'kubed-transient-logs nil nil
                   :scope '("logs")))

;;;###autoload (autoload 'kubed-transient-scale-deployment "kubed-transient" nil t)
(transient-define-prefix kubed-transient-scale-deployment ()
  "Scale Kubernetes deployments."
  ["Kubernetes Scale Deployment\n"
   ["Action"
    ("$" "Scale" kubed-scale-deployment)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)
    ("-r" "Replicas" "--replicas="
     :prompt "Number of replicas: " :reader transient-read-number-N+)]]
  (interactive)
  (transient-setup 'kubed-transient-scale-deployment nil nil
                   :scope '("scale" "deployment")))

;;;###autoload (autoload 'kubed-transient-proxy "kubed-transient" nil t)
(transient-define-prefix kubed-transient-proxy ()
  "Create a proxy between localhost and the Kubernetes API server."
  ["Kubernetes Proxy\n"
   ["Action"
    (":" "Start proxy" kubed-proxy)
    ("|" "Stop proxy" kubed-stop-proxy)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)
    ("-a" "Address" "--address="
     :prompt "Local proxy address: ")
    ("-p" "Port" "--port="
     :prompt "Local proxy port: " :reader transient-read-number-N0)
    ("-w" "WWW directory" "--www="
     :prompt "Local WWW directory: " :reader transient-read-existing-directory)
    ("-P" "WWW prefix" "--www-prefix="
     :prompt "WWW prefix: ")
    ("-A" "API prefix" "--api-prefix="
     :prompt "API prefix: ")]]
  (interactive)
  (transient-setup 'kubed-transient-proxy nil nil
                   :scope '("proxy")))

;;;###autoload (autoload 'kubed-transient-rollout "kubed-transient" nil t)
(transient-define-prefix kubed-transient-rollout ()
  "Manage Kubernetes deployments."
  ["Kubernetes Rollout\n"
   ["Actions"
    ("W" "Watch"   kubed-watch-deployment-status)
    ("R" "Restart" kubed-restart-deployment)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)]]
  (interactive)
  (transient-setup 'kubed-transient-rollout nil nil
                   :scope '("rollout")))

;;;###autoload (autoload 'kubed-transient-attach "kubed-transient" nil t)
(transient-define-prefix kubed-transient-attach ()
  "Attach to running process in container in Kubernetes pod."
  ["Kubernetes Attach\n"
   ["Actions"
    ("a" "Attach" kubed-attach)
    ("!" "Command line" kubed-kubectl-command)]
   ["Switches"
    ("-i" "Open stdin" "--stdin")
    ("-t" "Allocate TTY" "--tty")]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)]]
  (interactive)
  (transient-setup 'kubed-transient-attach nil nil
                   :value '("--stdin" "--tty")
                   :scope '("attach")))

;;;###autoload (autoload 'kubed-transient-diff "kubed-transient" nil t)
(transient-define-prefix kubed-transient-diff ()
  "Display difference between Kubernetes resource definition and current state."
  ["Kubernetes Diff\n"
   ["Actions"
    ("d" "Diff" kubed-diff)
    ("!" "Command line" kubed-kubectl-command)]
   ["Switches"
    ("-M" "Include managed fields" "--show-managed-fields")]
   ["Options"
    ("-f" "Definition file" "--filename="
     :reader kubed-transient-read-resource-definition-file-name)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)]]
  (interactive)
  (transient-setup 'kubed-transient-diff nil nil
                   :scope '("diff")))

;;;###autoload (autoload 'kubed-transient-exec "kubed-transient" nil t)
(transient-define-prefix kubed-transient-exec ()
  "Execute command in Kubernetes pod."
  ["Kubernetes Exec\n"
   ["Actions"
    ("X" "Execute" kubed-exec)
    ("!" "Command line" kubed-kubectl-command)]
   ["Switches"
    ("-i" "Open stdin" "--stdin")
    ("-t" "Allocate TTY" "--tty")]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)
    ("--" "Command" "-- ="
     :prompt "Command: ")]]
  (interactive)
  (transient-setup 'kubed-transient-exec nil nil
                   :value '("--stdin" "--tty")
                   :scope '("exec")))

;;;###autoload (autoload 'kubed-transient-run "kubed-transient" nil t)
(transient-define-prefix kubed-transient-run ()
  "Run container image in a Kubernetes pod."
  ["Kubernetes Run\n"
   ["Actions"
    ("r" "Run" kubed-run)
    ("!" "Command line" kubed-kubectl-command)]
   ["Switches"
    ("-A" "Attach" "--attach")
    ("-i" "Open stdin" "--stdin")
    ("-t" "Allocate TTY" "--tty")
    ("-R" "Remove after exit" "--rm")
    ("-c" "Override container command" "--command")]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)
    ("-I" "Image" "--image="
     :prompt "Image to deploy: ")
    ("-p" "Port" "--port="
     :prompt "Port to expose: " :reader transient-read-number-N+)
    ("-E" "Env vars" "--env="
     :prompt "Set environment VAR=VAL: "
     :multi-value repeat)
    ("--" "Arguments" "-- ="
     :prompt "Arguments for container command: ")]]
  (interactive)
  (transient-setup 'kubed-transient-run nil nil
                   :scope '("run")))

;;;###autoload (autoload 'kubed-transient-apply "kubed-transient" nil t)
(transient-define-prefix kubed-transient-apply ()
  "Apply configuration to Kubernetes resource."
  ["Kubernetes Apply\n"
   ["Actions"
    ("*" "Apply" kubed-apply)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-f" "Definition file" "--filename="
     :reader kubed-transient-read-resource-definition-file-name)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)]]
  (interactive)
  (transient-setup 'kubed-transient-apply nil nil
                   :scope '("apply")))

;;;###autoload (autoload 'kubed-transient-patch "kubed-transient" nil t)
(transient-define-prefix kubed-transient-patch ()
  "Apply patch to Kubernetes resource."
  ["Kubernetes Patch\n"
   ["Actions"
    ("P" "Patch" kubed-patch)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)
    ("-t" "Patch type" "--type="
     :prompt "Patch type: "
     :choices ("strategic" "merge" "json"))]]
  (interactive)
  (transient-setup 'kubed-transient-patch nil nil
                   :scope '("patch")))

;;;###autoload (autoload 'kubed-transient-create "kubed-transient" nil t)
(transient-define-prefix kubed-transient-create ()
  "Create Kubernetes resource."
  ["Kubernetes Create\n"
   ["Kinds"
    ("p" "Pod" kubed-create-pod)
    ("d" "Deployment" kubed-transient-create-deployment)
    ("j" "Job" kubed-transient-create-job)
    ("c" "CronJob" kubed-transient-create-cronjob)
    ("s" "Service" kubed-create-service)]
   ["More"
    :pad-keys t
    ("S" "Secret" kubed-create-secret)
    ("N" "Namespace" kubed-create-namespace)
    ("i" "Ingress" kubed-transient-create-ingress)
    ("+" "Any type" kubed-create)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-f" "Definition file" "--filename="
     :reader kubed-transient-read-resource-definition-file-name)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)]]
  (interactive)
  (transient-setup 'kubed-transient-create nil nil
                   :scope '("create")))

;;;###autoload (autoload 'kubed-transient-display "kubed-transient" nil t)
(transient-define-prefix kubed-transient-display ()
  "Display Kubernetes resource."
  ["Kubernetes Display\n"
   ["Kinds"
    ("p" "Pod" kubed-display-pod)
    ("d" "Deployment" kubed-display-deployment)
    ("j" "Job" kubed-display-job)
    ("c" "CronJob" kubed-display-cronjob)
    ("s" "Service" kubed-display-service)]
   ["More"
    :pad-keys t
    ("S" "Secret" kubed-display-secret)
    ("N" "Namespace" kubed-display-namespace)
    ("i" "Ingress" kubed-display-ingress)
    ("RET" "Any type" kubed-display-resource)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)]]
  (interactive)
  (transient-setup 'kubed-transient-display nil nil
                   :scope '("get")))

;;;###autoload (autoload 'kubed-transient-edit "kubed-transient" nil t)
(transient-define-prefix kubed-transient-edit ()
  "Edit Kubernetes resource."
  ["Kubernetes Edit\n"
   ["Kinds"
    ("p" "Pod" kubed-edit-pod)
    ("d" "Deployment" kubed-edit-deployment)
    ("j" "Job" kubed-edit-job)
    ("c" "CronJob" kubed-edit-cronjob)
    ("s" "Service" kubed-edit-service)]
   ["More"
    :pad-keys t
    ("S" "Secret" kubed-edit-secret)
    ("N" "Namespace" kubed-edit-namespace)
    ("i" "Ingress" kubed-edit-ingress)
    ("RET" "Any type" kubed-edit-resource)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)]]
  (interactive)
  (transient-setup 'kubed-transient-edit nil nil
                   :scope '("edit")))

;;;###autoload (autoload 'kubed-transient-delete "kubed-transient" nil t)
(transient-define-prefix kubed-transient-delete ()
  "Delete Kubernetes resource."
  ["Kubernetes Delete\n"
   ["Kinds"
    ("p" "Pod" kubed-delete-pods)
    ("d" "Deployment" kubed-delete-deployments)
    ("j" "Job" kubed-delete-jobs)
    ("c" "CronJob" kubed-delete-cronjobs)
    ("s" "Service" kubed-delete-services)]
   ["More"
    :pad-keys t
    ("S" "Secret" kubed-delete-secrets)
    ("N" "Namespace" kubed-delete-namespaces)
    ("i" "Ingress" kubed-delete-ingresses)
    ("D" "Any type" kubed-delete-resources)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)]]
  (interactive)
  (transient-setup 'kubed-transient-delete nil nil
                   :scope '("delete")))

;;;###autoload (autoload 'kubed-transient-create-cronjob "kubed-transient" nil t)
(transient-define-prefix kubed-transient-create-cronjob ()
  "Create Kubernetes cronjob."
  ["Kubernetes Create CronJob\n"
   ["Actions"
    ("+" "Create" kubed-create-cronjob)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)
    ("-I" "Image" "--image="
     :prompt "Image to run: ")
    ("-S" "Schedule" "--schedule="
     :prompt "Cron schedule: ")
    ("--" "Command" "-- ="
     :prompt "Command: ")]]
  (interactive)
  (transient-setup 'kubed-transient-create-cronjob nil nil
                   :scope '("create" "cronjob")))

;;;###autoload (autoload 'kubed-transient-create-ingress "kubed-transient" nil t)
(transient-define-prefix kubed-transient-create-ingress ()
  "Create Kubernetes ingress."
  ["Kubernetes Create Ingress\n"
   ["Actions"
    ("+" "Create" kubed-create-ingress)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)
    ("-c" "Class" "--class="
     :prompt "Class" :reader kubed-transient-read-ingressclass)
    ("-d" "Default backend service" "--default-backend="
     :prompt "Default backend service"
     :reader kubed-transient-read-service-and-port)
    ("-a" "Annotation" "--annotation="
     :prompt "Ingress annotations: "
     :multi-value repeat)
    ("-r" "Rule" "--rule="
     :prompt "Ingress rule: ")]]
  (interactive)
  (transient-setup 'kubed-transient-create-ingress nil nil
                   :scope '("create" "ingress")))

;;;###autoload (autoload 'kubed-transient-create-deployment "kubed-transient" nil t)
(transient-define-prefix kubed-transient-create-deployment ()
  "Create Kubernetes deployment."
  ["Kubernetes Create Deployment\n"
   ["Actions"
    ("+" "Create" kubed-create-deployment)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)
    ("-r" "Replicas" "--replicas="
     :prompt "Number of replicas: " :reader transient-read-number-N+)
    ("-I" "Image" "--image="
     :prompt "Images to deploy: "
     :multi-value repeat)
    ("-p" "Port" "--port="
     :prompt "Port to expose: " :reader transient-read-number-N+)
    ("--" "Command" "-- ="
     :prompt "Command: ")]]
  (interactive)
  (transient-setup 'kubed-transient-create-deployment nil nil
                   :scope '("create" "deployment")))

;;;###autoload (autoload 'kubed-transient-create-job "kubed-transient" nil t)
(transient-define-prefix kubed-transient-create-job ()
  "Create Kubernetes job."
  ["Kubernetes Create Job\n"
   ["Actions"
    ("+" "Create" kubed-create-job)
    ("c" "Create from cronjob" kubed-create-job-from-cronjob)
    ("!" "Command line" kubed-kubectl-command)]
   ["Options"
    ("-n" "Namespace" "--namespace="
     :prompt "Namespace" :reader kubed-transient-read-namespace)
    ("-C" "Context" "--context="
     :prompt "Context" :reader kubed-transient-read-context)
    ("-I" "Image" "--image="
     :prompt "Image to run: ")
    ("--" "Command" "-- ="
     :prompt "Command: ")]]
  (interactive)
  (transient-setup 'kubed-transient-create-job nil nil
                   :scope '("create" "job")))

;;;###autoload (autoload 'kubed-list-transient "kubed-transient" nil t)
(transient-define-prefix kubed-list-transient ()
  "Help for Kubernetes resource list buffers."
  ;; TODO: Add a type-specific group with `:setup-children'.
  ["Kubernetes Resources:"
   ["Select"
    :pad-keys t
    ("RET" "Select" kubed-list-select-resource)
    ("C-o" "Display" kubed-list-display-resource :transient t)
    ("e" "Edit" kubed-list-edit :transient t)
    ("w" "Copy name" kubed-list-copy-as-kill :transient t)
    ("!" "Command line" kubed-list-kubectl-command :transient t)]
   ["Delete"
    ("D" "Delete" kubed-list-delete :transient t)
    ("d" "Mark" kubed-list-mark-for-deletion :transient t)
    ("u" "Unmark" kubed-list-unmark :transient t)
    ("x" "Delete marked" kubed-list-delete-marked :transient t)
    ("+" "Create" kubed-list-create :transient t)]
   ["Table"
    ("|" "Fit column" kubed-list-fit-column-width-to-content :transient t)
    ("}" "Widen column" tabulated-list-widen-current-column :transient t)
    ("{" "Narrow column" tabulated-list-narrow-current-column :transient t)
    ("S" "Sort" tabulated-list-sort :transient t)
    ("/" "Filter" kubed-list-set-filter :transient t)]
   ["Movement"
    :pad-keys t
    ("n" "Next line" next-line :transient t)
    ("p" "Previous line" previous-line :transient t)
    ("TAB" "Next column" kubed-list-next-column :transient t)
    ("S-TAB" "Previous column" kubed-list-previous-column :transient t)
    ("g" "Update" kubed-list-update :transient t)]]
  ["Type Specific"
   :class transient-columns
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'kubed-list-transient
                               kubed-list-transient-extra-suffixes))])

(provide 'kubed-transient)
;;; kubed-transient.el ends here
