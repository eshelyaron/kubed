;;; cobra.el --- Complete Cobra command lines   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Keywords: tools

;;; Commentary:

;; Cobra is a popular Golang framework for CLI programs.  This library
;; defines function `cobra-read-command-line', which helps you read a
;; command line for a program that uses Cobra, with completion.
;; Prominent examples of Cobra programs are `kubectl' and `docker'.

;;; Code:

(defvar cobra--cache nil)

(defun cobra-completion-table (executable s p a)
  "Completion table for command lines that invoke EXECUTABLE.

Perform completion action A on string S with predicate P."
  (let ((start 0))
    (while (string-match "[[:space:]=]" s start)
      (setq start (match-end 0)))
    (if (eq a 'metadata)
        `(metadata
          (category . cobra-command-line)
          (affixation-function
           . ,(lambda (cands)
                (let ((max (seq-max
                            (cons 0 (mapcar #'string-width cands)))))
                  (mapcar
                   (lambda (cand)
                     (list
                      cand ""
                      (if-let
                          ((desc (get-text-property
                                  0 'cobra-argument-description
                                  cand)))
                          (concat
                           (make-string (1+ (- max (string-width cand))) ?\s)
                           (propertize desc 'face 'completions-annotations))
                        "")))
                   cands)))))
      (let* ((lines
              (cdr
               (if (string= s (car cobra--cache))
                   ;; Cache hit.
                   cobra--cache
                 (setq
                  cobra--cache
                  (cons s
                        (apply #'process-lines-ignore-status
                               executable "__complete"
                               (let ((args (cdr (split-string-and-unquote s))))
                                 (if (string-suffix-p " " s)
                                     ;; Restore omitted empty argument.
                                     (nconc args '(""))
                                   args))))))))
             (code nil)
             (comps (seq-take-while
                     (lambda (line)
                       (not (and (string-match "^:\\([0-9]+\\)$" line)
                                 (setq code (string-to-number
                                             (match-string 1 line))))))
                     lines)))
        ;; `code' encodes "completion directives", as follows:
        ;; #b000001: An error occurred, ignore completions.
        ;; #b000010: Don't add space after completion.  (TODO.)
        ;; #b000100: Don't fall back to file completion.
        ;; #b001000: Completions are really file extension filters.
        ;; #b010000: Complete directory names.
        ;; #b100000: Preserve completions order.
        (when (and code (zerop (logand 1 code)))
          ;; Error bit in unset, proceed.
          (if (= #b100 (logand #b100 code))
              ;; No file name completion.
              (if (eq (car-safe a) 'boundaries)
                  `(boundaries
                    ,start . ,(and (string-match "[[:space:]=]" (cdr a))
                                   (match-beginning 0)))
                (let ((table
                       (mapcar
                        ;; Annotate completion candidates.
                        (lambda (comp)
                          (pcase (split-string comp "\t" t)
                            (`(,c ,d . ,_)
                             (propertize
                              c 'cobra-argument-description
                              ;; Only keep first sentence.
                              (car (split-string d "\\." t))))
                            (`(,c . ,_)  c)))
                        comps)))
                  (if a (complete-with-action a table (substring s start) p)
                    ;; `try-completion'.
                    (let ((comp (complete-with-action a table (substring s start) p)))
                      (if (stringp comp) (concat (substring s 0 start) comp) comp)))))
            ;; File name completion.
            (setq p
                  (cond
                   ((= #b1000 (logand #b1000 code))
                    ;; `comps' are valid extensions.
                    (lambda (f)
                      (or (file-directory-p f)
                          (when (string-match "\\.[^.]*\\'" f)
                            (member (substring f (1+ (match-beginning 0)))
                                    comps)))))
                   ((= #b10000 (logand #b10000 code))
                    ;; Directory name completion.
                    #'file-directory-p)))
            (if (eq (car-safe a) 'boundaries)
                ;; Find nested boundaries.
                (let* ((suf (cdr a))
                       (bounds (completion-boundaries
                                (substring s start) #'completion-file-name-table p
                                (substring suf 0 (string-match "[[:space:]=]" suf)))))
                  `(boundaries ,(+ (car bounds) start) . ,(cdr bounds)))
              (if a (complete-with-action a #'completion-file-name-table
                                          (substring s start) p)
                (let ((comp (complete-with-action a #'completion-file-name-table
                                                  (substring s start) p)))
                  (if (stringp comp) (concat (substring s 0 start) comp) comp))))))))))

;;;###autoload
(defun cobra-read-command-line (prompt initial &optional hist)
  "Prompt with PROMPT for a command line starting with INITIAL.

Optional argument HIST is the name of the history list variable to use,
if it is nil or omitted, it defaults to `shell-command-history'."
  (let ((exec (car (split-string-and-unquote (car (ensure-list initial))))))
    (minibuffer-with-setup-hook
        (:append (lambda ()
                   (let ((map (make-sparse-keymap)))
                     ;; Ensure SPC is self-inserting.
                     (keymap-unset map "SPC")
                     (use-local-map
                      (make-composed-keymap map (current-local-map))))))
      (completing-read prompt (apply-partially #'cobra-completion-table exec)
                       nil nil initial (or hist 'shell-command-history)))))

(provide 'cobra)
;;; cobra.el ends here
