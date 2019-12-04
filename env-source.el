;;; env-source.el --- Environment Variables in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Version: 0.1
;; Package-Requires: none
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Source shell files and import variables into the Emacs environment.

;; TODO: Check for timestamp on `env-sources' and do `env-source-save' if they are
;; newer than `var/env.el'.


;;; Code:

(require 'diff)


;;;; Variables

(defgroup env-source-file nil
  "Source shell files and import variables into the Emacs environment."
  :prefix "env-source-file"
  :group 'environment)

(defcustom env-sources '("~/.env")
  "Shell files to source.

This is used to import shell environment variables into the Emacs session."
  :type '(repeat (string :tag "Shell file"))
  :group 'env-source-file)

(defcustom env-source-vars '("USER" "TEMPDIR" "SSH_AUTH_SOCK" "SHELL"
                             "PKG_CONFIG_PATH" "PATH" "MANPATH" "LC_MESSAGES"
                             "LC_CTYPE" "LC_COLLATE" "LANG" "GOPATH"
                             "BOOT_JVM_OPTIONS")
  "Variables to save."
  :type '(repeat (string :tag "Environment variable"))
  :group 'env-source-file)

(defvar env-source-cache-directory (expand-file-name "var" user-emacs-directory)
  "Directory used to store `env-source-cache-file'.")

(defvar env-source-cache-file (expand-file-name "env.el" env-source-cache-directory)
  "File used to store saved environment variables and their values.")

(defvar env-source-re "declare -x \\([^=\n]+\\)=?\\(.*\\)$"
  "Regular expression used to capture environment variables.")



;;;; Functions

(defun env-source-command-diff (command)
  "Run COMMAND using SHELL and return changed environment variables."
  (let ((old (get-buffer-create "*env-source-command-diff-old*"))
        (new (get-buffer-create "*env-source-command-diff-new*"))
        (diff (get-buffer-create "*env-source-command-diff-diff*")))
    (shell-command "export" old)
    (shell-command (concat command "; export") new)
    (switch-to-buffer (diff-no-select old new nil t diff))
    (let (vars)
      ;; Update environment variables
      (goto-char (point-min))
      (while (search-forward-regexp (concat "^+" env-source-re) nil t)
        (let ((var (match-string 1))
              (value (read (match-string 2))))
          (push (cons var value) vars)))
      ;; Remove environment variables
      (while (search-forward-regexp (concat "^-" env-source-re) nil t)
        (let ((var (match-string 1)))
          (push (cons var nil) vars)))
      (kill-buffer old)
      (kill-buffer new)
      (kill-buffer diff)
      vars)))

(defun env-source-command (command)
  "Run COMMAND using SHELL and return the environment variables."
  (with-temp-buffer
    (shell-command (format "eval '%s'; export" command) t)
    (goto-char (point-min))
    (let (vars)
      (while (search-forward-regexp env-source-re nil t)
        (let ((var (match-string 1))
              (val (match-string 2)))
          (when (member var env-source-vars)
            (push (cons var (and val (read val))) vars))))
      vars)))

(defun env-set-vars (vars)
  "Set environment variables per VARS.

VARS should be an alist where CAR is an environment variable
name (a string) and CDR is its value (also a string).

The environment variable PATH is mapped to the variable
`exec-path'."
  (cl-loop for (var . val) in vars
           do (setenv var val)
           when (and val (string= var "PATH"))
           do (and (setq exec-path (split-string val path-separator))
                   (bound-and-true-p eshell-path-env)
                   (setq eshell-path-env val)))
  vars)

(defun env-source-file (filename)
  "Use the users shell to source FILENAME.

Import any updated environment variables into the Emacs session."
  (interactive "fSource file: ")
  (env-set-vars (env-source-command (concat ". " filename))))

(defun env-source-save ()
  "Source shell files and save the result in a file."
  (interactive)
  (with-temp-file env-source-cache-file
    (prin1 (env-set-vars (mapcan #'env-source-file env-sources)) (current-buffer))))

(defun env-source-load (&optional cache)
  "Load environment variables from a file, possibly using a CACHE.

Then import them into the Emacs session.

If CACHE is non-nil, don't try to load from the cache file."
  (if (and cache
           (file-exists-p env-source-cache-file)
           (let ((modified (file-attribute-modification-time
                            (file-attributes env-source-cache-file))))
             (cl-some (lambda (file)
                        (time-less-p (file-attribute-modification-time
                                      (file-attributes file))
                                     modified))
                      env-sources)))
      (with-temp-buffer
        (insert-file-contents env-source-cache-file)
        (env-set-vars (read (current-buffer))))
    (env-source-save)))

(defun env-source-add-to-path (&rest paths)
  "Add PATHS to the OS and Emacs executable search paths."
  (let* ((old-path (reverse (split-string (getenv "PATH") path-separator)))
         new-path)
    (dolist (path (append paths old-path))
      (setq path (expand-file-name path))
      (when (file-directory-p path) (cl-pushnew path new-path)))
    (setenv "PATH" (mapconcat #'identity new-path path-separator))
    (setq exec-path new-path)))


(provide 'env-source-file)

;;; env-source.el ends here
