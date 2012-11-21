;;; esn-remote.el --- Connects to a remote server
;; 
;; Filename: esn-remote.el
;; Description: Connects to a remote server
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Tue Nov 20 11:44:05 2012 (-0600)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL:
;; Keywords:
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defun esn-is-remote-p ()
  "Tests if the current buffer is a remote file."
  (and (boundp 'tramp-file-name-regexp)
       (buffer-file-name)
       (eq (save-match-data
             (string-match tramp-file-name-regexp (buffer-file-name))) 0)))

(defvar esn-remote-shell-pattern "^[^#$%>\n]*\\(\\]\\|[#$%>]\\) *"
  "Remote shell pattern")

(defun esn-remote-ready-p (buffer)
  "Determines if the remote shell is ready or not."
  (interactive (list (current-buffer)))
  (let (ret)
    (save-excursion
      (save-match-data
        (set-buffer buffer)
        (goto-char (point-max))
        (when (re-search-backward esn-remote-shell-pattern nil t)
          (goto-char (match-end 0))
          (skip-chars-forward " \t\n")
          (setq ret (= (point-max) (point))))))
    (when (interactive-p)
      (message "Ready: `%s'" ret))
    (symbol-value 'ret)))

(defvar esn-remote-command-lst '()
  "List of remote commands to process")

(defvar esn-remote-command-timer nil)

(defun esn-remote-command (&optional command buffer)
  "Send Remote command.  If the process is not ready, wait until it is.  This is determined by `esn-remote-ready-p'"
  (let ((buf nil)
        (cmd nil))
    (if command
        (progn
          (setq cmd command)
          (setq buf (or buffer (esn-attach-remote-ssh))))
      (setq cmd (car (reverse esn-remote-command-lst)))
      (setq buf (nth 1 cmd))
      (setq cmd (nth 0 cmd)))
    (if (not (buffer-live-p buf))
        (when (not command)
          (setq esn-remote-command-lst (reverse (cdr (reverse esn-remote-command-lst)))))
      (if (esn-remote-ready-p buf)
          (save-excursion
            (set-buffer buf)
            (goto-char (point-max))
            (insert cmd)
            (comint-send-input)
            (when (not command)
              (setq esn-remote-command-lst (reverse (cdr (reverse esn-remote-command-lst))))))
        (when (and command buf)
          (add-to-list 'esn-remote-command-lst `(,command ,buf)))))
    (if (not esn-remote-command-lst)
        (when esn-remote-command-timer
          (cancel-timer esn-remote-command-timer))
      (setq esn-remote-command-timer (run-with-timer 1 nil 'esn-remote-command)))))

(defun esn-attach-remote-ssh ()
  "Attaches a remote session"
  (let ((ret nil))
    (when (esn-is-remote-p)
      (unless (featurep 'ssh)
        (require 'ssh nil t))
      (if (not (featurep 'ssh))
          (message "Cannot connect a remote ssh to the current buffer since the ssh package is not loaded.")
        (let ((td (tramp-dissect-file-name (buffer-file-name)))
              method user host localname)
          (setq localname (tramp-file-name-localname td))
          (setq method (tramp-file-name-method td))
          (setq user (tramp-file-name-user td))
          (setq host (tramp-file-name-host td))
          (cond
           ((string= method "plinkx")
            ;; Assumes plink = ssh
            (setq ret (get-buffer (concat "*" host ":" (file-name-directory localname) "*")))
            (unless ret
              (let ((default-directory esn-path))
                (setq ret (get-buffer-create  (concat "*" host ":" (file-name-directory localname) "*")))
                ;; So that ssh doesn't try to use things from TRAMP.
                (ssh (concat host) (concat "*" host ":" (file-name-directory localname) "*")))
              (message "%s" ret)
              (esn-remote-command (format "cd %s" (shell-quote-argument (file-name-directory localname))) ret)))))))
    (symbol-value 'ret)))

(provide 'esn-remote)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-remote.el ends here
