;;; esn-which.el --- Determines what programs are active
;;
;; Filename: esn-which.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Sat Nov 13 13:04:31 2010 (-0600)
;; Version:
;; Last-Updated: Mon May  2 15:00:24 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 86
;; URL:
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
;; 21-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Jan 18 13:29:48 2011 (-0600) #83 (us041375)
;;    Made an optional check for PLT tools executables to be present.
;; 07-Dec-2010    Matthew L. Fidler
;;    Last-Updated: Tue Dec  7 11:17:03 2010 (-0600) #33 (Matthew L. Fidler)
;;    Don't require files.  Let autoloads do that.
;; 07-Dec-2010    Matthew L. Fidler
;;    Last-Updated: Tue Dec  7 10:55:25 2010 (-0600) #31 (Matthew L. Fidler)
;;    Bugfix for Pirana.  Now doesn't detect pirana when file is blank.
;; 30-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Tue Nov 30 08:24:37 2010 (-0600) #24 (Matthew L. Fidler)
;;    When checking for which things are used, require the appropriate libraries.
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

(declare-function esn-xpose-run-number "esn-xpose")

;;;###autoload
(defun esn-use-plt-archive-p (&optional file-name)
  "A function saying if this is a plt tools archive or not.

FILE-NAME is an optional argument to check if the supplied file
is in the PLT tools archive or not.
"
  (save-match-data
    (let (
          (ret nil)
          (is-work (or file-name (buffer-file-name)))
          )
      (if (not is-work)
          (setq ret nil)
        (when (string-match "[/\\\\][^/\\\\]+$" is-work)
          (setq is-work (replace-match "" nil nil is-work)))
        (when (string-match "[/\\\\]\\([^/\\\\]+\\)[/\\\\]\\([^/\\\\]+\\)$" is-work)
          (setq is-work (downcase
                         (concat (match-string 1 is-work)
                                 "/"
                                 (match-string 2 is-work)))))
        (setq is-work (string-match "textfiles/control" is-work))
        (setq ret (and esn-mode-use-plt is-work))
        (setq ret (and ret (and esn-exec-plt (file-exists-p esn-exec-plt)))))
      (when ret
        (require 'esn-plt))
      (symbol-value 'ret))))

;;;###autoload
(defun esn-use-xmind-p ()
  "Determines if you may use Xmind"
  (and esn-mode-use-xmind (esn-xmind-components-present)))

(defun esn-xmind-components-present ()
  "Determines if Zip and Unzip are present"
  (if esn-use-xmind-saved
      esn-use-xmind-save
    (setq esn-use-xmind-saved 't)
    (setq esn-use-xmind-save
          (or
           (and (executable-find "zip") (executable-find "unzip"))
           (or (executable-find "7z")
               (and esn-w32 (file-exists-p (concat (getenv "PROGRAMFILES") "\\7-zip\\7z.exe"))))))
    (when esn-use-xmind-save
      (require 'esn-xmind))
    esn-use-xmind-save))

;;;###autoload
(defun esn-use-psn-p ()
  "Use Perl Speaks NONMEM?"
  (if esn-mode-psn-check-install
      (symbol-value 'esn-mode-psn-install-val)
    (let (
          (ver (esn-command-to-string "execute --version"))
          (ret nil))
      (when ver
        (setq ret (string-match "PsN" ver))
        (setq esn-mode-psn-install-val ret))
      (when ret
        (require 'esn-psn))
      (symbol-value 'ret))))

(defgroup esn-census nil
  "* Esn Census options"
  :group 'esn-mode)

(defcustom esn-mode-use-census nil
  "* Defines if Census assumptions for control streams is used
   (different header)"
  :type 'boolean
  :group 'esn-census)

(defcustom esn-census-exe
  (if esn-w32
      (progn
        (if (file-exists-p
             "c:\\Program Files\\Census\\Census.exe")
            "c:\\Program Files\\Census\\Census.exe"
          (if (file-exists-p "c:\\Census\\Census.exe")
              "c:\\Census\\Census.exe"
            ""))))
  "The string that defines the location of PLT Tools"
  :type 'file
  :group 'esn-census
  :group 'esn-exec)

;;;###autoload
(defun esn-use-census-p (&rest ignore)
  "Figures out if this directory is under Census control"
  (if esn-use-census-saved
      esn-use-census-save
    (setq esn-use-census-saved 't)
    (setq esn-use-census-save
          (if (and esn-census-exe (not (string= "" esn-census-exe)) (file-exists-p esn-census-exe))
              (if (and
                   (not (esn-use-plt-p))
                   (not (esn-use-pdx-p))
                   (not (esn-use-plt-p)))
                  (if esn-mode-use-census
                      't
                    nil)
                nil)
            nil))
    (when esn-use-census-save
      (require 'esn-census))
    esn-use-census-save))


;;;###autoload
(defun esn-use-wfn-p ()
  "Returns if Wings for NONMEM is installed and setup for use with EsN."
  (let ((ret (and esn-mode-wfn-bat (not (string= "" esn-mode-wfn-bat)) (file-exists-p esn-mode-wfn-bat))))
    (when ret
      (require 'esn-wfn))
    (symbol-value 'ret)))


;;;###autoload
(defun esn-use-pdx-p ()
  "* Determines if we should use PDx-Pop"
  (if esn-force-pdx
      esn-force-pdx
    (if esn-force-no-pdx
        (not esn-force-no-pdx)
      (if esn-use-pdx-saved
          esn-use-pdx-save
        (let (
              (ret nil)
              (is-ctl (buffer-file-name))
              )
          (if (not is-ctl)
              (setq ret nil)
            (setq is-ctl (string-match "\\.ctl$" is-ctl))
            (setq ret (and esn-mode-use-pdx is-ctl))
            (setq ret (and ret (and esn-PDxPop-exe (file-exists-p esn-PDxPop-exe))))
            (setq ret (and ret (not (esn-use-plt-p))))
            )
          (setq esn-use-pdx-saved 't)
          (setq esn-use-pdx-save ret)
          (when ret
            (require 'esn-pdx))
          (symbol-value 'ret))))))


;;;###autoload
(defun esn-use-plt-p ()
  "* A function saying if one should use PLT tools or not."
  (if esn-force-plt
      esn-force-plt
    (if esn-force-no-plt
        (not esn-force-no-plt)
      (if esn-use-plt-saved
          esn-use-plt-save
        (let (
              (ret nil)
              (is-work (buffer-file-name))
              (case-fold-search 't)
              )
          (if (not is-work)
              (setq ret nil)
            (when (string-match "[/\\\\][^/\\\\]+$" is-work)
              (setq is-work (replace-match "" nil nil is-work)))
            (when (string-match "[/\\\\]\\([^/\\\\]+\\)$" is-work)
              (setq is-work (downcase (match-string 1 is-work))))
            (setq is-work (string-match "work" is-work))
            (setq ret (and esn-mode-use-plt is-work))
            (setq ret (and ret (or (not esn-exec-plt-check)
				   (and esn-exec-plt esn-exec-plt-check (file-exists-p esn-exec-plt))))))
          (setq esn-use-plt-saved 't)
          (setq esn-use-plt-save ret)
          (when ret
            (require 'esn-plt))
          (symbol-value 'ret))))))


(defvar esn-use-pirana-saved nil)
(defvar esn-use-pirana-save nil)
(make-variable-buffer-local 'esn-use-pirana-saved)
(make-variable-buffer-local 'esn-use-pirana-save)

;;;###autoload
(defun esn-use-pirana-p ()
  "Figures out if this directory is under Pirana control"
  (if esn-force-pirana
      esn-force-pirana
    (if esn-force-no-pirana
        nil
      (if esn-use-pirana-saved
          esn-use-pirana-save
        (let ((fn (buffer-file-name))
              ret)
          (if (and esn-pirana-exe (not (string= "" esn-pirana-exe))
                   (file-exists-p esn-pirana-exe))
              (if (and
                   (not (esn-use-plt-p))
                   (not (esn-use-pdx-p)))
                  (progn
                    (if (and fn
                             (string-match "\\.[mM][oO][dD]$" fn)
                             esn-mode-use-pirana)
                        (setq ret (file-exists-p "./pirana.dir"))
                      )
                    (setq esn-use-pirana-saved 't)
                    (setq esn-use-pirana-save 't)
                    )
                (setq ret nil))
            (setq ret nil))
          (when ret
            (require 'esn-pirana))
          (symbol-value 'ret))))))

;;;###autoload
(defun esn-use-nmqual-p ()
  "* Check to see if nmqual is installed"
  (if esn-exec-nmqual
      (if (string= "" esn-exec-nmqual)
          nil
        (if (file-exists-p esn-exec-nmqual)
            esn-exec-nmqual
          nil))))

;;;###autoload
(defun esn-use-xpose-p ()
  "Determines if EsN uses Xpose"
  (if esn-use-xpose-saved
      esn-use-xpose-save
    (setq esn-use-xpose-saved 't)
    (setq esn-use-xpose-save
          (let ((ret (and esn-xpose
                          (progn (require 'esn-xpose) (featurep 'esn-xpose))
                          (esn-xpose-run-number))))
            ;; Cannot use PLT tools and Xpose at the same time...
            (setq ret (and ret (not (esn-use-plt-p))))
            (symbol-value 'ret)))
    (when esn-use-xpose-save
      (require 'esn-xpose))
    esn-use-xpose-save))

(provide 'esn-which)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-which.el ends here
