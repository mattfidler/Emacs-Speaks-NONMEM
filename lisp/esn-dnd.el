;;; esn-dnd.el --- Drag and Drop for Emacs Speaks NONMEM
;;
;; Filename: esn-dnd.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Fri Nov 12 17:09:07 2010 (-0600)
;; Version:
;; Last-Updated:
;;           By:
;;     Update #: 16
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp', `warnings'.
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

(declare-function esn-add-input-line "esn-input")

(defadvice dnd-open-local-file (around esn-import-drag-and-drop activate)
  "* Drag Files to Emacs Speaks NONMEM."
  (unless (esn-drag-and-drop (ad-get-arg 0))
    ad-do-it))

(defadvice dnd-open-file (around esn-import-drag-and-drop activate)
  "* Drag Files to Emacs Speaks NONMEM"
  (unless (esn-drag-and-drop (ad-get-arg 0))
    ad-do-it))

(defun esn-drag-and-drop (uri)
  "* Drag and drop support for Emacs Speaks NONMEM.  Currently supports dragging CSV files"
  (when (eq major-mode 'esn-mode)
    (let ((f (dnd-get-local-file-name uri t)) ret)
      (when (string-match "\\.csv$" f)
        (when (string-match " " f)
          (setq f (concat "\"" f "\"")))
        (insert (concat "$DATA " f "\n\n$INPUT "))
        (esn-add-input-line)
        (setq ret 't))
      (symbol-value 'ret))))


(provide 'esn-dnd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-dnd.el ends here
