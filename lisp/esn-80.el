;;; esn-80.el --- 80 character code highlighting
;; 
;; Filename: esn-80.el
;; Description: 80 character code highlighting
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Tue Dec  7 15:45:21 2010 (-0600)
;; Version: 0.13
;; Last-Updated: Wed Apr 27 18:51:26 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 16
;; URL: http://esnm.sourceforge.net/
;; Keywords:
;; Compatibility: 23.2
;; 
;; Features that might be required by this library:
;;
;;   `esn-options', `esn-options-header-universal', `esn-reg',
;;   `esn-start'.
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

(eval-when-compile
  (require 'esn-start)
  (require 'esn-reg))

;;  These are the functions to show 80 lines.

;; Based loosely on vline.
(eval-when-compile
  (require 'esn-options))

(autoload 'esn-get-variable-names "esn-extended")
(autoload 'esn-fix-numbering "esn-extended")


(defcustom esn-highlight-over nil
  "* If true, highlights the `esn-character-limit'."
  :type 'boolean
  :group 'esn-80)

(defcustom esn-highlight-80-col-even-if-not-over nil
  "* If true, highlights the point where the line would go over `esn-character-limit', even if they dont actually go over that amount.  This is true for everything except the current line."
  :type 'boolean
  :group 'esn-80)

(defcustom esn-80-face
  '(
    (t (:inverse-video t :weight extra-bold)))
  "*A default face over `esn-character-limit' highlighting."
  :type 'face
  :group 'esn-80
  )

(defvar esn-80-last-window-start nil
  "The last position of the window."
  )
(defvar esn-80-last-window-height nil
  "The last window height."
  )
(defvar esn-80-last-line nil
  "Last location of the line."
  )
(defvar esn-80-last-buffer-name nil
  "Last buffer name."
  )

(defvar esn-80-table-size 200
  "Size of 80 highlighting table.")
(defvar esn-80-table (make-vector esn-80-table-size nil)
  "Actual 80 highlihgting table."
  )


;;;###autoload
(defun esn-80-clear ()
  "Clears the 80 line highlighting."
  (interactive)
  (mapc (lambda(x)
          (and x (delete-overlay  x)))
        esn-80-table
        ))

(defun esn-80-get-i (pt)
  "Gets the table number of the point.  PT specifies the point where this is checked."
  (save-excursion
    (goto-char (window-start))
    (let (
          (i 0)
          (ret nil))
      (while (and
              ret
              (< i (1- (window-height)))
              (< i (length esn-80-table))
              (not (eobp)))
        (beginning-of-line)
        (when (= pt (point))
          (setq ret 't))
        (end-of-line)
        (if (not (eobp))
            (forward-line 1))
        (setq i (+ i 1)))
      (unless ret
        (setq i -1))
      (symbol-value 'i))))

;;;###autoload
(defun esn-80-show ()
  "Show 80 character line (useful for NONMEM 5)."
  (interactive)
  (when (or esn-highlight-over esn-highlight-80-col-even-if-not-over)
  (save-excursion
    (let (
          (column esn-character-limit)
          (llen (- (window-width) esn-character-limit))
          (ecl esn-character-limit)
          (line-str "")
          (i 0)
          (p1 nil)
          (p2 nil)
          (len 0)
          (tmp1 "")
          (tmp2 "")
          (tmpi 0)
          (off 0)
          (p3 nil)
          (ovr nil)
          (change '())
          (change-it nil)
          (fixed "")
          (add-record nil)
          (tvar (esn-get-variable-names "THE"))
          (avar (esn-get-variable-names "OME"))
          (svar (esn-get-variable-names "SIG")))
      (setq fixed (buffer-substring (window-start)
                                    (save-excursion
                                      (goto-char (window-start))
                                      (forward-line (window-height))
                                      (end-of-line)
                                      (point))))
      (save-excursion
        (goto-char (window-start))
        (setq add-record nil)
        (when (re-search-backward (eval-when-compile (esn-reg-records)) nil t)
          (setq fixed (concat "$PROBLEM silly\n" (match-string 0) "\n" fixed))
          (setq add-record 't)))
      (with-temp-buffer
        (insert fixed)
        (goto-char (point-min))
        (esn-fix-numbering tvar avar svar)
        (setq fixed (split-string (buffer-substring (save-excursion
                                                      (goto-char (point-min))
                                                      (when add-record
                                                        (forward-line 2)
                                                        (beginning-of-line))
                                                      (point))
                                                    (point-max)) "\n"))
                                        ;(message "%s" (mapconcat (lambda(x) x) fixed "\n")))
      (save-excursion
        (beginning-of-line)
        (setq p1 (point)))
      (when (and ;; Conditions for changes to the current line only.
             (and esn-80-last-window-start (= esn-80-last-window-start (window-start)))
             (and esn-80-last-window-height (= esn-80-last-window-height (window-height)))
             (and esn-80-last-line (= p1 esn-80-last-line))
             (and esn-80-last-buffer-name (string= esn-80-last-buffer-name (buffer-name))))
	     (setq tmpi (esn-80-get-i p1))
        (unless (= -1 tmpi)
          (add-to-list 'change tmpi)))
      (when (and ;; Conditions to only change a few
             (and esn-80-last-window-start (= esn-80-last-window-start (window-start)))
             (and esn-80-last-window-height (= esn-80-last-window-height (window-height)))
             (and esn-80-last-buffer-name (string= esn-80-last-buffer-name (buffer-name))))
        ;;
        (setq tmpi (esn-80-get-i p1))
        (unless (= -1 tmpi )
          (add-to-list  'change tmpi))
        (setq tmpi (esn-80-get-i esn-80-last-line))
        (unless (= -1 tmpi)
          (add-to-list 'change tmpi)))
      ;; Save variables.
      (setq esn-80-last-window-start (window-start))
      (setq esn-80-last-window-height (window-height))
      (setq esn-80-last-line p1)
      (setq esn-80-last-buffer-name (buffer-name))
      
                                        ;       (message "Change: %s" change)
      (goto-char (window-start))
      (while (and
              (< i (1- (window-height)))
              (< i (length esn-80-table))
              (not (eobp)))
        (setq ovr (aref esn-80-table i))
        (setq change-it (= (length change) 0))
        (when (and (not change-it) (memq i change))
          (setq change-it 't))
        (when change-it
          ;; Delete overlay.
          (and (aref esn-80-table i) (delete-overlay (aref esn-80-table i)))
          (save-excursion
            (beginning-of-line)
            (setq p2 (point)))
          (move-to-column column)
          ;; if column over the cursor's column (tab  or wide char).
          (when (> (current-column) column)
            (backward-char))
          (let (
                ;; Consider a newline, tab and wide char.
                (str (make-string (- column (current-column)) ? ))
                (tmp "")
                (char (char-after)))
            ;;      (llen (- (window-width) esn-character-limit))
            ;;      (when esn-wfn-extended
            (setq off 0)
            (save-excursion
              (beginning-of-line)
              (when (looking-at ".*")
                (setq off (- (length (nth i fixed)) (length (match-string 0))))))

            (if (< 0 off)
                (if (> (length str)  off)
                    (setq str (substring str 0 (- 0 off)))
                  (setq str ""))
              (setq str (concat str (make-string (- 0 off) ? ))))
            (setq line-str "")
            (when (> (+ off llen) 0)
              (setq line-str (make-string (+ off llen) ? )))
            (setq line-str (propertize line-str 'face esn-80-face))
            (setq str (concat str line-str))
                                        ;           (setq str (propertize str 'intangible ""))
            (setq len 0)
            (unless ovr
              (setq ovr (make-overlay 0 len))
              (overlay-put ovr 'rear-nonsticky t)
              (aset esn-80-table i ovr))
            ;; Initialize overlay
            (overlay-put ovr 'face nil)
            (overlay-put ovr 'before-string nil)
            (overlay-put ovr 'after-string nil)
            (overlay-put ovr 'invisible nil)
            (overlay-put ovr 'window (selected-window))
            (overlay-put ovr 'intangible "")
            (setq tmp str)
            (setq ecl esn-character-limit)
            (while (and (not (eolp)) (< off 0))
              (forward-char 1)
              (setq off (+ off 1))
              (setq ecl (+ ecl 1))
              (setq tmp (substring tmp 1)))
            (cond
             ((eolp)
              (if (<= (+ off (current-column))  ecl)
                  (progn
                    (move-overlay ovr (point) (point))
                    (unless  (or
                              (not esn-highlight-80-col-even-if-not-over)
                              (= p1 p2)
                              (<= (+ off llen) 0))
                      (overlay-put ovr 'after-string tmp)))
                (move-overlay ovr (+ (point) (- ecl (+ off (current-column))))
                              (point))
                (overlay-put ovr 'face esn-80-face)
                (setq str (substring str 0 (- ecl (+ off (current-column)))))
                (unless  (or
                          (not esn-highlight-80-col-even-if-not-over)
                          (= p1 p2)
                          (<= (+ off llen) 0))
                  (overlay-put ovr 'after-string str))
                (when (= p1 p2)
                  (overlay-put ovr 'intangible nil))))
             ( 't
               (save-excursion
                 (end-of-line)
                 (setq len (- (window-width) (current-column)))
                 (if (>= 0 len)
                     (setq tmp "")
                   (setq tmp (make-string len ? ))
                   (setq tmp (propertize tmp 'face esn-80-face))
                   (setq tmp (propertize tmp 'intangible ""))))
               (move-overlay ovr (- (point) off) (save-excursion (end-of-line) (point)))
               (overlay-put ovr 'face esn-80-face)
               (unless  (or
                         (not esn-highlight-80-col-even-if-not-over)
                         (= p1 p2)
                         (<= (+ off llen) 0))
                 (overlay-put ovr 'after-string tmp))
               (when (= p1 p2)
                 (overlay-put ovr 'intangible nil))))))
        (setq i (1+ i))
        (forward-line)))))))

(add-hook 'esn-always-post-command-hook 'esn-80-show)

(provide 'esn-80)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-80.el ends here
