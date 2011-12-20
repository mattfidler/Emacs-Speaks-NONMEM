;;; esn-toolbar.el --- Support for a toolbar in ESN.
;;
;; Filename: esn-toolbar.el
;; Description: Toolbar support in EsN.
;; Author:      Matthew Fidler.
;; Maintainer:  Matthew L. Fidler
;; Created: Tue Jan 26 15:06:53 2010 (-0600)
;; Version:  0.1
;; Last-Updated: Wed Apr 27 18:58:30 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 159
;; URL: http://esnm.sourceforge.net
;; Keywords: Tool Bar
;; Compatibility: Emacs 23.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;; If you see a toolbar, but no icons, check out the value of
;; esn-icon-directory.
;;
;; The toolbar can be customized in several ways.  To see options, do:
;; M-x customize-group RET esn-toolbar RET
;; If you change any of the variables, you _may_ need to restart Emacs
;; to see any effect.  See also the documentation for esn-toolbar-items
;; if you wish to change its value.
;;
;; Technical issues.
;;
;; Deleted R toolbar code 
;;
;; 2009-03-16: toolbar code in Emacs 23 has changed slightly to 22,
;; and presumably once Emacs 22 is no longer supported, this code can
;; be cleaned up a bit (i.e. no need to set load-path.)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 08-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep  8 11:57:20 2010 (-0500) #129 (Matthew L. Fidler)
;;    Made toolbar only put items that the user can run into it...
;; 22-Apr-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Apr 22 12:39:27 2010 (-0500) #97 (Matthew L. Fidler)
;;    Added icon for switching to NONMEM output.
;; 20-Apr-2010    Matthew L. Fidler
;;    Last-Updated: Tue Apr 20 15:02:02 2010 (-0500) #88 (Matthew L. Fidler)
;;    Added Bootstrapping icon to PLT tools bar under windows.
;; 16-Apr-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jan 28 15:49:04 2010 (-0600) #78 (Matthew L. Fidler)
;;    Added Mayosoft Officina PDF icons for linking to PLT tools PDFs
;; 28-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jan 28 15:48:46 2010 (-0600) #77 (Matthew L. Fidler)
;;    Added Xmind individual Icon.
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 14:37:11 2010 (-0600) #70 (Matthew L. Fidler)
;;    Removed submit via NMFE for PLT tool-bar.
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 16:44:34 2010 (-0600) #54 (Matthew L. Fidler)
;;    Added multiple toolbars under emacs (not xemacs)
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 15:08:32 2010 (-0600) #5 (Matthew L. Fidler)
;;    Add Xmind toolbar.
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
(require 'esn-vars)

(defgroup esn-toolbar nil
  "ESN: toolbar support."
  :group 'esn
  :link '(emacs-commentary-link :tag "Commentary" "esn-toolbar.el")
  :prefix "esn-")

(defcustom esn-use-toolbar
  (if (featurep 'xemacs)
      (memq (device-type) '(x gtk mswindows))
    (and (fboundp 'display-images-p) (display-images-p)))
  "*Non-nil means ESN should support the toolbar.
Currently works only under Emacs 21 and maybe XEmacs 21.4."
  :group 'esn-toolbar
  :type 'boolean)
(defcustom esn-toolbar-own-icons nil
  "*Non-nil means that we only put our toolbar entries in ESN.
Otherwise we get standard toolbar as well as ESN entries.
Under Emacs, the standard toolbar items are copied from the default toolbar.
Under XEmacs, the items stored in `esn-toolbar-xemacs-general' are added."
  :group 'esn-toolbar
  :type 'boolean)
(defvar esn-toolbar-items
  '(
    (esn-switch-rpt "lstout")
    (esn-xmind-project "xmind" esn-use-xmind)
    (esn-xmind-ctl "xmind-single" esn-use-xmind)
    (esn-plt-submit "plt" esn-use-plt)
    (esn-plt-graphics-pdf "plt-graphics" esn-use-plt)
    (esn-plt-summary-pdf "plt-summary" esn-use-plt)
    (esn-plt-results-pdf "plt-results" esn-use-plt)
    (esn-plt-all "plt-all" esn-use-plt)
    (esn-plt-bootstrap "bootstrap" esn-use-plt)
    (esn-nm-submit "nm")
    (esn-nmqual-submit "nmqual" esn-mode-nmqual-installed)
    (esn-wfn-submit "w" esn-mode-wfn-installed)
    (esn-psn-execute "psn" esn-mode-psn-installed)
    (esn-PDxPop-start "PDx-Pop" esn-use-pdx)
    (esn-census-start "census" esn-use-census)
    (esn-pirana-start "pirana" esn-use-pirana)
    )
  "Items to be added to the ESN toolbar for PLT tools.
Each list element has three items:
1. the name of the function to run
2. the icon to be used (without .xpm extension)
3. the function required to add the item to the toolbar.
"
  )


(defun esn-make-toolbar-emacs (map)
  "Make the ESN toolbar under Emacs."
  (setq esn-toolbar
        (if (or esn-toolbar-own-icons (null tool-bar-map))
            (make-sparse-keymap)
          (copy-keymap tool-bar-map)))
  (let ((tool-bar-map esn-toolbar)
        (load-path (list esn-icon-directory)))
    ;; in Emacs 22, icons are found by examining load-path, bound here
    ;; whereas Emacs 23 seems to want them in image-load-path, set at the
    ;; bottom of this file.
    (mapc (lambda (x)
            (if (not (nth 2 x))
                (tool-bar-add-item-from-menu (nth 0 x) (nth 1 x) map)
              (if (functionp (nth 2 x))
                  (if (funcall (nth 2 x))
                      (tool-bar-add-item-from-menu (nth 0 x) (nth 1 x) map))
                (tool-bar-add-item-from-menu (nth 0 x) (nth 1 x) map)
                )))
          esn-toolbar-items)
    (setq esn-toolbar tool-bar-map)
    (setq tool-bar-map esn-toolbar)
    ))


;;;###autoload
(defun esn-add-toolbar (map)
  "Add the ESN toolbar to a particular mode."
  (esn-make-toolbar-emacs map)
  (set (make-local-variable 'tool-bar-map) esn-toolbar))

(defvar esn-icon-directory
  (expand-file-name (concat (file-name-as-directory esn-default-directory) "icons"))
  "*Location for EsN icons.
This variable should be set automatically by the EsN install process.
If `esn-icon-directory' is invalid, please report a bug.")

(when (boundp 'image-load-path)
  (add-to-list 'image-load-path esn-icon-directory))

(provide 'esn-toolbar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-toolbar.el ends here
