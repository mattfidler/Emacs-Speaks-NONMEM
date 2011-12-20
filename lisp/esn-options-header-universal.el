;;; esn-options-header-universal.el --- Universal Header Options
;; 
;; Filename: esn-options-header-universal.el
;; Description: Universal Header Options
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. fidler
;; Created: Wed Jan 27 12:03:05 2010 (-0600)
;; Version: 0.1
;; Last-Updated: Wed Apr 27 18:59:16 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 25
;; URL: http://esnm.sourceforge.net
;; Keywords: 
;; Compatibility: Emacs 23.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 09-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Jan 27 12:03:39 2010 (-0600) #3 (Matthew L. Fidler)
;;    Changed Header Portions read-only to false by default.
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



(defcustom esn-show-options 't
  "* Show options for header;  Should be set by the setup script."
  :type 'boolean
  :group 'esn-header-universal
  )
(defcustom esn-vc-auto-commit nil
  "* Automatically commit the files to the repository?"
  :type 'boolean
  :group 'esn-header-universal
  )
(defcustom esn-completing-current-directory ""
  "* Defines how the current directory is defined for files when running the NONMEM script."
  :type 'string
  :group 'esn-header-universal
  )

(defcustom esn-vc-upon-new-modification-log-line 't
  "* Defines if a new version of the model should be created when a new author mod<ified the file."
  :type 'boolean
  :group 'esn-header-universal
  )
(defcustom esn-vc-upon-cp nil
  "* Defines if version control is started when the EsN detects that the file was copied."
  :type 'boolean
  :group 'esn-header-universal)
(defcustom esn-mode-after-header-only-on-new-file nil
  "* Defines if the after-header variable is only added when you create a new control stream."
  :type 'boolean
  :group 'esn-header-universal
  )
(defcustom esn-update-add-header-if-not-present nil
  "* Defines of header is added if it isn't present."
  :type 'boolean
  :group 'esn-header-universal
  )
(defcustom esn-update-add-header-on-create nil
  "* Defines if header is aded upon new file creation."
  :type 'boolean
  :group 'esn-header-universal
  )


(defcustom esn-mode-nm-versions '(
("5" "V Level 1.1" )
("6" "VI Level 1.0" )
("6.2" "VI Level 2.0" )
("7" "VII Level 1.0")
)
  " * This defines the versions that nonmem-mode knows about."
  :type '(repeat
	  (list
	   (string :tag "Major")
	   (string :tag "Major/Minor version description")
	   )
	  )
  :group 'esn-header-universal
  :group 'esn-general
  )


(defcustom esn-hide-uneditable-header-components nil
  "* If non-nil, ESN hides the uneditable compoments of the header."
  :type 'boolean
  :group 'esn-header-universal
  :group 'esn-hidden
  )

(defcustom esn-set-read-only nil
  "* Set the header portion that should not be edited to read only."
  :type 'boolean
  :group 'esn-header-universal
  :group 'esn-hidden
  )
(defcustom esn-hide-the-header 't
  "* Should we hide the header? (except for editable regions)"
  :type 'boolean
  :group 'esn-header-universal
  :group 'esn-hidden
  )

(defcustom esn-update-header-init nil
  "* Defines if the header setup has been initialized."
  :type 'boolean
  :group 'esn-header-universal
  )
(defcustom esn-update-conserved-sequence-length 4
  "* Defines the minimum length of the conserved sequence to assume that the file was copied from another file, and update `esn-last-small-name' to update the tables appropriately."
  :type 'integer
  :group 'esn-header-universal
  :group 'esn-general
  )

(defcustom esn-nm-version ""
  "* This defines the default NONMEM version used when creating a buffer and submitting a file."
  :type 'string
  :group 'esn-header-universal
  :group 'esn-general
  )

(defcustom esn-update-authorship-sort-function `esn-string-lessp
  "* Defines the authorship sorting function.  If nil, then do not sort."
  :type 'sexp
  :group 'esn-header-universal
  )

(defcustom esn-insert-after-header  ""
  " * Defines the string inserted after the header."
  :type 'string
  :group 'esn-header-universal
  :group 'esn-general
  )

(defcustom esn-header-small 't
  "*When true, headers are small"
  :type 'boolean
  :group 'esn-header-universal
  )


(provide 'esn-options-header-universal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-options-header-universal.el ends here
