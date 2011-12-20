;;; esn-options-header-census.el --- Census Header Options
;;
;; Filename: esn-options-header-census.el
;; Description: Census Header Options
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Wed Jan 27 12:00:35 2010 (-0600)
;; Version: 0.1
;; Last-Updated: Wed Apr 27 18:59:19 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 8
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM
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



(defcustom esn-update-name-begin "File:"
  "* Defines the text used to begin the program name."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-name-end "----"
  "* Defines the text used to begin the program name."
  :type 'string
  :group 'esn-mode-update-file)

(defcustom esn-update-complete-date-time 't
  "* Update the last save date with the date and time instead of just the date."
  :type 'boolean
  :group 'esn-mode-update-file)

(defcustom esn-update-complete-date 't
  "* Updates the date that the file was saved.  Assumes the file has not been validated."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-save-date-before "Completed"
  "* Defines what text indicates the day that the program was saved. This assumes that the file has not been validated."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-save-date-after "--"
  "* Defines what text ends the day that the program was saved. This assumes that the file has not been validated."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-purpose-before "Purpose:"
  "* Defines what text indicates the program's purpose."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-purpose-after "----"
  "* Defines what text should come on the line after the program's purpose.  The initial ; and spaces are ignored."
  :type 'string
  :group 'esn-mode-update-file
  )

(defcustom esn-update-input-files 't
  "* If not nil, then esn-mode will update the input file name list when saving."
  :type 'boolean
  :group 'esn-mode-update-file
  )
(defcustom esn-update-input-files-begin "Input Files:"
  "* Defines the text where the input file list begins."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-input-files-end "----"
  "* Defines the text where the input file list ends.  Excludes spaces and ; and newline."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-author-begin "Author:"
  "* Defines the text where the authorship list begins."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-author-end "Date Completed:"
  "* Defines the text where the authoriship list ends."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-authorship nil
  "* Defines if when updating the header, authorship should be updated."
  :type 'boolean
  :group 'esn-mode-update-file
  )
(defcustom esn-update-authorship-always-add-at-end 't
  "* Defines if the authorship should always be added at the end."
  :type 'boolean
  :group 'esn-mode-update-file)

(defcustom esn-update-authorship-name-and-user nil
  "* If true, authorship is updated based on Name (user), otherwise just Name."
  :type 'boolean
  :group 'esn-mode-update-file
  )
(defcustom esn-update-copyright 't
  "* Defines if the copyright will be updated."
  :type 'boolean
  :group 'esn-mode-update-file
  )
(defcustom esn-update-copyright-begin "(c)"
  "* Defines the copyright statement search text."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-copyright-end "Completed"
  "* Defines the copyright statment search end text."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-copyright-company ""
  "* Defines the copyright company."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-copyright-with-authorship 't
  "Defines if authorship is updated at the copyright line.  Example

(c) 1999,2000,2001 John Doe, Jane Doe, Institution

Otherwise

(c) 1999,2000,2001 Institution

"
 :type 'boolean
 :group 'esn-mode-update-file)


(defcustom esn-update-input-files-line "\n;;    * %s"
  "* Defines what each input file should be listed as.  %s represents the file name."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-output-files 't
  "* If not nil, then esn-mode will update the output file name list when saving."
  :type 'boolean
  :group 'esn-mode-update-file
  )
(defcustom esn-update-output-files-begin "Output Files:"
  "* Defines the text where the output file list begins."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-output-files-end "----"
  "* Defines the text where the output file list ends.  Excludes spaces and ; and newline."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-output-files-line "\n;;    * %s"

  "* Defines what each output file should be listed as.  %s represents the file name."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-file-name-long 't
  "* If not nil, then esn-mode will update the long file name when saving (in header)."
  :type 'boolean
  :group 'esn-mode-update-file
  )

(defcustom esn-update-file-name-short nil
  "* If not nil, then esn-mode will update the short file name (in problem statment) when saving."
  :type 'boolean
  :group 'esn-mode-update-file
  )
(defcustom esn-update-purpose 't
  "* If not nil, then esn-mode will update the purpose by prompting."
  :type 'boolean
  :group 'esn-mode-update-file
  )
(defcustom esn-update-purpose-cp 't
  "* If not nil, then esn-mode will update the purpose to be different on copies."
  :type 'boolean
  :group 'esn-mode-update-file
  )
(defcustom esn-update-redo-header-if-cp 't
  "* If not nil, then esn-mode will delete old header, and create a new header  when the file was copied."
  :type 'boolean
  :group 'esn-mode-update-file
  )
(defcustom esn-update-modification-line "\n;;    * %s"
  "* Defines what each modificaiton line should be listed as.  %s represents the file name."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-modification-dt-every-time nil
  "* Defines if when updating modification log the user's line will be updated with the time saved instead of the time that the modification line was added."
  :type 'boolean
  :group 'esn-mode-update-file)
(defcustom esn-update-modification-begin "Modification Log:"
  "* Defines the text where the modification log begins."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-modification-end "----"
  "* Defines the text where the modification log ends."
  :type 'string
  :group 'esn-mode-update-file
  )

(defcustom esn-update-modification-log-date-time 't
  "* When true, the Initial date/time of modification will be used instead of just the date."
  :type 'boolean
  :group 'esn-mode-update-file
  )
(defcustom esn-update-modification-add-reason 't
  "*When true, new modification lines require a reason."
  :type 'boolean
  :group 'esn-mode-update-file
  )
(defcustom esn-update-modification-type 3
  "* Defines the type of Modification log that is done."
  :type '(choice
	  (const
	   :tag "Add a modification line for each distinct user."
	   1)
	  (const
	   :tag "Add a modification line for each user and for the same user if the last modification was not on the same date."
	   2)
	  (const
	   :tag "Add a modification line if the last user wasn't the current user."
	   3)
	  (const
	   :tag "Add a modification line if the last user wasn't the current user, or the last modification was not on the same day."
	   4)
	  )
  :group 'esn-mode-update-file
  )
(defcustom esn-update-modified-by 't
  "* If not nil, then esn-mode will update who modified the document last."
  :type 'boolean
  :group 'esn-mode-update-file
  )
(defcustom esn-date-format "%a %b %d"
  "Format for the standard date."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-date-time-format "%a %b %d %T %Z %Y"
  "Format for the standard date/time."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-header "$PROBLEM $small-name$
;;;;C Parent=
;; -----------------------------------------------------------------------------
;; File:
;; -----------------------------------------------------------------------------
;; Compound:
;; Formulation:
;; Route:
;; Purpose:
;; -----------------------------------------------------------------------------
;; (c)
;; Completed
;; -----------------------------------------------------------------------------
;; Input Files:
;; -----------------------------------------------------------------------------
;; Output Files:
;; -----------------------------------------------------------------------------
;; Modification Log:
;; -----------------------------------------------------------------------------
$after-header$"
  "* This option defines the initial header placed into the file. $create-information$ defines the creation information, $after-header$ defines what goes after the header $small-name$ defines the small name for the file."
  :type 'string
  :group 'esn-mode
  )
(defcustom esn-update-version nil
  "* This defines if the NONMEM version will be updated when creating a buffer."
  :type 'string
  :group 'nonmem-mode
  )
(defcustom esn-update-cp-version 't
  "* This defines if the version will be prompted for on a file copy."
  :type 'string
  :group 'nonmem-mode)
(defcustom esn-update-version-begin "Created for NONMEM"
  "* Defines the text where the version information begins."
  :type 'string
  :group 'esn-mode-update-file )
(defcustom esn-update-version-after "----"
  "* Defines the text wher ethe version information ends."
  :type 'string
  :group 'esn-mode-update-file
  )
(defcustom esn-update-creation-date-time 't
  "* When true, the Initial creation string will be date-time
  instead of just date."
  :type 'boolean
  :group 'esn-mode-update-file
  )

(defcustom esn-update-authorship-always-add '( )
  "* Defines the authors that should alwasy be added to the authorship."
    :type '(repeat
	    (string :tag "Author:")
	  )
  :group 'esn-mode-update-file)

(defcustom esn-update-cp-save-terms '(
                                         ("Purpose:" "----" )
                                         ("Route:" "Purpose" )
                                         ("Formulation:" "Route" )
                                         ("Compound:" "Formulation" )
                                         ("Parent= " "----" )
                                         )
  "* Defines the Save terms for the header."
  :type '(repeat
	  (list
	   (string :tag "Before Term:")
	   (string :tag "After Term:")
	   )
	  )
  :group 'esn-mode-update-file
  )
(defcustom esn-update-editable-regions '(
                                         ("Purpose:" "----" )
                                         ("Route:" "Purpose" )
                                         ("Formulation:" "Route" )
                                         ("Compound:" "Formulation" )
                                         ("Parent= " "----" )
                                         )
  "* This defines the editabale regions of the header."
  :type '(repeat
	  (list
	   (string :tag "Text Before Editable:")
	   (string :tag "Text On line after editable:")
	   )
	  )
  :group 'esn-mode-update-file
  )

(defcustom esn-after-problem 't
  "* Defines if the header should be added before or after the $PROBLEM statment."
  :type 'boolean
  :group 'esn-mode-update-file
  )

;; Keep the same for each mode used.  i.e. sticky per mode.
(defcustom esn-mode-use-cov-type-labels 't
  "* Defines if you should use COV labels for off-diagonal elements of a matrix"
  :type 'boolean
  :group 'esn-mode
  )

(defcustom esn-full-numbering nil
  "* Defines if when numbering EsN should use the full expansion, instead of
; 1 -
; THETA(1) -
or
; ETA(1) -
or
; EPS(1) -
"
  :type 'boolean
  :group 'esn-mode
  )
(defcustom esn-number-estimates nil
  "* Defines if EsN will automatically number estimates."
  :type 'boolean
  :group 'esn-mode
  )

(defcustom esn-tos-comments-use-c nil
  "* Defines if when numbering EsN should use the PLT tools ;C comments.  Not available under PDX pop."
  :type 'boolean
  :group 'esn-mode
  )

(defcustom esn-mirror-problem-purpose 't
  "* Defines if the $PROBLEM statment is mirrored by the purpose in the header."
  :type 'boolean
  :group 'esn-mode-update-file
  )

(defcustom esn-problem-return-to-comment nil
  "* Defines if the $PROBLEM statment is mirrored by the purpose in the header."
  :type 'boolean
  :group 'esn-mode-update-file
  )

(provide 'esn-options-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-options-header-census.el ends here
