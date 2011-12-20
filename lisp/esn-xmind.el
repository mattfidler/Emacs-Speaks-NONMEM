;;; -*-no-byte-compile: t; -*-
;;;; esn-xmind.el --- Emacs Speaks NONMEM's implementation of Xmind Extension 
;;
;; Filename: esn-xmind.el
;; Description: This uses zip and unzip from infozip to write to a Xmind
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. fidler
;; Created: Tue Jan 26 09:16:16 2010 (-0600)
;; Version: 0.1
;; Last-Updated: Fri May 13 16:38:40 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 1460
;; URL: http://esnm.sourceforge.net
;; Keywords: Xmind
;; Compatibility: Gnu Emacs 23.x.  Other versions may be
;;                compatible but are not tested.
;;
;; Features that might be required by this library: 
;;
;;   None
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is part of part of Emacs Speaks NONMEM.  It allows updating of Xmind
;; mind maps by using infozip's zip and unzip OR 7zip, and emacs lisp to update
;; the xml files.  It will either add or update a control stream in a Xmind mind
;; map.  It searches for a parent topic by looking for strings such as:
;;
;; Xmind: Parent Topic
;;
;; Or
;;
;; Reference Model: reference.mod
;;
;; If it doesn't find a parent topic, it uses the main topic as a
;; parent topic.
;;
;;
;; TODO:
;; - Parse XML only when needed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 12-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Nov 12 18:25:11 2010 (-0600) #1416 (Matthew L. Fidler)
;;    Bug fix changing ?> to ?\>
;; 29-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Oct 29 11:26:01 2010 (-0500) #1405 (Matthew L. Fidler)
;;    Small bug fix.
;; 26-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Oct 26 23:33:49 2010 (-0500) #1401 (Matthew L. Fidler)

;;    Added a possible bug fix so that xmind files are created with the buffer
;;    that originally called the save.  Therefore saving and then switching
;;    buffers will not affect the xmind file creation.  Hopefully it works.

;; 26-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Oct 26 22:55:33 2010 (-0500) #1388 (Matthew L. Fidler)
;;    More descriptive errors for missing files in the constructed xmind file.
;; 26-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Oct 26 15:20:10 2010 (-0500) #1334 (Matthew L. Fidler)
;;
;;    Create xmind file without the copying a previous file.  Backup xmind files
;;    on save. For example _project.xmind is backed up to _project.xmind~
;;
;; 06-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Oct  6 11:58:27 2010 (-0500) #1288 (Matthew L. Fidler)
;;    Fix bug in get current id that confuses working directories under PLT tools.
;; 01-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Oct  1 16:46:47 2010 (-0500) #1284 (Matthew L. Fidler)
;;    Allow movement to add topic...?
;; 28-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Sep 28 16:40:41 2010 (-0500) #1243 (Matthew L. Fidler)

;;    When esn-update-get-purpose returns "", just use the $PROBLEM statement
;;    for the purpose.  If that is blank, use the file name.

;; 28-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Sep 28 15:43:16 2010 (-0500) #1221 (Matthew L. Fidler)
;;    Made update Xmind project based on files error resistant. 
;; 17-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Sep 17 10:39:18 2010 (-0500) #1209 (Matthew L. Fidler)
;;    Bug fix for get parent topic.
;; 17-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Sep 17 09:55:03 2010 (-0500) #1201 (Matthew L. Fidler)

;;    Updated Xmind to be created in WORK-xmind directory under PLT tools.  That
;;    way speed issues don't present themselves under PLT (as per Dennis)

;; 02-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Sep  2 11:46:17 2010 (-0500) #1148 (Matthew L. Fidler)
;;    Changed update xmind to create parent tree structure when not present. Also added general Xmind update.
;; 27-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Aug 27 10:18:04 2010 (-0500) #1045 (Matthew L. Fidler)
;;    Fixed warnings and code to be cleaner.
;; 17-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jun 17 16:23:36 2010 (-0500) #1003 (Matthew L. Fidler)
;;    After profiling, esn-xmind-components-present is taking alot of processor time.  Save results after one call.
;; 17-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jun 17 15:47:36 2010 (-0500) #997 (Matthew L. Fidler)
;;    Only update individual xmind map once, if nothing changed that needs updating.
;; 17-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jun 17 15:37:34 2010 (-0500) #988 (Matthew L. Fidler)
;;    Make individual map a temporary xmind checkout.
;; 17-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jun 17 10:56:27 2010(-0500) #942 (Matthew L. Fidler)
;;    Attempt to fix the problem with & > < in the purpose causing Xmind files to become corrupt.
;; 15-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jun 15 15:58:15 2010 (-0500) #930 (Matthew L. Fidler)
;;    Added a check to make sure this is not a PLT archive.
;; 14-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Mon Jun 14 10:41:21 2010 (-0500) #924 (Matthew L. Fidler)
;;    Added support for 7-zip.
;; 14-May-2010    Matthew L. Fidler
;;    Last-Updated: Fri May 14 09:11:18 2010 (-0500) #904 (Matthew L. Fidler)
;;    Bug fixes relating to killing buffers.
;; 04-May-2010    Matthew L. Fidler
;;    Last-Updated: Tue May  4 09:19:51 2010 (-0500) #897 (Matthew L. Fidler)
;;    Added save-restriction to updating Xmind comment.  Some issue with pdx pop
;; 26-Apr-2010    Matthew L. Fidler
;;    Last-Updated: Mon Apr 26 08:51:41 2010 (-0500) #891 (Matthew L. Fidler)
;;    Bug fix for killing buffer without saving.
;; 22-Apr-2010    Matthew L. Fidler
;;    Last-Updated: Thu Apr 22 16:15:24 2010 (-0500) #878 (Matthew L. Fidler)
;;    Fixed Bug in association between text Xmind field and ID.
;; 22-Apr-2010    Matthew L. Fidler
;;    Last-Updated: Thu Apr 22 12:11:24 2010 (-0500) #861 (Matthew L. Fidler)
;;    Added Esn Error Handling
;; 18-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Thu Feb 18 08:38:06 2010 (-0600) #847 (Matthew L. Fidler)
;;    Changed to have full topic names from parent topic.
;; 09-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Tue Feb  9 20:57:38 2010 (-0600) #715 (Matthew L. Fidler)
;;    Added Idle timer to update xmind files.
;; 09-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Tue Feb  9 20:14:17 2010 (-0600) #698 (Matthew L. Fidler)
;;    Last fix caused xmind not to launch... Removing
;; 09-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Tue Feb  9 18:23:22 2010 (-0600) #694 (Matthew L. Fidler)
;;    Only create USERSCRIPTS when the buffer file name actually exists.
;; 09-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Tue Feb  9 10:33:33 2010 (-0600) #692 (Matthew L. Fidler)
;;    When ID is NIL , esn-xmind-get-title returns NIL
;; 09-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Tue Feb  9 09:37:29 2010 (-0600) #690 (Matthew L. Fidler)
;;    Made topic update of Xmind by default ON.
;; 08-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Mon Feb  8 15:26:07 2010 (-0600) #684 (Matthew L. Fidler)
;;    Bug fix of updating Xmind properties.
;; 29-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Fri Jan 29 16:34:48 2010 (-0600) #647 (Matthew L. Fidler)
;;    Start Xmind map before updating parent information.
;; 29-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Fri Jan 29 11:44:44 2010 (-0600) #645 (Matthew L. Fidler)
;;    Fixed loading of files when Xmind doesn't exist.
;; 29-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Fri Jan 29 09:40:53 2010 (-0600) #629 (Matthew L. Fidler)
;;    Changed method to get Parent ID -- use XML tree, took care of incorrectly
;;    identifying the parent id.
;; 28-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jan 28 15:27:29 2010 (-0600) #523 (Matthew L. Fidler)
;;    Bug Fix for Saving Files when the checked-out directory is not in sync.
;; 28-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jan 28 15:18:36 2010 (-0600) #514 (Matthew L. Fidler)
;;    Removed Thumbnails from initial Maps.
;; 28-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jan 28 15:14:01 2010 (-0600) #505 (Matthew L. Fidler)
;;    Created a template Xmind file for the current model's properties.
;; 28-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jan 28 14:24:05 2010 (-0600) #480 (Matthew L. Fidler)
;;    Added Bug fix for opening files.  No longer requires a project to be created.
;; 28-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jan 28 13:33:43 2010 (-0600) #454 (Matthew L. Fidler)
;;    Took out message for Zip file creation.
;; 28-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jan 28 12:07:04 2010 (-0600) #449 (Matthew L. Fidler)
;;    Added a list of completions for automatic tab completion.
;; 28-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jan 28 11:41:06 2010 (-0600) #438 (Matthew L. Fidler)
;;    Added code to allow that when the Xmind field is changed, the Xmind map is changed (on save).
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 16:38:02 2010 (-0600) #321 (Matthew L. Fidler)
;;    Added auto-update of Parent Topic name.
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 15:55:28 2010 (-0600) #282 (Matthew L. Fidler)
;;    Fixed GetParent under PLT tools.
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 14:54:13 2010 (-0600) #262 (Matthew L. Fidler)
;;    Added function to get Parent's title
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 14:33:58 2010 (-0600) #242 (Matthew L. Fidler)
;;    Fixed the Xmind file to use the working directory as well as the file anem
;;    for PLT tools.
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 13:30:23 2010 (-0600) #236 (Matthew L. Fidler)
;;    Make USERSCRIPTS directory if not present and using PLT tools.
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 15:54:36 2010 (-0600) #234 (Matthew L. Fidler)
;;    Fixed error with Xmind file update.
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 15:32:07 2010 (-0600) #193 (Matthew L. Fidler)
;;    When Creating an initial project, now include the path of project file where
;;    the map was created.
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 15:03:11 2010 (-0600) #180 (Matthew L. Fidler)
;;    Added Xmind Launch Function (function under esn-exec, customize variables
;;    in this file)
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 14:54:48 2010 (-0600) #170 (Matthew L. Fidler)
;;    Added A Use-Xmind that checks for presence of Zip and Unzip.
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 14:38:08 2010 (-0600) #162 (Matthew L. Fidler)
;;    Added Xmind Toggle.
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

(declare-function esn-error "esn-exec")
(declare-function esn-narrow-to-current-problem "esn-narrow")
(declare-function esn-message "esn-exec")
(declare-function esn-alert "esn-exec")
(eval-when-compile
  (require 'cl))
(require 'xml)
(defun esn-drives ()
  "* Gets drives to check for PortableApps."
  (let (
        (tmp-script (make-temp-file "drives" nil ".vbs"))
        ret
        )
    (delete-file tmp-script)
    (with-temp-file tmp-script
      (insert "
    Set fso = CreateObject(\"Scripting.FileSystemObject\")
Set Drives = fso.Drives
Wscript.Echo \"(list \"
For Each DiskDrive in Drives
DriveLetter = DiskDrive.DriveLetter
DriveType = DiskDrive.DriveType
If DriveType = 1 Or DriveType = 4 Or DriveType = 5 Or DriveType = 0 Then
        Wscript.Echo \"\"\"\" & DriveLetter  & \":/\"\"\"
End If
Next
Wscript.Echo \")\""))
    (with-temp-buffer
      (insert
       (esn-command-to-string
        (format "cscript %s"
                tmp-script
                )))
      (goto-char (point-min))
      (forward-line 2)
      (end-of-line)
      (delete-region (point-min) (point))
      (insert "(setq ret ")
      (eval-buffer)
      (goto-char (point-max))
      (insert "\n)")
      (esn-error "%s" (buffer-substring (point-min) (point-max)))
      (eval-buffer))
    (delete-file tmp-script)))
(defun esn-7z ()
  "* Locates 7zip executable 7z."
  (if (not esn-w32)
      (executable-find "7z")
    nil
    ))

;;;###autoload
(defun esn-xmind-default-map-name ()
  "* Defines the default map name for Xmind files.  PLT stores the mind map under USERSCRIPTS"
  (if (and (esn-use-plt-p) (not (file-exists-p "../USERSCRIPTS")))
      (make-directory "../USERSCRIPTS"))
  (if (esn-use-plt-p)
      (concat "../USERSCRIPTS/" esn-xmind-default-map)
    (concat "./" esn-xmind-default-map)))
;;;###autoload
(defun esn-xmind-default-individual-map (&optional control-file-full-path ext)
  "* Defines the default individual Xmind map.  
When using PLT, EsN stores the mind-map under [WORKINGDIRECTORYNAME]-xmind/[CONTROLSTREAMNAME,NO EXTENSION].xmind
Otherwise stores under [CONTROLSTREAMNAME,NOEXTENSION].xmind"
  (let (
        (f (or control-file-full-path (buffer-file-name)))
        (ext (or ext "xmind"))
        (case-fold-search 't)
        dir
        )
    (when f
      (when (string-match "[.][^.]*$" f)
        (setq f (replace-match (concat "." ext) nil nil f)))
      (when (esn-use-plt-p)
        (when (string-match "\\([/\\\\][^/\\\\]*work[^/\\\\]*\\)" f)
          (setq f (replace-match (concat "\\1-" ext) nil nil f))
          (when (string-match "[/\\\\][^/\\\\]*$" f)
            (setq dir (replace-match "" nil nil f))
            (when (not (file-exists-p dir))
              (make-directory dir))))))
    (symbol-value 'f)))

(defun esn-xmind-default-prefix ()
  "* Defines the default prefix for Xmind Files."
  (if (esn-use-plt-p)
      "../"
    "./"))
;;;###autoload
(defun esn-xmind-update-ctl-map ()
  "* Updates the Control Stream Map based on what is currently going on in the control stream."
  (when (file-exists-p (buffer-file-name))
    (unless (esn-use-plt-archive-p)
      (save-restriction
        ;; Don't permanently check-out the file.
        ;; This allows files in the same directory to skip the checkout of the overall project map.
        (let (
              (esn-xmind-checkout-time nil)
              (esn-xmind-checkout-file nil)
              (esn-xmind-checkout-dir nil))
          ;; First start the control stream.
          (esn-xmind-start-ctl-map)
          (esn-xmind-rep-ctl-map)
          (esn-xmind-cancel-checkout))))))


(defun esn-xmind-rep-ctl-map ()
  "* Updates content in the current Xmind file"
  (let (
        (f (esn-xmind-default-individual-map)))
    (when f
      (let (
            (content (esn-xmind-get-content f))
            (manifest (esn-xmind-get-manifest f))
            (est (esn-est-method-txt))
            (ntheta (esn-max-theta))
            (neta (esn-max-eta))
            (neps (esn-max-eps))
            (mdesc (esn-model-txt))
            (data (save-restriction
                    (save-excursion
                      (esn-narrow-to-current-problem)
                      (goto-char (point-min))
                      (if (not (re-search-forward "\\<\\$DATA? *\\([^ \n]*\\)" nil t))
                          "None"
                        (match-string 1))))))
        (unless (and
                 (string= est esn-xmind-ctl-est)
                 (= ntheta esn-xmind-ctl-ntheta)
                 (= neta esn-xmind-ctl-neta)
                 (= neps esn-xmind-ctl-neps)
                 (string= mdesc esn-xmind-ctl-mdesc)
                 (string= data esn-xmind-ctl-data))
          (setq esn-xmind-ctl-est est)
          (setq esn-xmind-ctl-ntheta ntheta)
          (setq esn-xmind-ctl-neta neta)
          (setq esn-xmind-ctl-neps neps)
          (setq esn-xmind-ctl-mdesc mdesc)
          (setq esn-xmind-ctl-data data)
          (with-temp-buffer
            (insert content)
            (goto-char (point-min))
            ;; Estimation Step.
            (when (re-search-forward "Estimation" nil t)
              (when (re-search-forward "<title>" nil t)
                (delete-region (point)
                               (save-excursion
                                 (if (not (re-search-forward "</title>" nil t))
                                     (esn-error "Malformed .xmind file")
                                   (backward-char (length (match-string 0)))
                                   (point))))
                (insert est)))
            (goto-char (point-min))
            (when (re-search-forward "# Thetas" nil t)
              (when (re-search-forward "<title>" nil t)
                (delete-region (point)
                               (save-excursion
                                 (if (not (re-search-forward "</title>" nil t))
                                     (esn-error "Malformed .xmind file")
                                   (backward-char (length (match-string 0)))
                                   (point))))
                (insert (format "%s" ntheta))))
            (goto-char (point-min))
            (when (re-search-forward "# Etas" nil t)
              (when (re-search-forward "<title>" nil t)
                (delete-region (point)
                               (save-excursion
                                 (if (not (re-search-forward "</title>" nil t))
                                     (esn-error "Malformed .xmind file")
                                   (backward-char (length (match-string 0)))
                                   (point))))
                (insert (format "%s" neta))))
            (goto-char (point-min))
            (when (re-search-forward "# Eps" nil t)
              (when (re-search-forward "<title>" nil t)
                (delete-region (point)
                               (save-excursion
                                 (if (not (re-search-forward "</title>" nil t))
                                     (error "Malformed .xmind file")
                                   (backward-char (length (match-string 0)))
                                   (point))))
                (insert (format "%s" neps))))
            (goto-char (point-min))
            (when (search-forward "id=\"26i7agq08s4eug0m6qhlv3g57m\"" nil t)
              (when (re-search-forward "Model" nil t)
                (when (re-search-forward "<title>" nil t)
                  (delete-region (point)
                                 (save-excursion
                                   (if (not (re-search-forward "</title>" nil t))
                                       (esn-error "Malformed .xmind file")
                                     (backward-char (length (match-string 0)))
                                     (point))))
                  (insert mdesc))))
            (goto-char (point-min))
            (when (search-forward "id=\"26i7agq08s4eug0m6qhlv3g57m\"" nil t)
              (when (re-search-forward "Data" nil t)
                (when (re-search-forward "<title>" nil t)
                  (delete-region (point)
                                 (save-excursion
                                   (if (not (re-search-forward "</title>" nil t))
                                       (error "Malformed .xmind file")
                                     (backward-char (length (match-string 0)))
                                     (point))))
                  (insert data)
                  (goto-char (point-min))
                  (when (search-forward "id=\"26i7agq08s4eug0m6qhlv3g57m\"" nil t)
                    (when (re-search-forward "Data" nil t)
                      (when (re-search-forward "xlink:href=\"" nil t)
                        (delete-region (point)
                                       (save-excursion
                                         (if (not (re-search-forward "\"" nil t))
                                             (esn-error "Malformed .xmind file")
                                           (backward-char (length (match-string 0)))
                                           (point))))
                        (insert (concat "file:./" data))))))))
            (setq content (buffer-substring (point-min) (point-max))))
          ;; Update Xmind file.
          (esn-xmind-update-xmind-part content "content.xml" f)
          ;; Delete Thumbnail (since we can't update it).
          (while (string-match "<file-entry[^\n>]*?Thumbnail[^\n>]*?>"
                               manifest)
            (setq manifest (replace-match "" nil nil manifest)))
          (while (string-match "\n" manifest)
            (setq manifest (replace-match " " nil nil manifest)))
          (while (string-match "  +" manifest)
            (setq manifest (replace-match " " nil nil manifest)))
          (when (file-exists-p (format "%s/Thumbnails" esn-xmind-checkout-dir))
            (esn-xmind-del-tmp-dir (format "%s/Thumbnails" esn-xmind-checkout-dir)))
          (esn-xmind-update-xmind-part manifest "META-INF/manifest.xml" f)
          (esn-xmind-update-xmind-file))))))
(defun esn-xmind-get-safe-purpose (&optional purpose)
  "* Gets an Xmind safe first line of the purpose/PROBLEM.
Replaces > with &gt; < with &lt; and & with &amp;
If purpose is specified, just quote the string.
"
  (interactive)
  (let (
        (prp (or purpose (esn-update-get-purpose 't))))
    (when (string= prp "")
      (save-excursion
        (save-match-data
          (save-restriction
            (widen)
            (goto-char (point-min))
            (if (re-search-forward (eval-when-compile (format "%s[ \t]*\\(.*\\)" (esn-reg-record-exp "PRO" 't))) nil t)
                (setq prp (match-string 1)))))))
    (when (string-match "^[ \t]*$" prp)
      (setq prp (buffer-file-name)))
    (when (string-match "^[ \t]*" prp)
      (setq prp (replace-match "" nil nil prp)))
    (while (string-match "^;[;Cc|]*[ \t]?" prp)
      (setq prp (replace-match "" nil nil prp)))
    (while (string-match "^\\([ \t]+\\)\\*\\([ \t]\\)" prp)
      (setq prp (replace-match "\\1-\\2" nil nil prp)))
    (while (string-match "<"  prp)
      (setq prp (replace-match "&lt;" nil nil prp)))
    (while (string-match ">"  prp)
      (setq prp (replace-match "&gt;" nil nil prp)))
    (with-temp-buffer
      (insert prp)
      (goto-char (point-min))
      (while (search-forward "&" nil t)
        (if (not (looking-at "\\(amp\\|lt\\|gt\\);"))
            (insert "amp;")))
      (goto-char (point-min))
      (when (looking-at ".*")
        (setq prp (match-string 0))))
    (symbol-value 'prp)))
(defun esn-xmind-start-ctl-map ()
  "* Creates a Default Xmind map for the current file"
  (let (
        (f (buffer-file-name))
        f2
        )
    (when f
      (setq f2 f)
      (setq f (esn-xmind-default-individual-map f))
      (unless (file-exists-p f)
        (let (
              content manifest
                      (label (esn-xmind-label))
                      (purpose (esn-xmind-get-safe-purpose)))
          (esn-xmind-blank-map f purpose label)
          (esn-xmind-checkout f)
          (setq manifest (esn-xmind-get-manifest f))
          (setq content (esn-xmind-get-content f))
          ;; Add Link
          (when (string-match "[/]\\([^/]*\\)$" f2)
            (setq f2 (match-string 1 f2)))
          (with-temp-buffer
            (insert content)
            (goto-char (point-min))
            (when (re-search-forward "<topic\\>" nil t)
              (when (re-search-forward ">" nil t)
                (backward-char 1)
                (insert " xlink:href=\"file:./")
                (insert f2)
                (insert "\"")))
            (setq content (buffer-substring (point-min) (point-max))))
          ;; Add xml table
          (when (string-match "</topic>" content)
            (setq content (replace-match (concat esn-xmind-ctl-xml "</topic>") 't 't content)))
          (esn-xmind-update-xmind-part content "content.xml" f)
          (esn-xmind-update-xmind-file))))))

(defvar esn-xmind-blank-content.xml
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><xmap-content xmlns=\"urn:xmind:xmap:xmlns:content:2.0\" xmlns:fo=\"http://www.w3.org/1999/XSL/Format\" xmlns:svg=\"http://www.w3.org/2000/svg\" xmlns:xhtml=\"http://www.w3.org/1999/xhtml\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"2.0\"><sheet id=\"67rb5mhr721o6847rtsf104a4q\"><topic id=\"2nkrp3713sjrd7id6kfnlphfpk\" timestamp=\"%s\"><title>%s</title><labels><label>%s</label></labels></topic><title>%s</title><legend visibility=\"visible\"><position svg:x=\"-88\" svg:y=\"67\"/><marker-descriptions><marker-description description=\"Run Status Unknown\" marker-id=\"other-question\"/><marker-description description=\"Warning on most recent run\" marker-id=\"other-exclam\"/><marker-description description=\"Successful Minimization\" marker-id=\"other-yes\"/><marker-description description=\"Run Terminated\" marker-id=\"other-no\"/></marker-descriptions></legend></sheet></xmap-content>"
  "* Format for blank content.xml
first argument is timestamp.
Second argument is Root name
Third argument is Label of root
Fourth Argument is sheet title.")
(defvar esn-xmind-blank-meta.xml
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><meta xmlns=\"urn:xmind:xmap:xmlns:meta:2.0\" version=\"2.0\"/>"
  "* Blank XML")
(defvar esn-xmind-blank-manifest.xml
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><manifest xmlns=\"urn:xmind:xmap:xmlns:manifest:1.0\"><file-entry full-path=\"content.xml\" media-type=\"text/xml\"/><file-entry full-path=\"META-INF/\" media-type=\"\"/><file-entry full-path=\"META-INF/manifest.xml\" media-type=\"text/xml\"/><file-entry full-path=\"meta.xml\" media-type=\"text/xml\"/><file-entry full-path=\"Thumbnails/\" media-type=\"\"/></manifest>"
  "* Blank XML manifest"
  )
;;;###autoload
(defun esn-xmind-blank-map (file &optional root-name root-label sheet-title timestamp)
  "* Create blank map."
  (let (
        (esn-xmind-checkout-time (format-time-string "%s"))
        (esn-xmind-checkout-file file)
        (esn-xmind-checkout-dir (make-temp-file "xmind" 't))
        (esn-xmind-list-of-completions-id nil)
        (esn-xmind-list-of-completions-topic nil)
        (esn-xmind-list-of-completions-titles nil)
        (content (format esn-xmind-blank-content.xml
                         (or timestamp (esn-xmind-timestamp))
                         (or root-name "Project")
                         (or root-label (if (buffer-file-name) (file-name-directory (buffer-file-name)) ""))
                         (or sheet-title (format "Created in EsN %s" esn-mode-ver))))
        tmp-dir
        )
    (setq tmp-dir esn-xmind-checkout-dir)
    (with-temp-file (concat tmp-dir "/content.xml")
      (insert content))
    (with-temp-file (concat tmp-dir "/meta.xml")
      (insert esn-xmind-blank-meta.xml))
    (make-directory (concat tmp-dir "/META-INF"))
    (with-temp-file (concat tmp-dir "/META-INF/manifest.xml")
      (insert esn-xmind-blank-manifest.xml))
    (esn-xmind-update-xmind-file)))
;;;###autoload
(defun esn-xmind-start-map (&optional file)
  "* Creates a default map"
  (esn-xmind-cancel-checkout)
  (let (
        (f (or file (esn-xmind-default-map-name))))
    (unless (file-exists-p f)
      (esn-xmind-blank-map f))))
(defun esn-xmind-del-tmp-dir (&optional dir-to-del)
  "* Deletes temporary directory (completely, recursive function.)"
  (let* (
         (dir (or dir-to-del esn-xmind-checkout-dir))
         files
         )
    (when (and dir
               (file-exists-p dir))
      (setq files (directory-files dir 't))
      (mapc (lambda(d)
              (unless (string-match "/[.]+$" d)
                (let (
                      (f-attr (file-attributes d)))
                  (if (eq (nth 0 f-attr) t)
                      (progn
                        (esn-xmind-del-tmp-dir d))
                    (delete-file d)))))
            files
            )
      (delete-directory dir))))
(defun esn-xmind-rm-dirs ()
  "* Removes temporary directories"
  (interactive)
  (mapc (lambda(x)
          (esn-xmind-del-tmp-dir x))
        esn-xmind-checkout-rm-dir-lst)
  (setq esn-xmind-checkout-rm-dir-lst
        (remove-if-not #'(lambda(file)
                           (if (file-exists-p file)
                               (esn-message "Could not delete %s" file))
                           (file-exists-p file))
                       esn-xmind-checkout-rm-dir-lst
                       )))


(defvar esn-xmind-checkout-rm-dir-lst ())
(defun esn-xmind-cancel-checkout ()
  "* Cancels a checkout -- i.e. deletes everything that is in the temporary directory."
  (add-to-list 'esn-xmind-checkout-rm-dir-lst esn-xmind-checkout-dir)
  (setq esn-xmind-checkout-time nil)
  (setq esn-xmind-checkout-file nil)
  (setq esn-xmind-checkout-dir nil)
  (setq esn-xmind-list-of-completions-id nil)
  (setq esn-xmind-list-of-completions-topic nil)
  (setq esn-xmind-list-of-completions-titles nil)
  (run-with-timer 0.5 nil 'esn-xmind-rm-dirs))
;;;###autoload
(defun esn-xmind-checkout (&optional xmind-file)
  "* Checks out an Xmind file."
  (let (
        (file (or xmind-file (esn-xmind-default-map-name)))
        f-attr
        tm
        checkout
        tmp-dir
        )
    (setq f-attr (file-attributes file))
    (setq tm (nth 6 f-attr))
    (setq checkout nil)

    (if (not esn-xmind-checkout-time)
        (setq checkout 't)
      (unless (and (string= file esn-xmind-checkout-file)
                   (string= (format-time-string "%s" tm) esn-xmind-checkout-time))
        (esn-xmind-cancel-checkout)
        (setq checkout 't)))
    (when (and (not checkout) (not (file-exists-p esn-xmind-checkout-dir)))
      (setq checkout 't))
    (when checkout
      (setq tmp-dir (make-temp-file "xmind" 't))
      (if (executable-find "unzip")
          (esn-command-to-string (format "unzip %s -d %s" file
                                         tmp-dir))
        (if (executable-find "7z")
            (esn-command-to-string (format "7z x %s -o%s"
                                           file tmp-dir))
          (if (and esn-w32 (file-exists-p (concat (getenv "PROGRAMFILES") "\\7-zip\\7z.exe")))
              (esn-command-to-string (format "\"%s\\7-zip\\7z.exe\" x %s -o%s"
                                             (getenv "PROGRAMFILES") file tmp-dir)))))
      (setq f-attr (file-attributes file))
      (setq tm (nth 6 f-attr))
      (setq esn-xmind-checkout-time (format-time-string "%s" tm))
      (setq esn-xmind-checkout-file file)
      (setq esn-xmind-checkout-dir tmp-dir)
                                        ;      (message "Checked out %s to %s" file tmp-dir)
      )))


(defun esn-xmind-timestamp ()
  "* Xmind Timestamp"
  (format "%s000" (format-time-string "%s")))


(defun esn-xmind-label ()
  "* Gets XMind label for current file"
  (let (
        (fn (buffer-file-name))
        (two (esn-use-plt-p)))
    (when fn
      (if (string-match "/\\([^/]*/\\)\\([^/]*\\)$" fn)
          (if two
              (setq fn (concat (match-string 1 fn) (match-string 2 fn)))
            (setq fn (match-string 2 fn)))
        (if (string-match "/\\([^/]*\\)$" fn)
            (setq fn (match-string 1 fn)))))
    (symbol-value 'fn)))

(defun esn-xmind-topic (&optional prefix)
  "* Gets parsed XML for the current buffer"
  (let (
        (fn (esn-xmind-label))
        (purpose (esn-xmind-get-safe-purpose))
        id
        link
        (pre (or prefix (esn-xmind-default-prefix)))
        (ret nil))
    (setq id (concat "esn_" fn))
    (while (string-match "[^A-Za-z0-9_]" id)
      (setq id (replace-match "_" nil nil id)))
    (setq link (esn-xmind-default-individual-map (concat pre fn)))
    (with-temp-buffer
      (insert (format esn-xmind-default-topic
                      (concat id (format-time-string "%Y%m%d%H%M%S"))
                      (esn-xmind-timestamp)
                      link
                      purpose
                      fn
                      ))
      (setq ret (buffer-substring (point-min) (point-max))))
    (symbol-value 'ret)))

(defun esn-xmind-update-add-child (parent-id xml-topic xml-content)
  "* Adds a child topic to the xml file."
  (let (
        (current parent-id)
        (content xml-content)
        (topic xml-topic)
        pt
        )
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (when (re-search-forward (format "id=\"%s\"" (regexp-quote current)) nil t)
        (when (re-search-backward "<topic\\>" nil t)
          (setq pt (point))
          ;; Now figure out if this has any children
          (when (re-search-forward "<\\(/topic\\|children\\)>" nil t)
            (if (string= (downcase (match-string 1)) "/topic")
                (progn
                  ;; Only a topic
                  (backward-char (length (match-string 0)))
                  (insert "<children><topics type=\"attached\">")
                  (insert topic)
                  (insert "</topics></children>"))
              ;; Subtopics exist -- Insert at beginning of topics.
              (when (re-search-forward "<topics.*?>" nil t)
                (insert topic))))))
      (setq content (buffer-substring (point-min) (point-max))))
    (symbol-value 'content)))

(defun esn-xmind-update-current-file (&optional xmind-file prefix)
  "* Updates the mind map to include the current file."
  (unless xmind-file
    (esn-xmind-start-map))
  (when (string= "" esn-xmind-last-topic-title)
    (esn-xmind-current-topic) ; put the purpose from the zip file in.
    )
  (let (
        (case-fold-search 't)
        (content (esn-xmind-get-content xmind-file))
        (manifest (esn-xmind-get-manifest xmind-file))
        (current (esn-xmind-current-topic xmind-file))
        pt
        (purpose (esn-xmind-get-safe-purpose))
        link
        cnt
        topic
        (pre (or prefix (esn-xmind-default-prefix))))
    (unless (string= purpose esn-xmind-last-topic-title)
      (if current
          (progn

            (setq link (esn-xmind-default-individual-map (concat pre (esn-xmind-label))))
            (with-temp-buffer
              ;; Already defined in Mind Map.  Update contents:
              ;;  * Updates description (when esn-xmind-update-topic-name is true)
              ;;  * Updates Marker indicating the latest version hasn't been submitted yet.
              ;;  * Updates Link to xmind file.
              (insert content)
              (goto-char (point-min))
              (when (re-search-forward (format "\\<id=\"%s\"" (regexp-quote current)) nil t)
                (when (re-search-backward "<topic\\>" nil t)
                  (setq pt (point))
                  (when (re-search-forward "\\<\\(marker-id=\"\\)[^\"]*" nil t)
                    (replace-match "\\1other-question"))
                  (when esn-xmind-update-topic-name
                    (goto-char pt)
                    (when (re-search-forward "<title>" nil t)
                      (delete-region (point)
                                     (save-excursion
                                       (if (not (re-search-forward "</title>" nil t))
                                           (esn-error "Malformed .xmind file")
                                         (backward-char (length (match-string 0)))
                                         (point))))
                      (insert purpose)))
                  (goto-char pt)
                  (when (re-search-forward "\\(xlink:href=\"\\)[^\"]*" nil t)
                    (replace-match (concat " \\1file:" link)))))
              (setq content (buffer-substring (point-min) (point-max)))))
        ;; Now get parent.
        (setq current (esn-xmind-parent-topic xmind-file 'content))
        (unless current
          ;; Attach to main
          (with-temp-buffer
            (insert content)
            (goto-char (point-min))
            (when (re-search-forward "<topic\\>" nil t)
              (when (re-search-forward "id=\"\\([^\"]*\\)\""  nil t)
                (setq current (match-string 1))))))
        ;; Now figure out the best place to attach; Add as a child.
        (setq topic (esn-xmind-topic prefix))
        (setq content (esn-xmind-update-add-child current topic content)))
      (esn-xmind-update-xmind-part content "content.xml" xmind-file)
      ;; Delete Thumbnail (since we can't update it).
      (while (string-match "<file-entry[^\n>]*?Thumbnail[^\n>]*?>"
                           manifest)
        (setq manifest (replace-match "" nil nil manifest)))
      (while (string-match "\n" manifest)
        (setq manifest (replace-match " " nil nil manifest)))
      (while (string-match "  +" manifest)
        (setq manifest (replace-match " " nil nil manifest)))
      (when (file-exists-p (format "%s/Thumbnails" esn-xmind-checkout-dir))
        (esn-xmind-del-tmp-dir (format "%s/Thumbnails" esn-xmind-checkout-dir)))
      (esn-xmind-update-xmind-part manifest "META-INF/manifest.xml" xmind-file)
      (esn-xmind-update-xmind-file)
      (setq esn-xmind-last-topic-title purpose))))

(defun esn-xmind-current-topic (&optional xmind-file topic-name)
  "* Gets the topic ID defined for the current file.  If it doesn't exist, return nil.  If it does exist, Set local variable esn-xmind-last-topic-title
"
  (let (
        (file (or xmind-file (esn-xmind-default-map-name)))
        content
        ret
        fn
        pt1
        )
    (if topic-name
        (setq fn topic-name)
      (setq fn (buffer-file-name))
      (when fn
        (if (string-match "/\\([^/]*/\\)\\([^/]*\\)$" fn)
            (if (esn-use-plt-p)
                (setq fn (concat (match-string 1 fn) (match-string 2 fn)))
              (setq fn (match-string 2 fn)))
          (if (string-match "/\\([^/]*\\)$" fn)
              (setq fn (match-string 1 fn))))
        (setq fn (concat "<label>" fn "</label>"))
        (setq content (esn-xmind-get-content file))
        (when content
          (with-temp-buffer
            (insert content)
            (goto-char (point-min))
            (when (re-search-forward (format "%s" (regexp-quote fn)) nil t)
              (when (re-search-backward "<topic\\>" nil t)
                (when (re-search-forward "\\<id=\"\\([^\"]*\\)\"" nil t)
                  (setq ret (match-string 1))
                  (when (search-forward "<title>" nil t)
                    (setq pt1 (point))
                    (when (search-forward "</title>" nil t)
                      (backward-char (length (match-string 0)))
                      (setq esn-xmind-last-topic-title (buffer-substring-no-properties pt1 (point))))))))))))
    (symbol-value 'ret)))

(defun esn-xmind-get-parent-title (&optional xmind-file)
  "* Gets the Parent Topic name, or main topic name for the current file"
  (let (
        (parent (esn-xmind-parent-topic  xmind-file))
        (file (or xmind-file (esn-xmind-default-map-name))))
    (unless parent
      (with-temp-buffer
        (insert (esn-xmind-get-content file))
        (goto-char (point-min))
        (when (re-search-forward "<topic\\>" nil t)
          (when (re-search-forward "id=\"\\([^\"]*\\)\""  nil t)
            (setq parent (match-string 1))))))
    (if parent
        (esn-xmind-get-title parent xmind-file)
      parent
      )))

(defun esn-xml-print (xml)
  "Outputs the XML in the current buffer.
XML can be a tree or a list of nodes."
  (dolist (node xml)
    (esn-xml-debug-print-internal node)))

(defun esn-xml-debug-print-internal (xml)
  "Outputs the XML tree in the current buffer.
The first line is indented with NO INDENTATION or RETURNS."
  ;; Taken from xml.el and changed so that there is NO indents or returns added (messes up Xmind files)
  (let ((tree xml)
        attlist)
    (insert ?\< (symbol-name (xml-node-name tree)))

    ;;  output the attribute list
    (setq attlist (xml-node-attributes tree))
    (while attlist
      (insert ?\  (symbol-name (caar attlist)) "=\""
              (xml-escape-string (cdar attlist)) ?\")
      (setq attlist (cdr attlist)))

    (setq tree (xml-node-children tree))

    (if (null tree)
        (insert ?/ ?>)
      (insert ?>)

      ;;  output the children
      (dolist (node tree)
        (cond
         ((listp node)
          (esn-xml-debug-print-internal node ))
         ((stringp node)
          (insert (xml-escape-string node)))
         (t
          (esn-error "Invalid XML tree"))))

      (insert ?\< ?/ (symbol-name (xml-node-name xml)) ?>))))

(defun esn-xmind-mv-topic (topic-id parent-id xml &optional xmind-file)
  "* Moves a topic from Xmind XML and places it under parent-id"
  (let (
        (id topic-id)
        topic
        new-xml
        )
    (setq new-xml (mapcar (lambda(x)
                            (esn-xmind-rm-topic-internal x id 'topic))
                          xml))
    (with-temp-buffer
      (esn-xml-print (list topic))
      (setq topic (buffer-substring (point-min) (point-max))))
    (with-temp-buffer
      (esn-xml-print new-xml)
      (setq new-xml (buffer-substring (point-min) (point-max))))
    (setq new-xml (esn-xmind-update-add-child parent-id topic new-xml))
    ;; Now add the topic under the parent id.
    (esn-xmind-update-xmind-part new-xml "content.xml" xmind-file)
    (esn-xmind-update-xmind-file)))

(defun esn-xmind-rm-topic-internal (xml rm-id topic)
  "* Removes topic in Xmind file;  internal tree-parser."
  (let (
        (tree xml)
        attlist
        tmp
        node
        (ret ())
        (id (format "%s" (xml-get-attribute-or-nil xml 'id))))
    (setq node (xml-node-name tree))
    (setq attlist (xml-node-attributes tree))
    (setq tree (xml-node-children tree))
    (setq ret tree)
    (if (and id (string= id rm-id))
        (progn
          (set topic xml)
          (setq ret nil)
          (symbol-value 'ret))
      (unless (null tree)
        (setq ret (mapcar
                   (lambda(x)
                     (if (listp x)
                         (esn-xmind-rm-topic-internal x rm-id 'topic)
                       x
                       ))
                   tree
                   )))
      (push attlist ret)
      (push node ret)
      (symbol-value 'ret))))

(defun esn-xmind-move-topic (new-topic &optional xmind-file)
  "* Moves the information for the current file under a new topic \"new-topic\""
  (let (
        (new-topic-id (esn-xmind-current-topic xmind-file new-topic))
        current-id
        content
        xml
        new-xml
        )
    (if (and (not new-topic-id) (not esn-xmind-create-new-topic-on-move))
        (esn-message "New Topic not found in Mind Map, keeping old topic...")
      (setq current-id (esn-xmind-current-topic xmind-file))
      (setq content (esn-xmind-get-content xmind-file))
      (when (not new-topic-id)
        (esn-message "New topic not found in Mind-map, creating new topic")
        (setq new-topic-id (esn-xmind-parent-topic xmind-file 'content)))
      (with-temp-buffer
        (insert content)
        (setq xml (xml-parse-region (point-min) (point-max))))
      (esn-xmind-mv-topic current-id new-topic-id xml xmind-file))))

(defun esn-xmind-get-control-streams ()
  "* Gets the control stream possibilites based on the current directory."
  (interactive)
  (let* (
         (dirs (if (esn-use-plt-p)
                   (remove-if-not (lambda(x)
                                    (and
                                     (file-directory-p x)
                                     (string-match "work" (downcase x))
                                     (not (string-match "\\(plta\\|xmind\\)" (downcase x)))
                                     (not (string-match "[/\\\\][.]plt" (downcase x)))))
                                  (file-expand-wildcards "../*"))
                 '("./")))
         (ret
          (apply 'append
                 (mapcar
                  (lambda(ext)
                    (apply 'append
                           (mapcar
                            (lambda(dir)
                              (file-expand-wildcards (concat dir "/*" ext)))
                            dirs)))
                  (append esn-default-extension (list ".txt"))))))
    (symbol-value 'ret)))

(defun esn-xmind-update-all-files-in-current-directory ()
  "*Updates all topics _project.xmind mind-map"
  (interactive)
  (let (
        (files (esn-xmind-get-control-streams))
        (i 0))
    (mapc (lambda(file)
            (save-window-excursion
              (let (
                    (opened (get-file-buffer file))
                    (esn-skip-read-only-check 't)
                    (case-fold-search 't))
                (if opened
                    (set-buffer opened)
                  (set-buffer (find-file-noselect file 't)))
                (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "\\<[$]PRO" nil t)
                    (setq esn-xmind-last-topic-title "")
                    (setq i (+ i 1))
                    (esn-message "Xmind Update #%s, based on file: %s" i file)
                    (condition-case err
                        (progn
                          (esn-xmind-update-current-file))
                      (error (esn-alert "Error updating %s: %s" file
                                        (error-message-string err)))))
                  (unless opened
                    (kill-buffer (current-buffer)))))))
          files)))

(defun esn-add-update-topics-now ()
  "* Stop idle timer and run NOW."
  (save-restriction
    (when esn-add-update-topics-timer
      (cancel-timer esn-add-update-topics-timer)
      (esn-add-update-parent-topic-actual)
      (esn-xmind-update-ctl-map))))

(defmacro esn-update-parent-topic-this-buffer ()
  "* Macro for updating the actual topic on the current buffer"
  `(lambda()
     (interactive)
     (save-excursion
       (set-buffer ,(current-buffer))
       (esn-add-update-parent-topic-actual)
       (setq esn-add-update-topics-timer
             (run-with-idle-timer 4 nil
                                  (lambda()
                                    (interactive)
                                    (save-excursion
                                      (set-buffer ,(current-buffer))
                                      (esn-xmind-update-ctl-map))))))))

;;;###autoload
(defun esn-add-update-topics ()
  "* Starts an idle timer to add or update a parent topic."
  (when esn-add-update-topics-timer
    (cancel-timer esn-add-update-topics-timer))
  (setq esn-add-update-topics-timer
        (run-with-idle-timer 4 nil (esn-update-parent-topic-this-buffer))))


(defun esn-add-update-parent-topic-actual (&optional xmind-file topic-name)
  "* Adds or Updates a Xmind Comment.  This doesn't count as a change to the buffer."
  (interactive)
  (when (and (file-exists-p (buffer-file-name ))
             (not (esn-use-plt-archive-p)))
    (save-restriction
      (esn-xmind-start-map)
      (let (
            (in-file (esn-xmind-current-topic xmind-file topic-name))
            (case-fold-search 't)
            (inhibit-point-motion-hooks 't)
            (inhibit-read-only 't)
            (file (or xmind-file (esn-xmind-default-map-name)))
            (parent-id (esn-xmind-parent-topic xmind-file))
            (buf-mod (buffer-modified-p))
            new-id
            h-reg
            parent-topic
            new-topic
            move
            f-attr
            tmp
            tm
            )
        (setq f-attr (file-attributes file))
        (setq tm (nth 6 f-attr))
        (setq tm (format-time-string "%s" tm))
        (if (not esn-xmind-last-parent-topic-time)
            (progn
              ;; It is a new file-open, update the Xmind field.
              (setq move nil)
              (setq esn-xmind-last-parent-topic-time tm))
          (if (string= tm esn-xmind-last-parent-topic-time)
              (progn
                ;; Its the same file, nothing has changed, if there is a change move
                ;; it.
                (setq move 't)
                (setq esn-xmind-last-parent-topic-time tm))
            ;; Its a different project file, update the Xmind field if the last
            ;; known open or save has the same value as it does now, otherwise move
            ;; the topic.
            (save-excursion
              (goto-char (point-min))
              (if (re-search-forward "\\(;[;C]*[ \t]*\\(?:Free\\|X\\)?\\(?:mind\\(?:map\\)?\\|map\\)[ \t]*[:=][ \t]*\\)\\([^ \t].*?\\)\\([ \t]*\\)$" nil t)
                  (setq tmp (match-string 2))))
            (if (string= esn-xmind-last-parent-topic
                         tmp)
                (setq move nil)
              (setq move 't))
            (setq esn-xmind-last-parent-topic-time tm)))
        (when in-file
          ;;(message "Currently in xmind map %s" in-file)
          (save-excursion
            (goto-char (point-min))
            (save-match-data
              (unless esn-xmind-list-of-completions-id
                (esn-xmind-list-of-completions))
              (if (assoc parent-id esn-xmind-list-of-completions-id)
                  (setq parent-topic (cadr (assoc parent-id esn-xmind-list-of-completions-id)))
                (setq parent-topic (save-match-data (esn-xmind-get-title parent-id  xmind-file)))))
            (if (re-search-forward "\\(;[;C]*[ \t]*\\(?:Free\\|X\\)?\\(?:mind\\(?:map\\)?\\|map\\)[ \t]*[:=][ \t]*\\)\\([^ \t].*?\\)\\([ \t]*\\)$" nil t)
                (progn
                  (if (not move)
                      (progn
                        (replace-match (format "\\1%s\\3" parent-topic)))
                    (setq new-topic (match-string 2))
                    (when (not (string= new-topic parent-topic))
                      ;; Get new-topic ID.
                      (save-match-data
                        (setq new-id (esn-xmind-current-topic xmind-file new-topic)))
                      (when (not (string= new-id parent-id))
                        (esn-xmind-move-topic new-topic xmind-file)
                        ;; Have update file, update time-stamp
                        (setq f-attr (file-attributes file))
                        (setq tm (nth 6 f-attr))
                        (setq tm (format-time-string "%s" tm))
                        (setq esn-xmind-last-parent-topic-time tm)
                        (setq parent-topic new-topic)
                        (setq parent-id new-id)))))
              (setq h-reg (esn-header-regexp-quote))
              (if (or (re-search-forward h-reg nil t)
                      (re-search-forward "\\$PRO[A-Z].*" nil t))
                  (progn
                    (insert "\n; Xmind: ")
                    (insert (format "%s" parent-topic))
                    (insert "\n"))))))
        (setq esn-xmind-last-parent-topic parent-topic)
        (when (not buf-mod)
          (set-buffer-modified-p nil))))))

(defun esn-xmind-get-extended-parent-title-internal (xml id title ret-q)
  "* Gets xmind extended title (internal function).  Sets external ret"
  (let (
        (tree xml)
        attlist
        (tit title))
    (cond
     ( (eq (xml-node-name tree) 'topic)
       (when (string= (format "%s" (xml-get-attribute tree 'id)) id)
         (set ret-q tit))))
    (unless (symbol-value ret-q)
      (setq tree (xml-node-children tree))
      (unless (null tree)
        (dolist (node tree)
          (cond
           ((listp node)
            (if (eq (xml-node-name node) 'title)
                (if (string= "" tit)
                    (setq tit (nth 0 (xml-node-children node)))
                  (setq tit (concat tit " > " (nth 0 (xml-node-children node)))))
              (esn-xmind-get-extended-parent-title-internal node id tit ret-q)))
           ((stringp node)
            nil
            )
           (t
            (esn-error "Invalid XML tree"))))))))

(defun esn-xmind-get-extended-parent-title (id &optional xmind-file)
  (if (not id)
      nil
    (let (
          content
          xml
          ret
          )
      (setq content (esn-xmind-get-content xmind-file))
      (with-temp-buffer
        (insert content)
        (setq xml (xml-parse-region (point-min) (point-max))))
      (dolist (node xml)
        (esn-xmind-get-extended-parent-title-internal node id "" 'ret))
      (symbol-value 'ret))))

(defun esn-xmind-get-title (id &optional xmind-file)
  "* Gets the Topic name for the ID specified"
  (if (not id)
      nil
    (let (
          (content (esn-xmind-get-content xmind-file))
          ret
          )
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (when (re-search-forward (format "\\<id=\"%s\"" (regexp-quote id)) nil t)
          (when (re-search-forward "<title>" nil t)
            (setq ret (buffer-substring (point)
                                        (save-excursion
                                          (when (re-search-forward "</title>" nil 't)
                                            (backward-char (length (match-string 0))))

                                          (point))))
            (when (string= ret "")
              (setq ret nil)))))
      (symbol-value 'ret))))

(defun esn-xmind-list-of-completions-internal (xml current-id title)
  "* Gets xmind extended title (internal function).  Sets external"
  (let (
        (tree xml)
        attlist
        (id current-id)
        (tit title))
    (if (not (eq (xml-node-name tree) 'topic))
        (setq tree (xml-node-children tree))
      (setq id (format "%s" (xml-get-attribute tree 'id)))
      (setq tree (xml-node-children tree)))
    (unless (null tree)
      (dolist (node tree)
        (cond
         ((listp node)
          (if (eq (xml-node-name node) 'title)
              (progn
                (if (string= "" tit)
                    (setq tit (nth 0 (xml-node-children node)))
                  (setq tit (concat tit " > " (nth 0 (xml-node-children node)))))
                (when id
                  (add-to-list 'id-lst (list id tit))
                  (add-to-list 'tit-lst (list tit id))))
            (esn-xmind-list-of-completions-internal node id tit)))
         ((stringp node)
          nil
          )
         (t
          (esn-error "Invalid XML tree")))))))

(defun esn-xmind-list-of-completions (&optional xmind-file)
  "* Gets a completion list for Topics in the current Xmind file."
  (interactive)
  (unless esn-xmind-list-of-completions-topic
    (let (
          (content (esn-xmind-get-content xmind-file))
          xml
          (debug-on-error 't)
          (id-lst '())
          (tit-lst '()))
      (with-temp-buffer
        (insert content)
        (setq xml (xml-parse-region (point-min) (point-max))))
      (dolist (node xml)
        (esn-xmind-list-of-completions-internal node nil ""))
      (setq esn-xmind-list-of-completions-id id-lst)
      (setq esn-xmind-list-of-completions-topic tit-lst)
      (setq esn-xmind-list-of-completions-titles
            (mapcar (lambda(x) (nth 0 x))
                    esn-xmind-list-of-completions-topic))))
  esn-xmind-list-of-completions-titles
  )

(defun esn-xmind-parent-topic-internal (xml cur-id-q ret-q &optional last)
  "* Internally looks for parent topic.  Sets external variables:
ret

Uses external variables cur-id,ret"
  (let ((tree xml)
        attlist
        (last-id last))
    (when (eq (xml-node-name tree) 'topic)
      (when (string= (format "%s" (xml-get-attribute tree 'id))
                     (symbol-value cur-id-q))
        (set ret-q last-id))
      (setq last-id (xml-get-attribute tree 'id)))
    (setq tree (xml-node-children tree))
    (when (not (symbol-value ret-q))
      (unless (null tree)
        (dolist (node tree)
          (cond
           ((listp node)
            (esn-xmind-parent-topic-internal node cur-id-q ret-q last-id))
           ((stringp node)
            nil
            )
           (t
            (esn-error "Invalid XML tree"))))))))

(defun esn-xmind-parent-topic (&optional xmind-file content-q)
  "* Gets the Parent Topic ID for current file.
If the file is already within the use that parent ID.
If the file is not already within the map check
;Xmind: File

For contents and see if there is a category in the file.
"
                                        ; FreeMind: MindMap_Node_ID
                                        ; FreeMind: MindMap_Node_Text
                                        ; FreeMind: Reference_File
                                        ; XMind: MindMap_Node_ID
                                        ; XMind: MindMap_Node_Text
                                        ; XMind: Reference_File
                                        ; Ref: reference_model
  (let (
        (inhibit-point-motion-hooks 't)
        (case-fold-search 't)
        content-i
        ret
        (fn (esn-xmind-label))
        (cur-id (esn-xmind-current-topic xmind-file))
        xml
        tmp1
        )
    (setq content-i (esn-xmind-get-content xmind-file))
    (with-temp-buffer
      (insert content-i)
      (setq xml (xml-parse-region (point-min) (point-max)))
      (dolist (node xml)
        (esn-xmind-parent-topic-internal node 'cur-id 'ret)))
    (unless ret
      ;; Could not find parent, look for file for direction.
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward ";[;C|]*[ \t]*\\(?:Free\\|X\\)?\\(?:mind\\(?:map\\)?\\|map\\)[ \t]*[:=][ \t]*\\([^ \t].*?\\)[ \t]*$" nil t)
          (setq tmp1 (match-string 1))
          (while (string-match "  +>  +" tmp1)
            (setq tmp1 (replace-match " > " tmp1)))
          (unless esn-xmind-list-of-completions-topic
            (esn-xmind-list-of-completions))
          ;; Try to get actual node
          (if (assoc tmp1 esn-xmind-list-of-completions-topic)
              (setq ret (cadr (assoc tmp1 esn-xmind-list-of-completions-topic)))
            ;; Create Nodes when content-q is defined
            (when content-q
              (let (
                    (desired tmp1)
                    (to-add "")
                    last-parent
                    id
                    (i 0))
                (while (and (not last-parent) (string-match " > \\([^>]*\\)$" desired))
                  (save-match-data
                    (if (string= to-add "")
                        (progn
                          (setq id (concat "esn_tree_" (number-to-string i) (esn-xmind-timestamp)))
                          (setq to-add (format esn-xmind-default-tree id
                                               (esn-xmind-timestamp)
                                               (esn-xmind-get-safe-purpose (match-string 1 desired)))))
                      ;; It is a child
                      (setq to-add (format esn-xmind-default-tree-children
                                           (concat "esn_tree_" (number-to-string i) (esn-xmind-timestamp))
                                           (esn-xmind-timestamp)
                                           (esn-xmind-get-safe-purpose (match-string 1 desired))
                                           to-add)))
                    (setq i (+ i 1)))
                  (setq desired (replace-match "" nil nil desired))
                  (when (assoc desired esn-xmind-list-of-completions-topic)
                    (setq last-parent (cadr (assoc desired esn-xmind-list-of-completions-topic)))
                    (set content-q (esn-xmind-update-add-child last-parent to-add (symbol-value content-q)))
                    (setq ret id))))))
          (unless ret
            (setq ret (esn-xmind-current-topic xmind-file tmp1))))
        (when (and (not ret)
                   (re-search-forward ";[;C]*[ \t]*\\(Ref[^:=]*?\\|Par[^:=]*?\\)[:=]" nil t)))))
    (symbol-value 'ret)))

(defun esn-xmind-get-content (&optional file xml-file)
  "* Gets Xmind Content.xml file from .xmind file"
  (let (
        (file (or file (esn-xmind-default-map-name)))
        (xml-file (or xml-file "content.xml"))
        tmp-dir
        content
        )
    (esn-xmind-checkout file)
    (setq tmp-dir esn-xmind-checkout-dir)
    (if (not (file-exists-p (format "%s/%s" tmp-dir xml-file)))
        (progn
          (message "Xmind file %s does not contain %s" file xml-file)
          (setq content nil))
      (setq content
            (with-temp-buffer
              (insert-file-contents (format "%s/%s" tmp-dir xml-file))
              (buffer-substring (point-min) (point-max)))))
    (symbol-value 'content)))

(defun esn-xmind-get-styles (&optional file)
  (esn-xmind-get-content file "styles.xml"))

(defun esn-xmind-get-meta (&optional file)
  (esn-xmind-get-content file "meta.xml"))

(defun esn-xmind-get-manifest (&optional file)
  (esn-xmind-get-content file "META-INF/manifest.xml"))

(defun esn-xmind-update-xmind-part (content file xmind-file)
  "* Updates a file in the checked out Xmind directory."
  (let (
        tmp-dir
        )
    (esn-xmind-checkout xmind-file)
    (setq tmp-dir esn-xmind-checkout-dir)
    (if (not (file-exists-p (format "%s/%s" tmp-dir file)))
        (progn
          (message "Xmind file does not contain file:%s, creating" file))
      (delete-file (format "%s/%s" tmp-dir file)))
    (with-temp-file (format "%s/%s" tmp-dir file)
      (insert content)
      (setq buffer-file-coding-system 'utf-8))))
(defvar esn-xmind-use-7z nil)
(defun esn-xmind-update-xmind-file (&optional no-zip)
  "* Creates or updates xmind file  based on the checked out file."
  (if (not esn-xmind-checkout-time)
      (message "Cannot update file;  no file has been checked out.")
    (when (file-exists-p esn-xmind-checkout-file)
      (if (file-exists-p (concat esn-xmind-checkout-file "~"))
          (delete-file (concat esn-xmind-checkout-file "~")))
      (rename-file esn-xmind-checkout-file (concat esn-xmind-checkout-file "~")))
    (let (
          (pwd (file-name-directory (buffer-file-name)))
          (xmind-file (expand-file-name esn-xmind-checkout-file))
          cmd
          )
      (if (and (not esn-xmind-use-7z) (not no-zip) (executable-find "zip") (executable-find "unzip"))
          (setq cmd (format "zip -r -1 %s *" xmind-file))
        (if (executable-find "7z")
            (setq cmd (format "7z a %s . -r -mx=1 -tzip" xmind-file))
          (if (and esn-w32 (file-exists-p (concat (getenv "PROGRAMFILES") "\\7-zip\\7z.exe")))
              (setq cmd (format "\"%s\\7-zip\\7z.exe\" a %s . -r -mx=1 -tzip" (getenv "PROGRAMFILES")
                                xmind-file)))))
      (message "%s" cmd)
      (cd esn-xmind-checkout-dir)
      (esn-command-to-string cmd)
      (cd pwd)
      (if (file-exists-p esn-xmind-checkout-file)
          (progn
            (esn-xmind-cancel-checkout)
            (when no-zip
              (esn-message "7z works; setting for later use.")
              (setq esn-xmind-use-7z 't)))
        (if (string-match " -1 " cmd)
            (if (not no-zip)
                (progn
                  (esn-alert "Program zip is not working, trying 7z if available")
                  (esn-xmind-update-xmind-file 't))
              (esn-xmind-cancel-checkout)
              (if (file-exists-p (concat esn-xmind-checkout-file "~"))
                  (rename-file (concat esn-xmind-checkout-file "~")
                               esn-xmind-checkout-file)))
          (esn-xmind-cancel-checkout)
          (if (file-exists-p (concat esn-xmind-checkout-file "~"))
              (rename-file (concat esn-xmind-checkout-file "~")
                           esn-xmind-checkout-file)))))))

;;;###autoload
(defun esn-xmind-strip-project (&optional file)
  "* Strip Project of Labels, and Makers"
  (interactive)
  (esn-xmind-cancel-checkout)
  (let (
        (f (or file (esn-xmind-default-map-name))))
    (if (not (file-exists-p f))
        (esn-error "Cannot find project file %s" f)
      (let (
            (content (esn-xmind-get-content f))
            (manifest (esn-xmind-get-manifest f)))
        (esn-xmind-checkout f)
        (esn-message "Stripping %s" esn-xmind-checkout-file)
        (while (string-match "<marker-refs>.*?</marker-refs>" content)
          (setq content (replace-match "" 't 't content)))
        (while (string-match "<labels>.*?</labels>" content)
          (setq content (replace-match "" 't 't content)))
        ;; Delete Thumbnail (since we can't update it).
        (while (string-match "<file-entry[^\n>]*?Thumbnail[^\n>]*?>"
                             manifest)
          (setq manifest (replace-match "" nil nil manifest)))
        (while (string-match "\n" manifest)
          (setq manifest (replace-match " " nil nil manifest)))
        (while (string-match "  +" manifest)
          (setq manifest (replace-match " " nil nil manifest)))
        (when (file-exists-p (format "%s/Thumbnails" esn-xmind-checkout-dir))
          (esn-xmind-del-tmp-dir (format "%s/Thumbnails" esn-xmind-checkout-dir)))
        (esn-xmind-update-xmind-part content "content.xml" f)
        (esn-xmind-update-xmind-part manifest "META-INF/manifest.xml" f)
        ;; Change Xmind file
        (when (string-match ".xmind" esn-xmind-checkout-file)
          (setq esn-xmind-checkout-file (replace-match "-stripped.xmind" 't 't esn-xmind-checkout-file)))
        (esn-message "Save Stripped file to: %s" esn-xmind-checkout-file)
        (esn-xmind-update-xmind-file)))))
(provide 'esn-xmind)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-xmind.el ends here
