;;; esn-macros.el --- Macros needed for compile.  
;; 
;; Filename: esn-macros.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Mon May  2 10:27:53 2011 (-0500)
;; Version: 
;; Last-Updated: Mon May  2 10:30:42 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 2
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

(defmacro esn-rec-post-hook (record function &optional append local)
  "Adds to records' post-command-hook"
  `(add-hook (quote ,(intern (concat "esn-" (downcase (esn-rec3 record)) "-post-command-hook")))
             ,function ,append ,local))

(defmacro esn-abbrev-post-hook (function &optional append local)
  "Adds to abbreviated record's post command hooks"
  (append (list 'progn)
          (mapcar (lambda(x)
                    `(esn-rec-post-hook ,x ,function ,append ,local))
                  esn-all-abbrev-recs)))

(defmacro esn-tos-post-hook (function &optional append local)
  "Adds to abbreviated record's post command hooks"
  (append (list 'progn)
          (mapcar (lambda(x)
                    `(esn-rec-post-hook ,x ,function ,append ,local))
                  '("theta" "omega" "sigma"))))

(defmacro esn-not-abbrev-post-hook (function &optional append local)
  "Adds to abbreviated record's post command hooks"
  (append (list 'progn)
          (mapcar (lambda(x)
                    `(esn-rec-post-hook ,x ,function ,append ,local))
                  esn-not-abbrev-rec)))


;; Modification hooks.

(defmacro esn-rec-modification-hook (record function &optional append local)
  "Adds to records' post-command-hook"
  `(add-hook (quote ,(intern (concat "esn-" (downcase (esn-rec3 record)) "-post-modification-hook")))
             ,function ,append ,local))

(defmacro esn-abbrev-modification-hook (function &optional append local)
  "Adds to abbreviated record's post command hooks"
  (append (list 'progn)
          (mapcar (lambda(x)
                    `(esn-rec-modification-hook ,x ,function ,append ,local))
                  esn-all-abbrev-recs)))

(defmacro esn-tos-modification-hook (function &optional append local)
  "Adds to abbreviated record's post command hooks"
  (append (list 'progn)
          (mapcar (lambda(x)
                    `(esn-rec-modification-hook ,x ,function ,append ,local))
                  '("theta" "omega" "sigma"))))

(defmacro esn-not-abbrev-modification-hook (function &optional append local)
  "Adds to abbreviated record's post command hooks"
  (append (list 'progn)
          (mapcar (lambda(x)
                    `(esn-rec-modification-hook ,x ,function ,append ,local))
                  esn-not-abbrev-rec)))

(defmacro esn-not-abbrev-modification-hook-2 (function exclude &optional append local)
  "Adds to abbreviated record's post command hooks"
  (append (list 'progn)
          (mapcar (lambda(x)
                    (unless (member-ignore-case x exclude)
                      `(esn-rec-modification-hook ,x ,function ,append ,local)))
                  esn-not-abbrev-rec)))


(defmacro esn-rec-pre-modification-hook (record function &optional append local)
  "Adds to records' post-command-hook"
  `(add-hook (quote ,(intern (concat "esn-" (downcase (esn-rec3 record)) "-pre-modification-hook")))
             ,function ,append ,local))

(defmacro esn-abbrev-pre-modification-hook (function &optional append local)
  "Adds to abbreviated record's post command hooks"
  (append (list 'progn)
          (mapcar (lambda(x)
                    `(esn-rec-pre-modification-hook ,x ,function ,append ,local))
                  esn-all-abbrev-recs)))

(defmacro esn-tos-pre-modification-hook (function &optional append local)
  "Adds to abbreviated record's post command hooks"
  (append (list 'progn)
          (mapcar (lambda(x)
                    `(esn-rec-pre-modification-hook ,x ,function ,append ,local))
                  '("theta" "omega" "sigma"))))

(defmacro esn-not-abbrev-pre-modification-hook (function &optional append local)
  "Adds to abbreviated record's post command hooks"
  (append (list 'progn)
          (mapcar (lambda(x)
                    `(esn-rec-pre-modification-hook ,x ,function ,append ,local))
                  esn-not-abbrev-rec)))

(defmacro esn-not-abbrev-pre-modification-hook-2 (function exclude &optional append local)
  "Adds to abbreviated record's post command hooks"
  (append (list 'progn)
          (mapcar (lambda(x)
                    (unless (member-ignore-case x exclude)
                      `(esn-rec-pre-modification-hook ,x ,function ,append ,local)))
                  esn-not-abbrev-rec)))


(provide 'esn-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-macros.el ends here
