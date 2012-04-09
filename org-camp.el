;;; header-camp.el --- import TODO items from basecamp into org-mode

;; Copyright (C) 2011
;; Author: Kirk Kelsey

;; This file is *not* a part of GNU Emacs.

;;; License

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

(require 'url)


(defconst org-camp-version "0.1" "Version of `org-camp'.")

(defgroup org-camp nil
  "Org-mode package for importing todo lists from Basecamp.com."
  :group 'org)

(defcustom org-camp-auth-token ""
  "User authentication token."
  :group 'org-camp
  :type '(string))

(defcustom org-camp-file-heading ""
  "Heading text to insert when generating org-mode file from
Basecamp. This can be customized to add file level org-mode
variables as necessary."
  :group 'org-camp
  :type '(string))

(defcustom org-camp-subdomain ""
  "The subdomain that should preceed .basecamphq.com in URLs."
  :group 'org-camp
  :type '(string))

(defcustom org-camp-target-file (concat org-directory "/basecamp.org")
  "The file into which Basecamp todos are imported."
  :group 'org-camp
  :type '(file))

(defun org-camp-parse-todo-list-xml (node)
  "Inserts the org-mode version of XML `node' into the current
buffer."
  (cond 
   ((null node) nil)
   ((stringp node) nil)
   ((equal (xml-node-name node) 'todo-lists)
    (mapc 'org-camp-parse-todo-list-xml (xml-node-children node)))
   ((equal (xml-node-name node) 'todo-list)
    (mapc 'org-camp-parse-todo-list-xml (xml-node-children node)))
   ((equal (xml-node-name node) 'todo-items)
    (mapc 'org-camp-parse-todo-list-xml (xml-node-children node)))
   ((equal (xml-node-name node) 'name)
    (insert (format "* %s\n" (car (xml-node-children node)))))
   ;;; Since we're simply streaming xml->org, we can't handle the description
   ;;; arriving before the name.
   ;; ((equal (xml-node-name node) 'description)
   ;;  (insert (car (xml-node-children node))))
   ((equal (xml-node-name node) 'todo-item)
    (mapc 'org-camp-parse-todo-list-xml (xml-node-children node)))
   ((string= (xml-node-name node) 'content)
    (insert (format "*** TODO %s\n"
                    (car (xml-node-children node)))))))

(defun org-camp-parse-todo-lists-buffer (buffer)
  "Parses the contents of `buffer' containing the XML description
of Basecamp todo lists and writes org-mode TODOs into the file
specified by `org-camp-target-file'."
  (with-temp-file org-camp-target-file
    (insert org-camp-file-heading)
    (let ((xml-nodes (xml-parse-region 1 (buffer-size buffer) buffer)))
      (mapc 'org-camp-parse-todo-list-xml xml-nodes))))

(defun org-camp-todo-url-callback (status)
  "Callback function for `org-camp-pull'. Calls
`org-camp-parse-todo-lists-buffer' on current buffer then reloads
the agenda."
  (org-camp-parse-todo-lists-buffer (current-buffer)))

(defun org-camp-url (request)
  "Returns a Basecamp URL build from `org-camp-subdomain' and the
provided `request' string."
  (cond ((= (length org-camp-subdomain) 0)
         (error "The value of `org-camp-subdomain' must be customized"))
        (t
         (concat "https://" org-camp-subdomain ".basecamphq.com/" request))))

;;;###autoload
(defun org-camp-pull ()
  "Pulls Basecamp TODO items into `org-camp-target-file'."
  (interactive)
  (if (= (length org-camp-auth-token) 0)
      (error "The `org-camp-auth-token' must be customized"))
  (save-excursion
    (let ((url-request-method "GET")
          (url-request-extra-headers
           `(("Accept" . "application/xml")
             ("Content-Type" . "application/xml")
             ("Authorization" .
              ,(concat "Basic "
                       (base64-encode-string
                        (concat org-camp-auth-token ":X")))))))
      ;; This could optionally take a "?responsible_party=#{id}"
      (url-retrieve (org-camp-url "todo_lists.xml")
                    'org-camp-todo-url-callback))))

(provide 'org-camp)
;;; org-camp ends here
