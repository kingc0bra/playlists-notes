;;; playlists.el --- Utilities for managing curated playlists.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  JT

;; Author: JT <jt@totallyjazzed.com>
;; Keywords: music, playlists

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; playlists.el provides the project configuration for publishing a
;; static html site.

;; For a complete list of available project configuration parameters,
;; see https://orgmode.org/manual/Publishing-options.html

;; For reference, see
;; https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html

;;; Code:
(require 'ox-publish)

(defvar playlists/project-root ""
  "The project's root directory. Defaults to relative paths to the current directory.")

(defvar playlists/publish-directory
  (concat (file-name-as-directory playlists/project-root) "public_html")
  "The directory where the project will be published.")

(defun li (text)
  "Wrap TEXT as an HTML list item."
  (format "\n  <li>%s</li>" text ))

(defun link (href label)
  "Create a link to HREF with the given LABEL."
  (format "<a href=\"%s\">%s</a>" href label))

(defun playlists/format-headline (todo todo-type priority text tags info)
  "A custom heading formatter.

See `org-html-format-headline-function' for details."
  (let* ((date (org-macro--get-property "date" text))
         (heady_id (org-macro--get-property "heady_id" text))
         (lma_id (org-macro--get-property "lma_id" text))
         (relisten_id (org-macro--get-property "relisten_source_id" text))
         (tags (if tags (org-html--tags tags info) ""))
         (mod_text
          (format "<span class=\"heading\">\n  %s</span><span class=\"tags\">%s</span><span class=\"sub-heading\">\n<ul>%s%s%s%s%s\n</ul></span>"
                  text
                  (concat (and tags "&#xa0;") tags)
                  (li (link (format "https://archive.org/details/%s" lma_id) "archive"))
                  (li (link (format "https://relisten.net/grateful-dead/%s?source=%s"
                                    (replace-regexp-in-string "-" "/" date)
                                    relisten_id)
                            "relisten"))
                  (li (link (concat "https://www.dead.net/show/"
                                    (downcase
                                     (replace-regexp-in-string
                                      " " "" (format-time-string "%B-%e-%Y"
                                                                 (apply 'encode-time
                                                                        (parse-time-string (concat date " 00:00")))))))
                            "dead.net")) ;may-8-1977
                  (li (link (format "http://headyversion.com/show/%s/grateful-dead/%s" heady_id date)
                                    "headyversions"))
                  (li (link (format "http://deadstats.com/shows/%s" date) "deadstats")) )))
    mod_text))

(setq org-export-global-macros '(
  ("tagline" . "@@html:<div class=\"tagline\">$1</div>@@")))

;; publish entire site
(add-to-list
 'org-publish-project-alist
 '("playlists" :components ("playlists-notes" "playlists-static")))

;; publish org documents
(add-to-list
 'org-publish-project-alist
 `("playlists-notes"
  :base-directory ,(concat (file-name-as-directory playlists/project-root) "org")
  :base-extension "org"
  :publishing-directory ,playlists/publish-directory
  :recursive t
  :publishing-function org-html-publish-to-html
  :html-format-headline-function playlists/format-headline
  :headlines-levels 2
  :section-numbers nil
  :with-author nil
  :with-creator nil
  :with-properties nil
  :with-tags t
  :with-timestamps nil
  :with-title t
  :with-toc nil
  :html-doctype "html5"
  :html-head-include-default-style nil
  :html-head-include-scripts nil
  :html-head "<link rel=\"stylesheet\" href=\"css/styles.css\">"
  :html-self-link-headlines nil
  :html-postamble "<div class=\"footer\"></div>"
  :html5-fancy t
  ))


;; publish static content
(add-to-list
 'org-publish-project-alist
 `("playlists-static"
   :base-directory ,(concat (file-name-as-directory playlists/project-root) "org")
   :base-extension "css\\|js"
   :publishing-directory ,playlists/publish-directory
   :recursive t
   :publishing-function org-publish-attachment
   ))

(provide 'playlists)
;;; playlists.el ends here
