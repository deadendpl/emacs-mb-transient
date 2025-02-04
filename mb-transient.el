;;; mb-transient.el --- Transient menu for MusicBrainz searches  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  Oliwier Czerwiński <oliwier.czerwi@proton.me>
;; Keywords: convenience
;; Package-Requires: (transient)
;; Version: 20250129
;; URL: https://github.com/deadendpl/emacs-mb-transient

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

;; A Transient menu for invoking MusicBrainz web searches.

;;; Code:

(require 'transient)

(defvar mb-transient-search-method "indexed"
  "A search method.")

(defvar mb-transient-type "release"
  "A search type.")

(defconst mb-transient-type-list
  '(annotation
    area
    artist
    cdstub
    doc
    editor
    event
    instrument
    label
    place
    recording
    release
    release_group
    series
    tag
    work)
  "List of all available search types.")

(defvar mb-transient-query nil
  "A search query.")

(defconst mb-transient-advanced-syntax-list
  '(added
    address
    aid
    alias
    area
    areaaccent
    arid
    artist
    artistaccent
    artistname
    asin
    barcode
    begin
    beginarea
    bio
    catno
    code
    comment
    country
    creditname
    date
    description
    discid
    discids
    discidsmedium
    dur
    editor
    eid
    end
    endarea
    ended
    entity
    event
    eventaccent
    firstreleasedate
    format
    gender
    id
    iid
    instrument
    instrumentaccent
    ipi
    isni
    iso
    iso1
    iso2
    iso3
    isrc
    iswc
    label
    labelaccent
    laid
    lang
    lat
    long
    mediums
    name
    number
    packaging
    pid
    place
    placeaccent
    position
    primary_alias
    primarytype
    qdur
    quality
    recording
    recording_count
    recordingaccent
    reid
    release
    release_count
    releaseaccent
    releasegroup
    releasegroupaccent
    releases
    rgid
    rid
    script
    secondarytype
    series
    seriesaccent
    sid
    sortname
    status
    tag
    text
    tid
    title
    tnum
    tracks
    tracksmedium
    tracksrelease
    type
    video
    wid
    work
    workaccent)
  "List of all advanced syntax keywords.")

(defcustom mb-transient-exit-hook nil
  "Hook run after leaving `mb-transient'."
  :type 'hook)

(defun mb-transient--set-search-method (val)
  "Sets search method to VAL."
  (interactive "s")
  (setq mb-transient-search-method val))

(defun mb-transient--set-search-method-indexed ()
  "Sets search method to indexed."
  (interactive)
  (mb-transient--set-search-method "indexed"))
(defun mb-transient--set-search-method-advanced ()
  "Sets search method to advanced."
  (interactive)
  (mb-transient--set-search-method "advanced"))
(defun mb-transient--set-search-method-direct ()
  "Sets search method to direct."
  (interactive)
  (mb-transient--set-search-method "direct"))

(defun mb-transient--set-type (val)
  "Sets search type to VAL."
  (interactive "s")
  (setq mb-transient-type val))

(defun mb-transient--set-query (arg)
  "Sets search query.
With ARG, it prefills prompt with `mb-transient-query'."
  (interactive "P")
  (setq mb-transient-query (read-string "Query: "
                              (if arg mb-transient-query nil))))

(defun mb-transient--open ()
  "Combines `mb-transient-query', `mb-transient-type', and
`mb-transient-search-method' into a search URL that gets opened
with `browse-url'."
  (interactive)
  (browse-url
   (concat "https://musicbrainz.org/search?query=" mb-transient-query "&type=" mb-transient-type "&method=" mb-transient-search-method))
  (run-hooks 'mb-transient-exit-hook)
  )

(defun mb-transient--advanced-query-set ()
  "Returns a string valid for doing advanced searches for a search URL."
  (interactive)
  (let ((syntax (completing-read (format-prompt "Advanced syntax" nil)
                                 mb-transient-advanced-syntax-list nil t)))
    (when syntax
      (concat syntax ":"
              (read-string (format-prompt syntax nil)))
      )))

(defun mb-transient--advanced-method-setup ()
  "Sets search query to output of `mb-transient--advanced-query-set'."
  (interactive)
  (mb-transient--set-search-method-advanced)
  (setq mb-transient-query (mb-transient--advanced-query-set)))

(defun mb-transient--desc-setup (x)
  "Sets up descriptions in `mb-transient'."
  (pcase x
    ('search-method
     (concat (propertize "Search method" 'face 'transient-heading) " ("
             (propertize mb-transient-search-method 'face 'font-lock-variable-name-face) ")"))
    ('type
     (concat (propertize "Type" 'face 'transient-heading) " ("
             (propertize mb-transient-type 'face 'font-lock-variable-name-face) ")"))
    ('query
     (concat (propertize "Query" 'face 'transient-heading)
             (unless (or (string= mb-transient-query "") (not mb-transient-query))
               (concat " (" (propertize mb-transient-query 'face 'font-lock-variable-name-face) ")"))))
    ('advanced-query
     (concat "Advanced Query Syntax (fills " (propertize "Query" 'face 'transient-heading) ")"))))

;;;###autoload (autoload 'mb-transient "mb-transient" "Search in MusicBrainz" t)
(transient-define-prefix mb-transient ()
  "Search in MusicBrainz"
  ["Search method" :description (lambda () (mb-transient--desc-setup 'search-method))
   ("si" "Indexed" mb-transient--set-search-method-indexed :transient t)
   ("sa" (lambda () (mb-transient--desc-setup 'advanced-query)) mb-transient--advanced-method-setup :transient t)
   ("sd" "Direct Database Search" mb-transient--set-search-method-direct :transient t)]
  ["Type" :description (lambda () (mb-transient--desc-setup 'type))
   :pad-keys t
   [("t C-a" "annotation" (lambda () (interactive) (mb-transient--set-type "annotation")) :transient t)
    ("tA" "area" (lambda () (interactive) (mb-transient--set-type "area")) :transient t)
    ("ta" "artist" (lambda () (interactive) (mb-transient--set-type "artist")) :transient t)
    ("tc" "cdstub" (lambda () (interactive) (mb-transient--set-type "cdstub")) :transient t)
    ("td" "doc" (lambda () (interactive) (mb-transient--set-type "doc")) :transient t)
    ("tE" "editor" (lambda () (interactive) (mb-transient--set-type "editor")) :transient t)
    ("te" "event" (lambda () (interactive) (mb-transient--set-type "event")) :transient t)
    ("ti" "instrument" (lambda () (interactive) (mb-transient--set-type "instrument")) :transient t)]
   [("tl" "label" (lambda () (interactive) (mb-transient--set-type "label")) :transient t)
    ("tp" "place" (lambda () (interactive) (mb-transient--set-type "place")) :transient t)
    ("tR" "recording" (lambda () (interactive) (mb-transient--set-type "recording")) :transient t)
    ("tr" "release" (lambda () (interactive) (mb-transient--set-type "release")) :transient t)
    ("tg" "release_group" (lambda () (interactive) (mb-transient--set-type "release_group")) :transient t)
    ("ts" "series" (lambda () (interactive) (mb-transient--set-type "series")) :transient t)
    ("tt" "tag" (lambda () (interactive) (mb-transient--set-type "tag")) :transient t)
    ("tw" "work" (lambda () (interactive) (mb-transient--set-type "work")) :transient t)]
   ]
  ["Query" :description (lambda () (mb-transient--desc-setup 'query))
   ("<SPC>" "Enter query" (lambda () (interactive) (mb-transient--set-query nil)) :transient t)
   ("C-<SPC>" "Enter query (prefilled)" (lambda () (interactive) (mb-transient--set-query t)) :transient t)
   ("C-y" "Yank" (lambda () (interactive) (setq mb-transient-query (car kill-ring))) :transient t)]
  ["The rest"
   ("e" "Open" mb-transient--open)
   ("q" "Quit" mb-transient--quit)])

(defun mb-transient--quit ()
  "Quits transient menu."
  (interactive)
  (transient-quit-one)
  (run-hooks 'mb-transient-exit-hook)
  )

(provide 'mb-transient)

;;; mb-transient.el ends here
