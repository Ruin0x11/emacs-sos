;;; sos.el --- StackOverflow Search

;; Copyright (C) 2014 Rudolf Olah <omouse@gmail.com>

;; Author: Rudolf Olah
;; URL: https://github.com/omouse/emacs-sos
;; Version: 0.1
;; Created: 2014-02-15
;; By: Rudolf Olah
;; keywords: tools, search, questions
;; Package-Requires: ((org "7"))

;; Emacs-SOS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; Emacs-SOS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs-SOS. If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'cl)
(require 'json)
(require 'url)
(require 'url-http)
(require 'tabulated-list)

(provide 'sos)

(defvar sos-results)

(defvar sos-question-list-window)
(defvar sos-answer-view-window)

(defvar sos-question-list-buffer)

(defvar sos-get-answers 'nil
  "If non-nil retrieve and SO's answers to SO's questions when building the search result buffer.
This will slow down the process.")

(defun sos-decode-html-entities (str)
  "Decodes HTML entities in a string."
  (let ((result str))
    (loop for entity in '(("&quot;" . "\"")
                          ("&apos;" . "'")
                          ("&#39;" . "'")
                          ("&hellip" . "...")
                          ("&amp;" . "&")
                          ("&gt;" . ">")
                          ("&lt;" . "<")
                          ("&#194;" . "Â")
                          ("&#178;" . "²"))
          do (setq result (replace-regexp-in-string (regexp-quote (car entity)) (cdr entity) result nil 'literal))
          )
    result))

(defun sos-uncompress-callback (&optional status)
  "Callback for url-retrieve that decompresses gzipped content in
the HTTP response. Code taken from
http://stackoverflow.com/a/4124056/9903

Modified for use with url-retrieve-synchronously by making the
`status' argument optional.

Returns the buffer of the uncompressed gzipped content."
  (let ((filename (make-temp-file "download" nil ".gz"))
        (coding-system-for-read  'binary)
        (coding-system-for-write 'binary))
    (search-forward "\n\n") ; Skip response headers.
    (write-region (point) (point-max) filename)
    (with-auto-compression-mode
      (find-file filename))
    (current-buffer)))

(defun sos-get-response-body (buffer)
  "Extract HTTP response body from HTTP response, parse it as JSON, and return the JSON object. `buffer' may be a buffer or the name of an existing buffer.

Modified based on fogbugz-mode, renamed from
`fogbugz-get-response-body':
https://github.com/omouse/fogbugz-mode"
  (set-buffer buffer)
  (switch-to-buffer buffer)
  (let* ((uncompressed-buffer (sos-uncompress-callback))
         (json-response (json-read)))
    (kill-buffer uncompressed-buffer)
    json-response))

(defun sos-insert-search-result (item)
  "Inserts the contents of StackOverflow JSON object, `item',
into the current buffer."
  (let ((id (cdr (assoc 'question_id item))))
    (insert (format "* %s: %s [[http://stackoverflow.com/q/%d][link]]\n"
		    (upcase (subseq (cdr (assoc 'item_type item)) 0 1))
		    (cdr (assoc 'title item))
		    (cdr (assoc 'question_id item)))
	    ":PROPERTIES:\n"
	    ":ID: " (int-to-string id) "\n"
	    ":SO_TAGS: "
	    (reduce
	     (lambda (x y) (format "%s %s" x y))
	     (cdr (assoc 'tags item))) "\n"
	     ":END:\n"
	     (cdr (assoc 'excerpt item))
	     "\n\n** (Read more)\n"
	     (cdr (assoc 'body item))
	     (if (not sos-get-answers) ""
	       (sos-get-answers id))
	     "\n")))


;;;###autoload
(defun sos (query tag)
  "Searches StackOverflow for the given `query'. Displays excerpts from the search results.

API Reference: http://api.stackexchange.com/docs/excerpt-search"
  (interactive "sSearch StackOverflow: \nsTag: ")
  (let* ((api-url (concat "http://api.stackexchange.com/2.2/search/excerpts"
                          "?order=desc"
                          "&sort=votes"
                          "&tagged=" tag
                          "&q=" (url-hexify-string query)
                          "&site=stackoverflow"))
         (response-buffer (url-retrieve-synchronously api-url))
         (json-response (sos-get-response-body response-buffer)))
    ;; set up the buffer
    (switch-to-buffer-other-window (concat "*sos - " query "*"))
    (setq sos-question-list-window (selected-window))
    (setq sos-question-list-buffer (current-buffer))
    (sos-mode)
    (erase-buffer)
    ;; (org-mode)
    (visual-line-mode t)
    ;; (insert "#+TITLE: StackOverflow Search: " query "\n")

    (setq tabulated-list-format
            (vector
                    '("title" 80 t)
                    '("score" 13 t)
                    ;; '("modules" 30 t)
                    ;; '("name" 1 t)
                    ))

    (setq tabulated-list-sort-key nil)

    (let* ((results (cdr (assoc 'items json-response)))
           (count (length results))
            entries)

      (setq sos-results results)

      (dotimes (i count)
        (let* ((result (elt results i))
               (result-name (sos-decode-html-entities (cdr (assoc 'title result))))
               (result-score (int-to-string (cdr (assoc 'score result)))))

          (push (list i (vector

                         result-name
                         result-score

                         ))
                entries)))
      (setq tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print)

    ;; (sos-decode-html-entities)
    ;; ;; strip out HTML tags
    ;; (goto-char (point-min))
    ;; (while (search-forward "<span class=\"highlight\">" nil t)
    ;;   (replace-match "" nil t))
    ;; (goto-char (point-min))
    ;; (while (search-forward "</span>" nil t)
    ;;   (replace-match "" nil t))

    ;; (goto-char (point-min))
    ;; (org-global-cycle 1)
    ))


(defun sos-get-answers (id)
  "Get answers for SO's question defined by ID."
  (let* ((api-url (concat "http://api.stackexchange.com/2.2/"
                          "questions/"
                          (if (stringp id) id
                            (int-to-string id))
                          "/answers"
                          "?order=desc"
                          "&sort=votes"
                          "&filter=withbody"
                          "&site=stackoverflow"))
         (response-buffer (url-retrieve-synchronously api-url))
         (json-response (progn
                          (switch-to-buffer response-buffer)
                          (goto-char (point-min))
                          (sos-get-response-body response-buffer)))
         (answer-list (cdr (assoc 'items json-response)))
         (n-answers (length answer-list))
         (i 0)
         (sos-string
          (concat "Answers [" (int-to-string n-answers) "]\n")))
    (while (< i n-answers) 
      (let* ((answer (elt answer-list i))
             (accepted? (not (eq json-false (cdr (assoc 'is_accepted answer))))))
        (setq sos-string
              (concat sos-string
                      (concat
                       (propertize (concat "Answer " (int-to-string (1+ i)) (if accepted? " (Accepted)" "")
                                           " Score: " (int-to-string (cdr (assoc 'score answer)))
                                           "\n") 'face 'underline)
                       (cdr (assoc 'body answer))
                       "\n")

                      )
              i (1+ i))))
    sos-string))



(defun sos-current-result ()
  (elt sos-results (tabulated-list-get-id)))

;;;###autoload
(defun sos-answer ()
  "Get answers for SO question ID as defined in property block of the current question."
  (interactive)
  (let ((id (cdr (assoc 'question_id (sos-current-result)))))
    (if (not id)
        (message "Cannot see question ID at point.")
      (if (not (window-live-p sos-answer-view-window))
          (setq sos-answer-view-window
                (cond ((with-demoted-errors "Unable to split window: %S"
                         (split-window-vertically 20)))
                      (t ;; no splitting; just use the currently selected one
                       (selected-window)))))
      (select-window sos-answer-view-window)
      (switch-to-buffer "*sos - answer*")
      (sos-answer-mode)

      ;; an html2text bug removes "pre" since "p" is a substring
      (setq html2text-remove-tag-list (remove "p" html2text-remove-tag-list))
      (add-to-list 'html2text-remove-tag-list2 "p")

      (add-to-list 'html2text-format-tag-list '("code" . sos-answer-html2text-code) )
      (add-to-list 'html2text-format-tag-list '("pre" . sos-answer-html2text-pre) )

      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert (sos-get-answers id))
        (html2text)
        (goto-char (point-min))))))

(defvar sos-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'sos-answer)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    map)
  "Keymap used for sos-mode commands.")

(define-derived-mode sos-mode tabulated-list-mode "SOS")

(defface sos-answer-keywords-face
  '((((class color) (background light)) (:foreground "purple"))
    (((class color) (background dark)) :foreground "salmon"))
  "SOS answer mode face used to highlight keywords."
  :group 'sos-answer)

(defface sos-answer-code-block-face
  '((((class color) (background light)) (:foreground "purple"))
    (((class color) (background dark)) (:foreground "light gray" :background "#3E3D31")))
  "SOS answer mode face used to highlight keywords."
  :group 'sos-answer)

(defun sos-answer-html2text-code (p1 p2 p3 p4)
  (put-text-property p2 p3 'face 'sos-answer-keywords-face)
  (html2text-delete-tags p1 p2 p3 p4)
  )

(defun replace-region (from to string)
  "Replace the region specified by FROM and TO to STRING."
  (goto-char from)
  (insert string)
  (delete-char (- to from)))

(defun sos-answer-html2text-pre (p1 p2 p3 p4)
  (put-text-property p2 p3 'face 'sos-answer-code-block-face)
  ;; (html2text-delete-tags p1 p2 p3 p4)
  (replace-region p3 p4 "#+END_CODE")
  (replace-region p1 p2 "#+BEGIN_CODE\n")
  )

;; in the future?
;; http://jblevins.org/log/mmm

(defun sos-answer-quit-buffer ()
  (interactive)
  (unless (eq major-mode 'sos-answer-mode)
    (error "Must be in SOS answer mode"))
  (let ((curbuf (current-buffer)) (curwin (selected-window))
        (question-win))
    (walk-windows
     (lambda (win)
       ;; check whether the headers buffer window is visible
       (when (eq sos-question-list-buffer (window-buffer win))
         (setq question-win win))
       ;; and kill any _other_ (non-selected) window that shows the current
       ;; buffer
       (when
           (and
            (eq curbuf (window-buffer win)) ;; does win show curbuf?
            (not (eq curwin win))	    ;; but it's not the curwin?
            (not (one-window-p))) ;; and not the last one on the frame?
         (delete-window win))))  ;; delete it!
    ;; now, all *other* windows should be gone.
    ;; if the headers view is also visible, kill ourselves + window; otherwise
    ;; switch to the headers view
    (if (window-live-p question-win)
        ;; headers are visible
        (progn
          (kill-buffer-and-window) ;; kill the view win
          (setq sos-question-list-window nil)
          (select-window question-win)) ;; and switch to the headers win...
      ;; headers are not visible...
      (progn
        (kill-buffer)
        (setq sos-question-list-window nil)
        (when (buffer-live-p sos-question-list-buffer)
          (switch-to-buffer sos-question-list-buffer))))))

(defvar sos-answer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "q" 'sos-answer-quit-buffer)
    ;; (define-key map "n" 'sos-answer-next-question)
    ;; (define-key map "p" 'sos-answer-previous-question)
    map)
  "Keymap used for sos-mode commands.")

(define-derived-mode sos-answer-mode special-mode "SOS Answer")

(add-to-list 'evil-emacs-state-modes 'sos-mode)
;; (add-to-list 'evil-emacs-state-modes 'sos-answer-mode)
(evil-make-overriding-map sos-answer-mode-map 'normal t)
(ruin/window-movement-for-map sos-answer-mode-map)
(ruin/window-movement-for-map sos-mode-map)

(provide 'sos)

;;; sos.el ends here
