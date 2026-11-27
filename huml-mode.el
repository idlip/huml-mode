;;; huml-mode.el --- Major mode for HUML files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dilip

;; Author: Dilip <dilip@fossunited.org>
;; Assisted-by: Claude Opus 5.5
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: languages, data
;; URL: https://github.com/idlip/huml-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Major mode for HUML, a human-oriented markup language.  Files that
;; end in ".huml" open in `huml-mode' by default.  Turn it on in any
;; other buffer with `M-x huml-mode'.

;;; Code:

(require 'flymake)

(defconst huml--key-regexp
  (rx (or (seq (in "A-Za-z") (* (in "A-Za-z0-9_-")))
          (seq "\""
               (* (or (seq "\\" anychar) (not (in "\"\\\n"))))
               "\"")))
  "Match a bare or a quoted key.")

(defconst huml--escape-regexp
  (rx "\\" (in "bfnrtv\"\\"))
  "Match an escape sequence of a single-line string.")

(defvar huml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    table)
  "Syntax table for `huml-mode'.")

(defun huml--state-at (position)
  "Return the parse state at POSITION and keep point and the match data.
`syntax-ppss' moves point, and the propertize function changes the match
data.  Both would break a search loop that calls it."
  (save-match-data
    (save-excursion
      (syntax-ppss position))))

(defun huml--blank-line-p ()
  "Return non-nil when the current line holds only spaces."
  (save-excursion
    (beginning-of-line)
    (looking-at-p (rx (* " ") eol))))

(defun huml--code-line-p ()
  "Return non-nil when the current line is code.
Blank lines, comment lines and lines that start inside a multi-line string
are not code."
  (save-excursion
    (beginning-of-line)
    (not (or (looking-at-p (rx (* " ") (or eol "#")))
             (eq (nth 3 (syntax-ppss)) t)))))

;;; Multi-line strings

(defconst huml--multiline-open-regexp
  (rx bol (group (* " "))
      (or (seq "-" (+ " "))
          (seq (regexp huml--key-regexp) (* " ") ":" (* " ")))
      (group "\"\"\"") (* " ") (? "#" (* nonl)) eol)
  "Match the line that opens a multi-line string.
Group 1 is the indentation and group 2 is the opening quotes.  Bad spacing
still opens a string, so fixing the spacing never changes the syntax.")

(defun huml--put-fence (position)
  "Give the character at POSITION the generic string fence syntax."
  (put-text-property position (1+ position) 'syntax-table
                     (string-to-syntax "|")))

(defun huml--multiline-close (indentation)
  "Move point past the line that closes a block indented by INDENTATION.
Return the position of its last quote, or nil when the block is open."
  (when (re-search-forward
         (rx bol (literal indentation) (group "\"\"\"") (* " ")
             (? "#" (* nonl)) eol)
         nil t)
    (1- (match-end 1))))

(defun huml--syntax-propertize (start end)
  "Fence each multi-line string between START and END.
The fences make the body inert, so a quote or a hash inside it is text."
  (goto-char start)
  (while (and (< (point) end)
              (re-search-forward huml--multiline-open-regexp end t))
    (let ((indentation (match-string 1)))
      (huml--put-fence (match-beginning 2))
      (if-let* ((close (huml--multiline-close indentation)))
          (huml--put-fence close)
        (goto-char (point-max))))))

(defun huml--extend-propertize-region (start end)
  "Move START back to the line that opens the string that holds it.
The end of the region, END, stays as it is.
A string must be fenced in one pass, or its closing fence is lost.  The
fences before START are already set, so the parse state finds the opener."
  (let ((state (huml--state-at start)))
    (when (eq (nth 3 state) t)
      (save-excursion
        (goto-char (nth 8 state))
        (cons (line-beginning-position) end)))))

;;; Font-lock

(defconst huml--key-colon-regexp
  (rx (or bol " ") (group (regexp huml--key-regexp)) (group (or "::" ":")))
  "Match a key and its colons.
Group 1 is the key and group 2 is the colons.")

(defconst huml--number-regexp
  (rx (or bol " ")
      (group
       (or (seq (? (in "+-"))
                (or (seq "0x" (+ (in "0-9A-Fa-f_")))
                    (seq "0o" (+ (in "0-7_")))
                    (seq "0b" (+ (in "01_")))
                    (seq (+ (in "0-9_"))
                         (? "." (* (in "0-9_")))
                         (? "e" (? (in "+-")) (+ (in "0-9"))))
                    "inf"))
           "nan"))
      (or eol " " ","))
  "Match a number.  Group 1 is the number.")

(defconst huml--constant-regexp
  (rx (or bol " ")
      (group (or "true" "false" "null" "[]" "{}"))
      (or eol " " ","))
  "Match a constant.  Group 1 is the constant.")

(defun huml--match-key (limit)
  "Find the next key before LIMIT that is code, not string or comment text."
  (let (found)
    (while (and (not found) (re-search-forward huml--key-colon-regexp limit t))
      (setq found (not (nth 8 (huml--state-at (match-beginning 1))))))
    found))

(defun huml--match-escape (limit)
  "Find the next escape sequence before LIMIT in a single-line string."
  (let (found)
    (while (and (not found) (re-search-forward huml--escape-regexp limit t))
      (setq found (eq (nth 3 (huml--state-at (match-beginning 0))) ?\")))
    found))

(defconst huml--font-lock-keywords
  `((,(rx bol "%HUML" (* nonl)) 0 'font-lock-constant-face)
    (huml--match-key (1 'font-lock-property-use-face t)
                     (2 'font-lock-delimiter-face))
    (huml--match-escape 0 'font-lock-escape-face t)
    (,(rx bol (* " ") (group "-") " " (? (group "::")))
     (1 'font-lock-delimiter-face)
     (2 'font-lock-delimiter-face nil t))
    (,huml--number-regexp 1 'font-lock-number-face)
    (,huml--constant-regexp 1 'font-lock-constant-face)
    ("," 0 'font-lock-delimiter-face))
  "Font-lock keywords for `huml-mode'.
Strings and comments come from the syntax table.")

;;; Indentation and formatting

(defun huml--opens-block-p ()
  "Return non-nil when the current line ends with `::' before any comment.
A line that opens a multi-line string ends inside it, so it opens no block."
  (save-excursion
    (end-of-line)
    (let ((state (syntax-ppss)))
      (unless (eq (nth 3 state) t)
        (when (nth 4 state)
          (goto-char (nth 8 state)))
        (skip-chars-backward " ")
        (looking-back (rx "::") (line-beginning-position))))))

(defun huml--maximum-indentation ()
  "Return the deepest indentation the current line may have.
It is the indentation of the previous line of code, plus two when that line
opens a block."
  (save-excursion
    (beginning-of-line)
    (let ((found nil))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (setq found (huml--code-line-p)))
      (if found
          (+ (current-indentation) (if (huml--opens-block-p) 2 0))
        0))))

(defun huml--tab-cycles-p ()
  "Return non-nil when this command is a `TAB' that should cycle levels.
With `tab-always-indent' set to `complete', a `TAB' after the first
character of the line must leave the line alone so that it can complete."
  (and (eq this-command 'indent-for-tab-command)
       (not (and (eq tab-always-indent 'complete)
                 (> (point) (+ (line-beginning-position)
                               (current-indentation)))))))

(defun huml--indent-string-line (state)
  "Indent a blank line inside the multi-line string described by STATE.
Other lines in the string are content and stay as they are."
  (when (huml--blank-line-p)
    (indent-line-to
     (if (save-excursion (forward-line -1) (> (point) (nth 8 state)))
         (save-excursion (forward-line -1) (current-indentation))
       (+ 2 (save-excursion (goto-char (nth 8 state))
                            (current-indentation)))))))

(defun huml--shift-string-body (indentation delta)
  "Shift the body and closing line of the string opened here by DELTA.
INDENTATION is the old indentation of the opening line, which the closing
line still has.  Empty lines stay empty, so the content does not change."
  (save-excursion
    (end-of-line)
    (when-let* ((close (save-excursion (huml--multiline-close indentation))))
      (setq close (copy-marker close))
      (forward-line 1)
      (while (<= (point) close)
        (unless (eolp)
          (indent-line-to (max 0 (+ (current-indentation) delta))))
        (forward-line 1))
      (set-marker close nil))))

(defun huml--indent-code-line ()
  "Indent the current line of code, comment or blank line.
A blank line gets the deepest valid indentation.  Another line is rounded
up to an even width and held to that limit, so `indent-region' keeps the
levels already chosen.  `indent-for-tab-command' on such a line steps one
level up to the limit, then back down.  A line that opens a multi-line
string takes its body and closing line with it."
  (let* ((maximum (huml--maximum-indentation))
         (indentation (current-indentation))
         (level (* 2 (ceiling indentation 2))))
    (indent-line-to
     (cond ((huml--blank-line-p) maximum)
           ((huml--tab-cycles-p)
            (if (or (zerop level) (> level maximum)) maximum (- level 2)))
           (t (min level maximum))))
    (when (and (/= indentation (current-indentation))
               (save-excursion
                 (beginning-of-line)
                 (looking-at-p huml--multiline-open-regexp)))
      (huml--shift-string-body (make-string indentation ?\s)
                               (- (current-indentation) indentation)))))

(defun huml--indent-line ()
  "Indent the current line as HUML requires.
Point keeps its place in the text, as `indent-for-tab-command' expects."
  (let* ((state (save-excursion (beginning-of-line) (syntax-ppss)))
         (offset (max 0 (- (point) (save-excursion (back-to-indentation)
                                                   (point))))))
    (if (eq (nth 3 state) t)
        (huml--indent-string-line state)
      (huml--indent-code-line))
    (goto-char (+ offset (save-excursion (back-to-indentation) (point))))))

(defconst huml--spacing-rules
  `((,(rx (group (+ " ")) eol) 1 "" "Trailing space" t)
    (,(rx (group "\t")) 1 nil "Tab character")
    (,(rx (group "#") (not (in " \n"))) 1 "# "
     "Expected a space after the hash")
    (,(rx (or (not (in "\n -")) (seq (not (in "\n ")) "-"))
          (group (+ " ")) (or "::" ":"))
     1 ""
     "Unexpected space before the colon")
    (,(rx (or "::" ":") (group (* " ")) (not (in " \t\n#:"))) 1 " "
     "Expected one space after the colon")
    (,(rx bol (* " ") "-" (group (>= 2 " ")) (not (in " \t\n#"))) 1 " "
     "Expected one space after the dash")
    (,(rx (group (+ " ")) ",") 1 "" "Unexpected space before the comma")
    (,(rx "," (group (* " ")) (not (in " \t\n#"))) 1 " "
     "Expected one space after the comma"))
  "Spacing rules of the specification, used by the formatter and Flymake.
Each rule lists a regexp, the group to check, the text that group must have
\(nil when no fix is safe), a message, and a flag that is non-nil when the
rule also applies inside comments.  No rule applies inside strings.")

(defun huml--broken-spacing (rule beginning end)
  "Return the match data of each place where RULE is broken.
The search runs from BEGINNING to END."
  (pcase-let ((`(,regexp ,group ,replacement ,_message ,in-comments) rule)
              (found nil))
    (goto-char beginning)
    (while (re-search-forward regexp end t)
      (let ((state (huml--state-at (match-beginning group))))
        (unless (or (nth 3 state)
                    (and (nth 4 state) (not in-comments))
                    (and replacement (string= (match-string group)
                                              replacement)))
          (push (match-data t) found))))
    (nreverse found)))

(defun huml--fix-spacing (beginning end)
  "Fix the spacing between BEGINNING and END that has a safe fix.
The fixes run from the end back, so earlier positions stay valid."
  (dolist (rule huml--spacing-rules)
    (pcase-let ((`(,_regexp ,group ,replacement . ,_) rule))
      (when replacement
        (dolist (data (nreverse (huml--broken-spacing rule beginning end)))
          (set-match-data data)
          (replace-match replacement nil t nil group))))))

(defun huml--unterminated-string-diagnostics ()
  "Return a list of (BEGIN END MESSAGE) for each unterminated string.
A single-line string that does not close on its line makes the quotes after
it swap meaning, so code reads as string and string as code."
  (let (found)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((state (huml--state-at (line-end-position))))
        (when (and (eq (nth 3 state) ?\")
                   (>= (nth 8 state) (line-beginning-position)))
          (push (list (nth 8 state) (line-end-position) "Unterminated string")
                found)))
      (forward-line 1))
    found))

(defun huml--indent-region (beginning end)
  "Fix the spacing and indent every line between BEGINNING and END.
This is the value of `indent-region-function', so \\[indent-region] and
\\[indent-for-tab-command] on a region format the text.  Signal an error
when a string is unterminated, because the spacing fixes would then
change the content of other strings."
  (when-let* ((problem (car (last (save-excursion
                                    (huml--unterminated-string-diagnostics))))))
    (user-error "Unterminated string on line %d"
                (line-number-at-pos (car problem))))
  (let ((limit (copy-marker end))
        (this-command nil))
    (save-excursion
      (huml--fix-spacing beginning limit)
      (indent-region-line-by-line beginning limit))
    (set-marker limit nil)))

;;; Navigation

(defconst huml--top-level-regexp
  (rx bol (group (regexp huml--key-regexp)) ":")
  "Match a top-level key.  Group 1 is the key.")

(defun huml--beginning-of-defun (&optional arg)
  "Move to the start of the ARGth previous top-level entry.
A negative ARG moves to the next entries.  Return non-nil when point moved."
  (let ((count (or arg 1)))
    (if (> count 0)
        (progn (unless (bolp) (end-of-line))
               (re-search-backward huml--top-level-regexp nil 'move count))
      (end-of-line)
      (when (re-search-forward huml--top-level-regexp nil 'move (- count))
        (goto-char (match-beginning 0))))))

(defun huml--end-of-defun ()
  "Move to the end of the top-level entry that starts at point."
  (end-of-line)
  (when (re-search-forward huml--top-level-regexp nil 'move)
    (goto-char (match-beginning 0))))

(defconst huml--outline-regexp
  (rx (* " ") (regexp huml--key-regexp) "::" (* " ") (? "# " (* nonl)) eol)
  "Value of `outline-regexp' in `huml-mode'.  Keys of blocks are headings.")

(defun huml--outline-level ()
  "Return the outline level of the heading at point."
  (1+ (/ (current-indentation) 2)))

;;; Checking

(defun huml--spacing-diagnostics ()
  "Return a list of (BEGIN END MESSAGE) for each spacing rule broken."
  (let (found)
    (dolist (rule huml--spacing-rules)
      (dolist (data (huml--broken-spacing rule (point-min) (point-max)))
        (push (list (nth 0 data) (nth 1 data) (nth 3 rule)) found)))
    found))

(defun huml--escape-diagnostics ()
  "Return a list of (BEGIN END MESSAGE) for each invalid escape sequence."
  (let (found)
    (goto-char (point-min))
    (while (re-search-forward (rx "\\" anychar) nil t)
      (when (and (eq (nth 3 (huml--state-at (match-beginning 0))) ?\")
                 (not (string-match-p (rx bos (regexp huml--escape-regexp) eos)
                                      (match-string 0))))
        (push (list (match-beginning 0) (match-end 0)
                    "Invalid escape sequence")
              found)))
    found))

(defun huml--indentation-diagnostics ()
  "Return a list of (BEGIN END MESSAGE) for each bad indentation."
  (let (found)
    (goto-char (point-min))
    (while (not (eobp))
      (when (huml--code-line-p)
        (let* ((indentation (current-indentation))
               (message (cond ((/= 0 (% indentation 2))
                               "Indentation must be a multiple of two")
                              ((> indentation (huml--maximum-indentation))
                               "Unexpected indentation"))))
          (when message
            (push (list (point) (+ (point) indentation) message) found))))
      (forward-line 1))
    found))

(defun huml-flymake (report-function &rest _arguments)
  "Report spacing, indentation and escape errors through REPORT-FUNCTION.
This is a Flymake backend.  It checks the rules of the HUML specification
that need no parser, and it needs no external program."
  (funcall report-function
           (mapcar (pcase-lambda (`(,beginning ,end ,message))
                     (flymake-make-diagnostic
                      (current-buffer) beginning end :error message))
                   (save-excursion
                     (save-restriction
                       (widen)
                       (append (huml--spacing-diagnostics)
                               (huml--unterminated-string-diagnostics)
                               (huml--escape-diagnostics)
                               (huml--indentation-diagnostics)))))))

;;; Editing helpers

(defun huml-insert-list-item (&optional dict)
  "Start a list item on a line of its own, at the deepest valid indentation.
That is a first item after a line that ends in `::' and a sibling after
another item.  Press \\[indent-for-tab-command] to move it up a level.  When
`tab-always-indent' is `complete', move point into the indentation first.
With prefix argument DICT, write `- ::' for an item that holds a dict."
  (interactive "*P")
  (unless (huml--blank-line-p)
    (end-of-line)
    (newline))
  (indent-according-to-mode)
  (end-of-line)
  (insert (if dict "- ::" "- ")))

(defvar-keymap huml-mode-map
  :doc "Keymap for `huml-mode'."
  "C-c C-l" #'huml-insert-list-item)

;;;###autoload
(define-derived-mode huml-mode prog-mode "HUML"
  "Major mode for editing HUML files.

Indentation is fixed at two spaces.  \\[indent-region] formats the region:
it fixes the spacing and the indentation.  Mark the whole buffer with
\\[mark-whole-buffer] first to format all of it.  \\[huml-insert-list-item]
starts a list item.

\\{huml-mode-map}"
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx "#" (* " ")))
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'huml--indent-line)
  (setq-local indent-region-function #'huml--indent-region)
  (setq-local font-lock-defaults '(huml--font-lock-keywords))
  (setq-local syntax-propertize-function #'huml--syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'huml--extend-propertize-region nil t)
  (setq-local beginning-of-defun-function #'huml--beginning-of-defun)
  (setq-local end-of-defun-function #'huml--end-of-defun)
  (setq-local imenu-generic-expression `((nil ,huml--top-level-regexp 1)))
  (setq-local outline-regexp huml--outline-regexp)
  (setq-local outline-level #'huml--outline-level)
  (add-hook 'flymake-diagnostic-functions #'huml-flymake nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.huml\\'" . huml-mode))

(provide 'huml-mode)
;;; huml-mode.el ends here
