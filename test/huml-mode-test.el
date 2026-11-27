;;; huml-mode-test.el --- Tests for huml-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with: just test

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'ert-font-lock)
(require 'imenu)
(require 'huml-mode)

(defconst huml-mode-test--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the test file.")

(defun huml-mode-test--fixture (name)
  "Return the file name of the fixture NAME."
  (expand-file-name (concat "fixtures/" name) huml-mode-test--directory))

(defun huml-mode-test--state-after (text)
  "Return the `syntax-ppss' state after the first match of TEXT."
  (goto-char (point-min))
  (search-forward text)
  (syntax-ppss))

;;; Mode setup and syntax

(ert-deftest huml-mode-test-activates-for-huml-file ()
  "Opening a .huml file turns on `huml-mode' with comments set up."
  (with-temp-buffer
    (setq buffer-file-name "example.huml")
    (set-auto-mode)
    (should (eq major-mode 'huml-mode))
    (should (equal comment-start "# "))
    (should (equal comment-end ""))
    (should (eq (char-syntax ?#) ?<))
    (should (eq (char-syntax ?\n) ?>))
    (should (eq (char-syntax ?\") ?\"))
    (insert "key: \"value\" # note")
    (should (nth 4 (syntax-ppss)))
    (goto-char (point-min))
    (search-forward "\"va")
    (should (nth 3 (syntax-ppss)))))

(defconst huml-mode-test--multiline-text
  "key: \"\"\"
  one stray \" quote # not a comment
\"\"\"
list::
  - \"\"\"
    - not a key # nope
  \"\"\"
next: 1 # real
"
  "A document with a map value and a list item as multi-line strings.")

(defun huml-mode-test--check-multiline-text ()
  "Assert the syntax state around each part of the multi-line document."
  (let ((in-map-body (huml-mode-test--state-after "not a c"))
        (in-list-body (huml-mode-test--state-after "not a k"))
        (after-blocks (huml-mode-test--state-after "next"))
        (real-comment (huml-mode-test--state-after "# r")))
    (should (nth 3 in-map-body))
    (should-not (nth 4 in-map-body))
    (should (nth 3 in-list-body))
    (should-not (nth 4 in-list-body))
    (should-not (nth 3 after-blocks))
    (should (nth 4 real-comment))))

(ert-deftest huml-mode-test-multiline-string-is-inert ()
  "Quotes and hashes inside a multi-line string are plain text."
  (with-temp-buffer
    (insert huml-mode-test--multiline-text)
    (huml-mode)
    (huml-mode-test--check-multiline-text)))

(ert-deftest huml-mode-test-multiline-string-survives-edit ()
  "Editing inside a multi-line string keeps the closing fence."
  (with-temp-buffer
    (insert huml-mode-test--multiline-text)
    (huml-mode)
    (huml-mode-test--state-after "next")
    (goto-char (point-min))
    (search-forward "  one ")
    (insert "x")
    (huml-mode-test--check-multiline-text)))

(ert-deftest huml-mode-test-multiline-string-longer-than-a-chunk ()
  "A body longer than one propertize chunk keeps its closing fence."
  (with-temp-buffer
    (huml-mode)
    (insert "k: \"\"\"\n")
    (dotimes (_ 200)
      (insert "  line of text # here \"q\"\n"))
    (insert "\"\"\"\nafter: 1 # c\n")
    (should (nth 3 (huml-mode-test--state-after "line of text")))
    (should-not (nth 3 (huml-mode-test--state-after "after: 1")))))

(ert-deftest huml-mode-test-comment-is-not-a-string-opener ()
  "A comment line that looks like an opening line does not open a string."
  (with-temp-buffer
    (huml-mode)
    (insert "# note: \"\"\"\nreal: \"\"\"\n  body # x\n\"\"\"\nafter: 1 # c\n")
    (should (eq t (nth 3 (huml-mode-test--state-after "body"))))
    (should-not (nth 3 (huml-mode-test--state-after "after: 1")))))

;;; Font-lock

(ert-deftest huml-mode-test-font-lock ()
  "Faces match the assertions in the font-lock fixture."
  (ert-font-lock-test-file (huml-mode-test--fixture "font-lock.huml")
                           'huml-mode))

;;; Indentation and formatting

(ert-deftest huml-mode-test-indent-region ()
  "Indenting a region gives the results in the erts file."
  (ert-test-erts-file (huml-mode-test--fixture "indent.erts")
                      (lambda ()
                        (huml-mode)
                        (indent-region (point-min) (point-max)))))

(defun huml-mode-test--indentation-after-newline (text)
  "Return the indentation of the new line after inserting TEXT and a newline."
  (with-temp-buffer
    (huml-mode)
    (insert text)
    (newline-and-indent)
    (current-indentation)))

(ert-deftest huml-mode-test-newline-indents-after-block-opener ()
  "A new line is indented only after a line that opens a block."
  (should (= 2 (huml-mode-test--indentation-after-newline "a::")))
  (should (= 2 (huml-mode-test--indentation-after-newline "a:: # note")))
  (should (= 4 (huml-mode-test--indentation-after-newline "a::\n  - ::")))
  (should (= 0 (huml-mode-test--indentation-after-newline "a: 1")))
  (should (= 0 (huml-mode-test--indentation-after-newline "a: 1 # why ::")))
  (should (= 2 (huml-mode-test--indentation-after-newline
                "a::\n  text: \"\"\"\n    body\n  \"\"\"\n  b: 1")))
  (should (= 2 (huml-mode-test--indentation-after-newline
                "a::\n  text: \"\"\" # why ::\n    body\n  \"\"\""))))

(ert-deftest huml-mode-test-tab-cycles-levels ()
  "Tab steps a line up to the deepest level, then back down."
  (with-temp-buffer
    (huml-mode)
    (insert "a::\n  b::\n    c: 1\nd: 2")
    (let ((this-command 'indent-for-tab-command)
          (levels nil))
      (dotimes (_ 4)
        (indent-for-tab-command)
        (push (current-indentation) levels))
      (should (equal '(4 2 0 4) (nreverse levels)))
      (should (eolp)))))

(ert-deftest huml-mode-test-tab-moves-multiline-string ()
  "Tab on the line that opens a multi-line string moves the whole string."
  (with-temp-buffer
    (huml-mode)
    (insert "a::\n  t: \"\"\"\n    x\n\n  \"\"\"\n  b: 1\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((this-command 'indent-for-tab-command))
      (indent-for-tab-command))
    (should (equal "a::\nt: \"\"\"\n  x\n\n\"\"\"\n  b: 1\n" (buffer-string)))))

(ert-deftest huml-mode-test-tab-on-region-keeps-levels ()
  "Tab with an active region formats it and does not cycle each line."
  (with-temp-buffer
    (huml-mode)
    (insert "a::\n  b::\n    c: 1\n  d: 2\n")
    (transient-mark-mode 1)
    (push-mark (point-min) t t)
    (goto-char (point-max))
    (let ((this-command 'indent-for-tab-command))
      (indent-for-tab-command))
    (should (equal "a::\n  b::\n    c: 1\n  d: 2\n" (buffer-string)))))

(ert-deftest huml-mode-test-tab-completes-when-asked ()
  "With `tab-always-indent' set to `complete', tab after text completes."
  (with-temp-buffer
    (huml-mode)
    (insert "a::\n  b: tr")
    (let ((tab-always-indent 'complete)
          (this-command 'indent-for-tab-command)
          (completion-at-point-functions
           (list (lambda () (list (- (point) 2) (point) '("true"))))))
      (indent-for-tab-command))
    (should (equal "a::\n  b: true" (buffer-string)))))

(defun huml-mode-test--formatted (text)
  "Return TEXT after `indent-region' on the whole buffer in `huml-mode'."
  (with-temp-buffer
    (huml-mode)
    (insert text)
    (indent-region (point-min) (point-max))
    (buffer-string)))

(ert-deftest huml-mode-test-format-fixes-spacing ()
  "Formatting fixes the spacing the specification fixes and nothing else."
  (should (equal "\"quoted key\": 1\n"
                 (huml-mode-test--formatted "\"quoted key\"  :    1\n")))
  (should (equal "a:: \"x\", \"y\"\n"
                 (huml-mode-test--formatted "a::   \"x\"  ,\"y\"\n")))
  (should (equal "a::\n  - 1\n  - ::\n    b: 2 # note\n"
                 (huml-mode-test--formatted
                  "a::\n  -   1\n  -  ::\n    b:2 #note\n")))
  (should (equal "a:\t1\n" (huml-mode-test--formatted "a:\t1\n")))
  (should (equal "a: 1 # keep  :  this ,\n"
                 (huml-mode-test--formatted "a: 1 # keep  :  this ,  \n")))
  (should (equal "a: \"keep  :  this ,\"\n"
                 (huml-mode-test--formatted "a: \"keep  :  this ,\"\n")))
  (should (equal "a: \"\"\"\n  keep  :  this ,  \n\"\"\"\n"
                 (huml-mode-test--formatted
                  "a: \"\"\"\n  keep  :  this ,  \n\"\"\"\n")))
  (should (equal "a: \"\"\"\n  \"a #x  \n  b\"\n\"\"\"\n"
                 (huml-mode-test--formatted
                  "a:  \"\"\"\n  \"a #x  \n  b\"\n\"\"\"\n"))))

(ert-deftest huml-mode-test-format-refuses-unterminated-string ()
  "Formatting stops at an unterminated string and changes nothing."
  (with-temp-buffer
    (huml-mode)
    (insert "a: \"oops\nb: \"keep  ,  this\"\n")
    (should-error (indent-region (point-min) (point-max)) :type 'user-error)
    (should (equal "a: \"oops\nb: \"keep  ,  this\"\n" (buffer-string)))))

(ert-deftest huml-mode-test-format-region-only ()
  "Formatting a region leaves the rest of the buffer alone."
  (with-temp-buffer
    (huml-mode)
    (insert "a:  1\nb:  2\n")
    (goto-char (point-min))
    (indent-region (point-min) (line-end-position))
    (should (equal "a: 1\nb:  2\n" (buffer-string)))))

;;; Navigation

(defconst huml-mode-test--navigation-text
  "a::
  x: 1
# about b
b::
  y: 2
"
  "A document with two top-level keys and a comment between them.")

(ert-deftest huml-mode-test-defun-navigation ()
  "Defun commands move over top-level entries."
  (with-temp-buffer
    (insert huml-mode-test--navigation-text)
    (huml-mode)
    (search-backward "y: 2")
    (beginning-of-defun)
    (should (looking-at-p "b::"))
    (forward-char 1)
    (beginning-of-defun)
    (should (looking-at-p "b::"))
    (beginning-of-defun)
    (should (looking-at-p "a::"))
    (end-of-defun)
    (should (looking-at-p "b::"))))

(ert-deftest huml-mode-test-imenu-and-outline ()
  "Imenu lists top-level keys and outline levels follow the indentation."
  (with-temp-buffer
    (insert huml-mode-test--navigation-text)
    (huml-mode)
    (should (equal '("a" "b")
                   (delete "*Rescan*"
                           (mapcar #'car (imenu--make-index-alist 'noerror)))))
    (goto-char (point-min))
    (should (looking-at-p outline-regexp))
    (should (= 1 (funcall outline-level)))
    (search-forward "x: 1")
    (insert "\n  deep::")
    (beginning-of-line)
    (should (looking-at-p outline-regexp))
    (should (= 2 (funcall outline-level)))))

;;; Flymake

(defun huml-mode-test--diagnostic-messages (text)
  "Return the Flymake messages for TEXT in `huml-mode'."
  (with-temp-buffer
    (huml-mode)
    (insert text)
    (let (messages)
      (huml-flymake (lambda (diagnostics &rest _)
                      (setq messages
                            (mapcar (lambda (diagnostic)
                                      (string-trim
                                       (flymake-diagnostic-text diagnostic)))
                                    diagnostics))))
      (sort messages #'string<))))

(ert-deftest huml-mode-test-flymake ()
  "Flymake reports broken rules and nothing in valid text."
  (should-not (huml-mode-test--diagnostic-messages
               (concat "a::\n  b: \"x:y  \\\\u \\t\" # ok\n  c: \"\"\"\n"
                       "      #bad  \t\\u\n  \"\"\"\n  d:: 1, 2\n")))
  (should (equal '("Expected one space after the colon")
                 (huml-mode-test--diagnostic-messages "a:1\n")))
  (should (equal '("Expected one space after the colon")
                 (huml-mode-test--diagnostic-messages "a:  1\n")))
  (should (equal '("Unexpected space before the colon")
                 (huml-mode-test--diagnostic-messages "a : 1\n")))
  (should (equal '("Unexpected space before the colon")
                 (huml-mode-test--diagnostic-messages "a- : 1\n")))
  (should (equal '("Expected a space after the hash")
                 (huml-mode-test--diagnostic-messages "a: 1 #note\n")))
  (should (equal '("Trailing space")
                 (huml-mode-test--diagnostic-messages "a: 1 \n")))
  (should (equal '("Tab character")
                 (huml-mode-test--diagnostic-messages "a: 1\t# note\n")))
  (should (equal '("Unterminated string")
                 (huml-mode-test--diagnostic-messages "a: \"x\nb: 1\n")))
  (should (equal '("Invalid escape sequence")
                 (huml-mode-test--diagnostic-messages "a: \"\\u00e9\"\n")))
  (should (equal '("Indentation must be a multiple of two")
                 (huml-mode-test--diagnostic-messages "a::\n   b: 1\n")))
  (should (equal '("Unexpected indentation")
                 (huml-mode-test--diagnostic-messages "a: 1\n    b: 2\n"))))

;;; Editing helpers

(defun huml-mode-test--after-list-item-command (text &optional dict)
  "Return the buffer after `huml-insert-list-item' at the end of TEXT.
DICT is the prefix argument."
  (with-temp-buffer
    (huml-mode)
    (insert text)
    (huml-insert-list-item dict)
    (buffer-string)))

(ert-deftest huml-mode-test-insert-list-item ()
  "A list item is a first item, a sibling, or a dict item."
  (should (equal "a::\n  - " (huml-mode-test--after-list-item-command "a::")))
  (should (equal "a::\n  - 1\n  - "
                 (huml-mode-test--after-list-item-command "a::\n  - 1")))
  (should (equal "a::\n  - "
                 (huml-mode-test--after-list-item-command "a::\n  ")))
  (should (equal "a::\n  - ::"
                 (huml-mode-test--after-list-item-command "a::" t))))

(provide 'huml-mode-test)
;;; huml-mode-test.el ends here
