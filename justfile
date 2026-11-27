# huml-mode tasks. Run `just` to list them.

# Run the ERT tests.
test:
    emacs --batch -Q -L . -l test/huml-mode-test.el -f ert-run-tests-batch-and-exit

# Byte-compile with warnings as errors, lint, then test.
check: && test
    #!/usr/bin/env bash
    set -euo pipefail
    trap 'rm -f ./*.elc' EXIT
    emacs --batch -Q -L . --eval '(setq byte-compile-error-on-warn t)' \
        -f batch-byte-compile huml-mode.el
    checkdoc=$(emacs --batch -Q --eval '(checkdoc-file "huml-mode.el")' 2>&1)
    [ -z "$checkdoc" ] || { echo "$checkdoc"; exit 1; }
    emacs --batch -Q -L . -l tools/lint/package-lint.el \
        --eval '(setq package-lint-main-file "huml-mode.el")' \
        -f package-lint-batch-and-exit huml-mode.el
    emacs --batch -Q -L . -l tools/lint/xr.el -l tools/lint/relint.el \
        -f relint-batch huml-mode.el
