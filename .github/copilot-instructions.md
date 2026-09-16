# Repository Instructions

## Architecture

- This repository is a personal Emacs configuration. `early-init.el` runs before package initialization and handles startup-time settings; `init.el` adds `lisp/` to `load-path` and loads the active modules in a deliberate order.
- `lisp/init-package.el` bootstraps Elpaca on first startup, enables `elpaca-use-package-mode`, and makes `use-package` declarations install packages by default. Do not add a second package manager or hand-maintain a package lockfile.
- Active behavior belongs in focused `lisp/init-*.el` modules: core Emacs settings, file behavior, UI, completion, Org, editing, programming support, Japanese input, and experimental/miscellaneous settings. Each loaded module must end with `(provide 'init-<name>)`, matching the feature required by `init.el`.
- `etc/yasnippet/snippets/` contains local Org-mode snippets. `var/`, `elpaca/`, `elpa/`, and `eln-cache/` are runtime or package-manager state; do not edit or commit generated contents.
- `README.org` describes an older Org-tangling workflow and references untracked source files. Treat the checked-in `.el` files as the live configuration unless an Org source is added to the repository.

## Commands

- There is no project build command, lint command, or tracked automated test suite.
- Check the Lisp structure of one changed module without loading packages:

  ```sh
  emacs --batch -Q --eval '(with-temp-buffer (insert-file-contents "lisp/init-org.el") (emacs-lisp-mode) (check-parens))'
  ```

  Replace `lisp/init-org.el` with the changed file. To check all active Lisp modules, run the same expression once per `*.el` file.
- ERT is configured as an interactive helper in `lisp/init-programming.el`: open the buffer containing a test, press `C-c t` to evaluate that buffer, then select a test in the ERT interface. No `ert-deftest` forms are currently tracked, so there is no existing single-test command to run.

## Conventions

- Use `use-package` for both built-in and external features. Mark built-ins with `:ensure nil`; external packages normally rely on the global `use-package-always-ensure` setting. Retain timing/dependency declarations such as `:demand`, `:defer`, `:after`, and `:commands` when changing package configuration.
- Prefer package-local configuration through `:custom`, `:bind`, `:hook`, `:init`, and `:config`, following the existing declaration style. Hooks are written as mode/function pairs, and global keybindings are generally placed in the owning feature’s declaration.
- Keep `init.el` as the orchestration-only entrypoint. Add a new loaded module there only when its initialization order matters; otherwise extend the existing feature module that owns the setting.
- Files use Emacs Lisp section comments (`;;;` for major sections and `;;;;` for subsections), predominantly Japanese explanatory comments, and lexical binding where declared in the file header. Preserve the surrounding file’s language and style.
- The setup has OS-specific branches for Windows and GNU/Linux, particularly for frames, encodings, and Mozc. Keep platform-dependent values inside those branches rather than applying them globally.
- Some configuration points at personal paths or credentials (for example, Org files and service stores). Do not replace them with repository-local placeholders, expose their contents, or commit new secrets.
