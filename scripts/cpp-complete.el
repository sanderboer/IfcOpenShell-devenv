(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))

(when (not package-archive-contents) ;; fetch the list of packages available
  (package-refresh-contents))
;; install the missing packages
(setq package-list '(use-package))
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))
(setq use-package-verbose t)
(require 'use-package)
(use-package auto-compile :ensure t)
(setq load-prefer-newer t)


;; QUELPA
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://framagit.org/steckerhalter/quelpa/raw/master/bootstrap.el")
    (eval-buffer)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
(require 'quelpa-use-package)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; move line

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column (transpose-lines 1)
               (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column (forward-line 1)
               (transpose-lines 1)
               (forward-line -1)))

(global-set-key (kbd "M-<up>")
                'move-line-up)
(global-set-key (kbd "M-<down>")
                'move-line-down)

(define-key global-map (kbd "C-M-;")
  '(lambda()
     (interactive)
     (newline-and-indent)
     (insert "////////////////////////////////////////////////////////////")
     (newline-and-indent)
     (insert "//  %%")
     (newline-and-indent)
     (insert "////////////////////////////////////////////////////////////")
     (newline-and-indent)
     (search-backward "%%")
     (delete-forward-char 2)
     ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode
           mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(use-package evil
  :ensure t
  :init (setq evil-want-integration t)(setq evil-want-keybinding nil):config
  (evil-mode 1)
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (use-package powerline-evil
    :ensure t
    :config (progn
              (require 'powerline-evil))))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'color)

(defun hexcolour-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
  This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (cadr values))
         (b (caddr values)))
    (floor (+ (* .3 r)
              (* .59 g)
              (* .11 b))
           256)))

(defun set-colors-based-on-theme ()
  (interactive)
  "Sets the hl-line face to have no foregorund and a background
        that is 10% darker than the default face's background."
  (setq bg (if (eq (face-background 'default t) nil)
               (color-darken-name "#000000" 0)
             (face-background 'default t)))
  (if (< (hexcolour-luminance bg) 200)
      (setq sgn -1)
    (setq sgn 1))
  (set-face-attribute 'hl-line
                      nil
                      :foreground nil
                      :background (color-desaturate-name (color-darken-name bg
                                                                            (* sgn 10))
                                                         100))
  (set-face-background 'show-paren-match
                       (color-desaturate-name (color-darken-name bg
                                                                 (* sgn 15))
                                              100))
  (set-face-background 'region
                       (color-desaturate-name (color-darken-name bg
                                                                 (* sgn 20))
                                              100)))

(setq custom-file "~/.emacs.d/customize-do-not-edit.el")
(load custom-file t)
(setq save-place-file "~/.emacs.d/save-places")
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq url-configuration-directory "~/.emacs.d/url.d")
(setq gamegrid-user-score-file-directory "~/.emacs.d/games.d")
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save-list.d/")
(setq gnus-home-directory "~/.emacs.d/gnus.d/")
(setq gnus-directory "~/.emacs.d/gnus.d/News/")
(setq message-directory "~/.emacs.d/gnus.d/Mail/")
(setq nnfolder-directory "~/.emacs.d/gnus.d/Mail/archive/")
(setq gnus-init-file "~/.emacs.d/gnus.d/init.el")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq enable-local-variables :all)
(cond
 ((file-exists-p "~/.emacs.d/site-lisp")
  (progn
    (cd "~/.emacs.d/site-lisp")
    (normal-top-level-add-subdirs-to-load-path))
  (setq load-path (append (list nil "~/.emacs.d/site-lisp")
                          load-path))))

;; IDO
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
(require 'cc-mode)
(ansi-color-for-comint-mode-on)

(defun sndr-term-keys ()
  (interactive)
  (progn
    (define-key input-decode-map "\e[1;2D" [S-left])
    (define-key input-decode-map "\e[1;2C" [S-right])
    (define-key input-decode-map "\e[1;2B" [S-down])
    (define-key input-decode-map "\e[1;2A" [S-up])
    (define-key input-decode-map "\e[1;2F" [S-end])
    (define-key input-decode-map "\e[1;2H" [S-home])
    (define-key input-decode-map "\e[1;5D" [C-left])
    (define-key input-decode-map "\e[1;5C" [C-right])
    (define-key input-decode-map "\e[1;5B" [C-down])
    (define-key input-decode-map "\e[1;5A" [C-up])
    (define-key input-decode-map "\e[1;3A" [M-up])
    (define-key input-decode-map "\e[1;3B" [M-down])
    (define-key input-decode-map "\e[1;3C" [M-right])
    (define-key input-decode-map "\e[1;3D" [M-left])))
(sndr-term-keys)

(menu-bar-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode t)
(setq visible-bell t)
(setq scroll-bar-mode nil)
(global-visual-line-mode 1)
(show-paren-mode t)
(setq show-paren-style 'expression) ; highlight entire expression
(setq visual-line-fringe-indicators '(nil vertical-bar))
(line-number-mode 1)
(column-number-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq cursor-type 'bar)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(set-default 'fill-column 80)
(setq fill-column 80)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent
(savehist-mode 1)
(setq history-length 1000)
(global-subword-mode 1)
(setq gc-cons-threshold 20000000)
(set-default 'sentence-end-double-space nil)
(setq select-enable-clipboard t) ;; Allow pasting selection outside of Emacs
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers
      t)
(setq auto-revert-verbose nil)
(setq echo-keystrokes 0.1)
(setq delete-by-moving-to-trash t)
(setq shift-select-mode nil)
(auto-compression-mode t) ;; Transparently open compressed files
(global-font-lock-mode t) ;; Enable syntax highlighting for older Emacsen that have it off

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq linum-format "%4d ")
(setq display-line-numbers t)

;;(set-face-attribute 'show-paren-match nil :background "grey20")
(set-face-attribute 'show-paren-mismatch nil
                    :background "red")
;; UTF8
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
(set-language-environment "UTF-8")
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(if (eq system-type 'gnu/linux)
    (set-clipboard-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-16le-dos))
;;autosave
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
;;yasnippet
(use-package yasnippet
  :defer t
  :ensure t
  :diminish yas-minor-mode
  :config (progn
            (yas-global-mode 1)))
;; FOLDING
(defun sndr-folding-setup ()
  (interactive)
  (hs-minor-mode)
  (define-key c-mode-base-map (kbd "<C-return>") #'hs-toggle-hiding)
  (define-key c-mode-base-map (kbd "<M-return>") #'hs-hide-level)
  (define-key c-mode-base-map (kbd "<C-M-return>") #'hs-show-all))

(add-hook 'prog-mode-hook
          (lambda ()
            (sndr-folding-setup)))

;;DIRED
(require 'dired-x)
(put 'dired-find-alternate-file 'disabled
     nil)
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "<kp-enter>") 'dired-advertised-find-file)
            (define-key dired-mode-map (kbd "<C-kp-enter>") (lambda ()
                                                              (interactive)
                                                              (dired-up-directory "..")))
            (hl-line-mode 1)))
(use-package ranger
  :ensure t
  :config (progn
            (require 'image-mode)
            (setq ranger-preview-file t
                  ranger-excluded-extensions
                  '("mkv" "iso" "mp4")
                  ;;         ranger-dont-show-binary t
                  ranger-override-dired-mode
                  t
                  ranger-show-literal
                  nil)))

;; ORG-mode
(defun sndr-setup-org ()
  "Set up Org-mode for programming."
  (interactive)
  (global-set-key "\C-cl" 'org-store-link)
  (setq org-directory "~/.org") ;; redeclare in .dir-locals
  (setq org-default-notes-file (concat org-directory "/projectnotes.org"))
  (setq org-startup-folded "content")
  (setq org-startup-indented "indent")
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-capture-templates '(("t" "Taak"
                                 entry
                                 (file+olp+headline org-default-notes-file
                                                    "Taken")
                                 "* DOEN %?\n
:PROPERTIES:
:Context: %a
:Captured: %U
:END:
\n%i")
                                ("f" "FIXNOW"
                                 entry
                                 (file+olp+headline org-default-notes-file
                                                    "Taken")
                                 "* FIXNOW BUG: %?\n
:PROPERTIES:
:Context: %a
:Captured: %U
:END:
\n%i")
                                ("i" "Idee"
                                 entry
                                 (file+olp+headline org-default-notes-file
                                                    "Idee")
                                 "* OOIT %?\n
:PROPERTIES:
:Context: %a
:Captured: %U
:END:
%i")
                                ("d" "Dagboek"
                                 entry
                                 (file+olp+datetree org-default-notes-file
                                                    "Dagboek")
                                 "* %?\n
:PROPERTIES:
:Context: %a
:Captured: %U
:END:
\n
%i")
                                ("l" "Link"
                                 entry
                                 (file+olp org-default-notes-file "Weblinks")
                                 "* %a\n %?\n %i")))
  (setq org-todo-keywords '((sequence "DOEN" "VOLGENDE" "FIXNOW" "NOTE"
                                      "|" "VOLTOOID" "OOIT" "WACHTEN")))
  (setq org-todo-keyword-faces '(("DOEN" . "red")
                                 ("VOLGENDE" . "OrangeRed")
                                 ("FIXNOW" . "DarkOrange")
                                 ("NOTE" . "green")
                                 ("VOLTOOID" . "green")
                                 ("OOIT" . "NavyBlue")
                                 ("WACHTEN" . "grey25")))
  (require 'org)
  ;; (server-start)
  (require 'org-protocol)
  (require 'org-protocol-capture-html));; end defun sndr-setup-org
(use-package org
  :ensure t
  :config (sndr-setup-org))
;; (use-package org-plus-contrib
;; :ensure t)

;; MOVING AROUND
(global-set-key (kbd "M-3")
                'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2")
                'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1")
                'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0")
                'delete-window) ; was digit-argument
(global-set-key (kbd "M-o")
                'other-window) ; was facemenu-keymap
(global-set-key (kbd "M-b")
                'ido-switch-buffer) ; was facemenu-keymap
(global-set-key (kbd "C-'")
                'match-paren)
(global-set-key (kbd "M-k")
                'kill-this-buffer)
(global-set-key (kbd "C-S")
                'isearch-forward-symbol-at-point)
(global-set-key "\C-h\C-e" 'hippie-expand)
(global-set-key (kbd "C-x g")
                'magit-status)
(global-set-key (kbd "M-f")
                'find-file)
(global-set-key (kbd "M-q")
                'delete-frame)
(global-set-key (kbd "M-p")
                'fill-paragraph)
(global-set-key (kbd "M-z")
                'keyboard-quit)
(global-set-key (kbd "M-g")
                'imenu)
(global-set-key (kbd "<f5>")
                'revert-buffer)
(global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
(global-unset-key (kbd "C-x 2")) ; was split-window-vertically
(global-unset-key (kbd "C-x 1")) ; was delete-other-windows
(global-unset-key (kbd "M-t")) ; was transppose -wrods
(windmove-default-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CPP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq uname (replace-regexp-in-string "[ \n]+$"
                                      ""
                                      (shell-command-to-string "uname")))
(use-package clang-format
  :ensure t
  :config (progn
            (global-set-key (kbd "C-c i")
                            'clang-format-region)
            (global-set-key (kbd "C-c u")
                            'clang-format-buffer)
            (setq clang-format-style-option "google")))


(defun insert-timeofday ()
  (interactive "*")
  (insert (format-time-string "---------------- %a, %d %b %y: %I:%M%p")))
(setq auto-mode-alist (append '(("\\.cpp$" . c++-mode)
                                ("\\.hin$" . c++-mode)
                                ("\\.cin$" . c++-mode)
                                ("\\.inl$" . c++-mode)
                                ("\\.rdc$" . c++-mode)
                                ("\\.h$" . c++-mode)
                                ("\\.c$" . c++-mode)
                                ("\\.cc$" . c++-mode)
                                ("\\.c8$" . c++-mode)
                                ("\\.txt$" . indented-text-mode)
                                ("\\.emacs$" . emacs-lisp-mode)
                                ("\\.gen$" . gen-mode)
                                ("\\.ms$" . fundamental-mode)
                                ("\\.fs$" . glsl-mode)
                                ("\\.vs$" . glsl-mode)
                                ("\\.m$" . objc-mode)
                                ("\\.mm$" . objc-mode))
                              auto-mode-alist))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (compile sander-makescript)
  (other-window 1))

(defun test-without-asking ()
  "Make the current build."
  (interactive)
  (compile sander-testscript)
  (other-window 1))

(defun start-debug ()
  "start debugger"
  (interactive)
  (compile sander-debugscript)
  (other-window 1))

(defun start-run ()
  "run program"
  (interactive)
  (compile sander-runscript)
  (other-window 1))

;; dir locals test
(defun sndr-dir-locals-dir ()
  "Return the directory local variables directory.
Code taken from `hack-dir-local-variables'."
  (interactive)
  (let ((variables-file (dir-locals-find-file (or (buffer-file-name)
                                                  default-directory)))
        (dir-name nil))
    ;; ( message variables-file)
    (cond
     ((stringp variables-file)
      (setq dir-name (file-name-directory variables-file)))
     ((consp variables-file)
      (setq dir-name (nth 0 variables-file))))
    dir-name))

(use-package autopair
  :defer t
  :config (autopair-global-mode 1))

(use-package glsl-mode :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; ANSI COLORS
;; Stolen from (http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html)
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start
                                (point))))

(add-hook 'compilation-filter-hook #'endless/colorize-compilation)

;; Stolen from (https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/)
(defun regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps
             "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

(setq non-sgr-control-sequence-regexp (regexp-alternatives '( ;; icon name escape sequences
                                                             "\033\\][0-2];.*?\007"
                                                             ;; non-SGR CSI escape sequences
                                                             "\033\\[\\??[0-9;]*[^0-9;m]"
                                                             ;; noop
                                                             "\012\033\\[2K\033\\[1F")))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward non-sgr-control-sequence-regexp
                              end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker (or comint-last-output-start
                          (point-min-marker)))
        (end-marker (process-mark (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker end-marker)))

(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)

(defun sander-find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (setq CorrespondingFileName nil)
  (setq BaseFileName (file-name-sans-extension buffer-file-name))
  (if (string-match "\\.c" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if (string-match "\\.h" buffer-file-name)
      (if (file-exists-p (concat BaseFileName ".cpp"))
          (setq CorrespondingFileName (concat BaseFileName ".cpp"))
        (if (file-exists-p (concat BaseFileName ".c"))
            (setq CorrespondingFileName (concat BaseFileName ".c"))
          (setq CorrespondingFileName (concat BaseFileName ".cc")))))
  (if (string-match "\\.hin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".cin")))
  (if (string-match "\\.cin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".hin")))
  (if (string-match "\\.cc" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if (string-match "\\.cpp" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if CorrespondingFileName
      (find-file CorrespondingFileName)
    (error "Unable to find a corresponding file")))

(defun sander-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (sander-find-corresponding-file)
  (other-window -1))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min)
                 (point-max)
                 nil)
  (untabify (point-min)
            (point-max)))

;; cscope vs irony/rtags

(defun sndr-cpp-all ()
  (interactive)
  (require 'cc-mode)
  (require 'google-c-style)
  (setq compilation-directory-locked nil)
  (google-set-c-style)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)
  (abbrev-mode 1)
  (linum-mode)
  (setq sander-makescript "build.sh")
  (setq sander-debugscript "debug.sh")
  (setq sander-runscript "run.sh")
  (setq sander-testscript "test.sh")
  (define-key global-map "\em" 'make-without-asking)
  (define-key global-map "\en" 'test-without-asking)
  (define-key global-map "\eM" 'start-debug)
  (define-key global-map "\er" 'start-run)
  (define-key global-map "\ef" 'find-file)
  (define-key global-map "\eF" 'find-file-other-window)
  (define-key c++-mode-map "\ec" 'sander-find-corresponding-file)
  (define-key c++-mode-map "\eC" 'sander-find-corresponding-file-other-window)

  (define-key evil-normal-state-map (kbd "<f8>") 'next-error)
  (define-key evil-normal-state-map (kbd "<f7>") 'previous-error)
  

 (setq gdb-many-windows t gdb-show-main t)

 (use-package highlight-indent-guides
    :ensure t
    :config (progn
              (setq
               ;; highlight-indent-guides-method 'fill
               ;; highlight-indent-guides-method 'column
               highlight-indent-guides-method 'character
               highlight-indent-guides-character ?\| highlight-indent-guides-responsive
               'top)
              (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)))
  (setq cc-other-file-alist '(("\\.cc\\'"
                               (".h"))
                              ("\\.h\\'"
                               (".cc"))
                              ("\\.cpp\\'"
                               (".h"))
                              ("\\.h\\'"
                               (".cpp"))))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (local-set-key (kbd "M-c")
                             'ff-find-other-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRONY ET AL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; == irony-mode ==
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

;; == company-mode ==
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              nil
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	company-backends                '((company-irony company-gtags))
	)
  :bind ("C-;" . company-complete-common)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook #'sndr-cpp-all)
(if (eq system-type 'gnu/linux)
    ;; LINUX
    (progn
      ;; (setq the-theme 'monokaix)
      (setq the-theme 'zenbu)
      )
  ;; NOT LINUX
  (progn
    (setq the-theme 'zweilight)
    (sndr-cpp-win)
    (if (string-equal uname "MINGW64_NT-10.0")
        (progn
          ;;(sndr-cpp-irony)
	  ))));; end if

(use-package powerline
  :ensure t
  :config (progn
            (setq powerline-default-separator (quote arrow))))

(defun base-look ()
  (interactive)
  (progn
    (if (string= (system-name)
                 "sun-ra")
        (progn
          (set-face-attribute 'default nil :height 100))
      (progn
        (set-face-attribute 'default nil :height 100)))
    (load-theme the-theme t)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (global-hl-line-mode t)
    (sndr-term-keys)
    (set-frame-parameter (selected-frame)
                         'alpha
                         '(93 85))
    (add-to-list 'default-frame-alist
                 '(alpha 93 85)) ; (fix-elscreen)
    (set-colors-based-on-theme)
    (set-face-background 'org-block
                         "unspecified-bg"
                         (selected-frame))
    ;; (powerline-center-theme)
    (powerline-evil-center-color-theme)))

(defun base-look-term ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (global-hl-line-mode t)
  (sndr-term-keys)
  (load-theme the-theme t)
  (set-face-background 'default
                       "unspecified-bg"
                       (selected-frame))
  (set-face-background 'linum
                       "unspecified-bg"
                       (selected-frame))
  (set-face-background 'org-block
                       "unspecified-bg"
                       (selected-frame))
  (set-colors-based-on-theme)
  ;; (powerline-center-theme)
  (powerline-evil-center-color-theme))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (sndr-term-keys)
                (select-frame frame)
                (if (display-graphic-p)
                    (base-look)
                  (base-look-term))))
  (if (display-graphic-p)
      (base-look)
    (base-look-term)))
