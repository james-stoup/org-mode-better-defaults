(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") )
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") )

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package emacs
  :init
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (setq confirm-kill-processes nil)		; Stop confirming the killing of processes
  (setq use-short-answers t)                      ; y-or-n-p makes answering questions faster
  (setq read-process-output-max (* 1024 1024))    ; Increase the amount of data which Emacs reads from the process
  (setq gc-cons-threshold 100000000)
  (global-hl-line-mode 1)			        ; Highlight the current line to make it more visible
  (setq lsp-idle-delay 0.500)
  )

(add-to-list 'image-types 'svg)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t)
  )

(use-package better-defaults)

(use-package comment-tags)
(autoload 'comment-tags-mode "comment-tags-mode")
(setq comment-tags-keymap-prefix (kbd "C-c t"))
(with-eval-after-load "comment-tags"
  (setq comment-tags-keyword-faces
        `(("TODO"  . ,(list :weight 'bold :foreground "#28ABE3"))
          ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
          ("BUG"   . ,(list :weight 'bold :foreground "#DB3340"))
          ("HACK"  . ,(list :weight 'bold :foreground "#E8B71A"))
          ("INFO"  . ,(list :weight 'bold :foreground "#F7EAC8"))
          )
        )
  (setq comment-tags-comment-start-only t
        comment-tags-require-colon t
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil))
(add-hook 'prog-mode-hook 'comment-tags-mode)

(use-package compat)

;; auto format on save
(use-package format-all)
(format-all-mode)

(use-package highlight-parentheses)

(use-package markdown-mode)

(use-package mode-icons
  :config
  (mode-icons-mode)
  )

(use-package org-bullets) ;; TODO: Move this to the org section

(use-package paradox)
(paradox-enable)

(use-package paredit)

(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package which-key
  :config
  (which-key-mode)
  )

(use-package helm
  :init
  (helm-mode 1)
  (progn (setq helm-buffers-fuzzy-matching t))
  :bind
  (("C-c h" . helm-command-prefix))
  (("M-x"   . helm-M-x))
  (("C-x b" . helm-buffers-list))
  (("C-c b" . helm-bookmarks))
  (("C-c g" . helm-grep-do-git-grep))  ;; Search using grep in a git project
  )

;;Describe keyboard bindings
(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds)
  )

;; (use-package projectile
;;   :bind (:map projectile-mode-map
;;               ("C-c p" . projectile-command-map))
;;   :config
;;   (projectile-global-mode)
;;   (setq projectile-completion-system 'helm)
;;   (helm-projectile-on)
;;   )

;; (use-package helm-projectile)

;; (use-package ac-helm)

;; (use-package seeing-is-believing)

(use-package treemacs
  :hook (after-init . treemacs)
  )

;; Start with the window maximized
(toggle-frame-maximized)

;; show the menu bar
(menu-bar-mode t)

;; show columns in addition to lines
(setq column-number-mode t
      initial-scratch-message nil
      visible-bell t
      show-paren-mode 1)

;; show line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

;; Navigate split windows using SHIFT + ARROW KEY
(windmove-default-keybindings)

;; Default to y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Cleanup whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(unless (file-directory-p "~/org~")
  (let ((org-dir "~/org"))
    (make-directory org-dir))
  )

(setq org-agenda-files '("~/org"))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(setq org-startup-indented t)

(use-package org-autolist
  :hook (org-mode . org-autolist-mode)
  )

(setq org-log-done 'time)

(use-package org
  :pin gnu
  :custom
  (org-confirm-babel-evaluate nil)              ;; Don't prompt before running code in org
  (org-src-fontify-natively t)                  ;; Use syntax highlighting in source blocks while editing
  (org-src-tab-acts-natively t)                 ;; Tabs act as 4 spaces in source blocks
  (org-src-preserve-indentation t)              ;; Preserving indentation in source blocks
  )

(setq org-return-follows-link  t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "BLOCKED(b)" "|" "DONE(d)" "WONT-DO(w)" ))
)

(setq org-capture-templates
      '(
        ("t" "TODO Item"
         entry (file "~/org/todos.org")
         "* TODO [#B] %? %^g\n"
         :empty-lines 0)

        ("j" "Journal Entry"
         entry (file+datetree "~/org/journal.org")
         "* %?"
         :empty-lines 1)

        ("m" "Meeting"
         entry (file+datetree "~/org/meetings.org")
         "* %? :meeting:%^g \n** Attendees\n - \n** Notes\n** Action Items\n*** TODO [#A] "
         :tree-type week
         :clock-in t
         :clock-resume t
         :empty-lines 0)

        ("n" "Note"
         entry (file+headline "~/org/notes.org" "Random Notes")
         "** %?"
         :empty-lines 0)
        ))

(setq org-tag-alist
      '(
        (:startgroup . nil)
        ("easy" . ?e)
        ("medium" . ?m)
        ("difficult" . ?d)  
        (:endgroup . nil)

        (:startgroup . nil)
        ("@work" . ?w)
        ("@home" . ?h)
        ("@anywhere" . ?a)
        (:endgroup . nil)
        
        ("CRITICAL" . ?c)
        ))

(setq org-tag-faces
      '(
        ("CRITICAL" . (:foreground "red1"          :weight bold))
        ("easy"     . (:foreground "forest green"  :weight bold))
        ("medium"   . (:foreground "yellow1"       :weight bold))
        ("hard"     . (:foreground "sienna"        :weight bold))
        ("@work"    . (:foreground "royalblue1"    :weight bold))
        ("@home"    . (:foreground "mediumPurple1" :weight bold))
        )
      )

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

;; This is a function used by the daily agenda function
(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(setq org-agenda-skip-deadline-if-done t)

;; Additional Agenda configurations can be defined here, right now there is only this one
(setq org-agenda-custom-commands
      '(
        ;; Daily Agenda - most used
        ("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-span 7)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
                                                   (air-org-skip-subtree-if-priority ?C)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority Unfinished tasks:")))
          )
         ((org-agenda-compact-blocks nil)))
        ))

(setq org-todo-keyword-faces
      '(
        ("TODO"        . (:weight bold :foreground "GoldenRod"))
        ("IN-PROGRESS" . (:weight bold :foreground "Cyan"     ))
        ("BLOCKED"     . (:weight bold :foreground "Red"      ))
        ("DONE"        . (:weight bold :foreground "LimeGreen"))
        ("WONT-DO"     . (:weight bold :foreground "LimeGreen"))
        )
      )

(setq org-tag-faces
      '(
        ("CRITICAL"     . (:foreground "red1"          :weight bold))
        ("grooming"     . (:foreground "forest green"  :weight bold))
        ("meeting"      . (:foreground "yellow1"       :weight bold))
        ("retro"        . (:foreground "royalblue1"    :weight bold))
        ("scrum"        . (:foreground "mediumPurple1" :weight bold))
        ("tech_design"  . (:foreground "sienna"        :weight bold))
        )
      )

(setq org-hide-emphasis-markers nil)
(add-hook 'org-mode-hook 'visual-line-mode)

(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
   )
  )
