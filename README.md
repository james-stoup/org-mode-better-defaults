
# Table of Contents

1.  [Initial Setup](#org8e47e3a)
    1.  [Add Core Repositories](#org223979b)
    2.  [Setup Use-Package](#org76f63a6)
    3.  [Configure Basic Functionality](#orgdce3769)
    4.  [Install Small Packages](#org63c778d)
    5.  [Helm](#orgc07253d)
    6.  [Projectile](#orgad17f2f)
    7.  [Treemacs](#org671369f)
    8.  [Improved Defaults](#orgae1bec0)
2.  [Core Org Mode Settings](#orgd88de16)
    1.  [Default Locations](#org5609bb9)
    2.  [Better Keybindings](#org2bf89dd)
    3.  [Indentation](#orgfab515f)
    4.  [Auto Lists](#org791d56c)
    5.  [Logging](#org9c8e51d)
    6.  [Better Babel](#org00106f2)
    7.  [Misc](#org2df38c4)
3.  [Better TODO Settings](#orgee6b2e1)
    1.  [Expanding TODO Keywords](#orga0a678e)
    2.  [Adding Better Capture Templates](#orgec6eba4)
    3.  [Adding Tags](#org35b5ea8)
4.  [Better Agendas](#org3d265ef)
    1.  [Daily Agenda](#orgea69744)
5.  [Org UI Improvements](#org4ec844e)
    1.  [Colorizing TODOs](#orgb30045f)
    2.  [Colorizing Tags](#orgb158dfb)
    3.  [Misc Features](#org7360586)
    4.  [Better Fonts](#org4b65ba6)
    5.  [Org Modern](#org09d5a6c)


This package is designed to improve the Org Mode experience with better default settings. Out of the box, Org Mode is already very useful. However, the experience can be greatly improved by making some basic changes to enhance your workflow and improve your experience. Many of these settings are recommended in various Org Mode tutorials, blogs, demos, and videos. This just saves you the time of having to make all these changes yourself.


<a id="org8e47e3a"></a>

# Initial Setup

This is the core setup that adds the core repos, configures use-package, sets useful defaults, initializes an auto complete and project management system, and finally sets some nice UI tweaks that greatly enhance the experience.


<a id="org223979b"></a>

## Add Core Repositories

Add additional repositories

    (require 'package)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") )
    (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") )


<a id="org76f63a6"></a>

## Setup Use-Package

This has to be installed first.

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


<a id="orgdce3769"></a>

## Configure Basic Functionality

This makes the UI a little more pleasant.

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


<a id="org63c778d"></a>

## Install Small Packages

These are a bunch of small, but useful, packages that make using org much nicer.

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


<a id="orgc07253d"></a>

## Helm

There are several options for this kind of functionality, but I'm going with Helm.

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


<a id="orgad17f2f"></a>

## Projectile

Once again, there are several solutions for this, but I've always liked Projectile so that's what I'm sticking with.

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


<a id="org671369f"></a>

## Treemacs

Treemacs needs to be installed because it will allow you to view the top two headings of org files. Effectively creating a top level outline view of all org files. This is incredibly useful. With this hook treemacs will start as soon as Emacs is launched. When that happens, it will prompt for the directory to use as the root of the project. I recommend using the org directory in your home directory (don't worry, it gets automatically created) as that is a pretty standard place to store your org files.

    (use-package treemacs
      :hook (after-init . treemacs)
      )


<a id="orgae1bec0"></a>

## Improved Defaults

Making the UI a little easier to interact with.

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


<a id="orgd88de16"></a>

# Core Org Mode Settings


<a id="org5609bb9"></a>

## Default Locations

Org needs to know where to look for things and the most common place to put your org files is in your home directory. So first let's create the `~/org` directory if it doesn't already exist. Then we can associate all files ending in `.org` with `org-mode`.

    (if (not (file-directory-p (substitute-in-file-name "$HOME/org")))
        (let ((org-dir (substitute-in-file-name "$HOME/org")))
          (make-directory org-dir))
      )
    
    (setq org-agenda-files '("~/org"))
    
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


<a id="org2bf89dd"></a>

## Better Keybindings

We can make things easier on ourselves with some better keybindings.

    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (define-key global-map "\C-cc" 'org-capture)


<a id="orgfab515f"></a>

## Indentation

    (setq org-startup-indented t)


<a id="org791d56c"></a>

## Auto Lists

This is something so simple I can't believe it isn't already turned on by default. Calvin Young's [org-autolist](https://github.com/calvinwyoung/org-autolist) is so useful. When you are making a list and you hit return, it automatically adds another bullet for you. It is wonderful.

    (use-package org-autolist
      :hook (org-mode . org-autolist-mode)
      )


<a id="org9c8e51d"></a>

## Logging

It is often helpful to record a timemstamp when a TODO item is marked done. You can record a timestamp as well as a note by changing `'time` to `'note`, but that can be overkill for most things.

    (setq org-log-done 'time)


<a id="org00106f2"></a>

## Better Babel

Working in source blocks is an amazing feature, but there are some annoyances. No longer having to confirm every time you want to execute a code block is wonderful.

    (use-package org
      :pin gnu
      :custom
      (org-confirm-babel-evaluate nil)              ;; Don't prompt before running code in org
      (org-src-fontify-natively t)                  ;; Use syntax highlighting in source blocks while editing
      (org-src-tab-acts-natively t)                 ;; Tabs act as 4 spaces in source blocks
      (org-src-preserve-indentation t)              ;; Preserving indentation in source blocks
      )


<a id="org2df38c4"></a>

## Misc

Always open links by hitting return.

    (setq org-return-follows-link  t)


<a id="orgee6b2e1"></a>

# Better TODO Settings


<a id="orga0a678e"></a>

## Expanding TODO Keywords

This expands the default TODO keywords by giving us some more robust options. Now there are two more working states and the ending state can be either DONE or WONT-DO.

    (setq org-todo-keywords
          '((sequence "TODO(t)" "IN-PROGRESS(i)" "BLOCKED(b)" "|" "DONE(d)" "WONT-DO(w)" ))
    )


<a id="orgec6eba4"></a>

## Adding Better Capture Templates

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


<a id="org35b5ea8"></a>

## Adding Tags

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


<a id="org3d265ef"></a>

# Better Agendas


<a id="orgea69744"></a>

## Daily Agenda

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


<a id="org4ec844e"></a>

# Org UI Improvements


<a id="orgb30045f"></a>

## Colorizing TODOs

    (setq org-todo-keyword-faces
          '(
            ("TODO"        . (:weight bold :foreground "GoldenRod"))
            ("IN-PROGRESS" . (:weight bold :foreground "Cyan"     ))
            ("BLOCKED"     . (:weight bold :foreground "Red"      ))
            ("DONE"        . (:weight bold :foreground "LimeGreen"))
            ("WONT-DO"     . (:weight bold :foreground "LimeGreen"))
            )
          )


<a id="orgb158dfb"></a>

## Colorizing Tags

    (setq org-tag-faces
          '(
            ("CRITICAL" . (:weight bold :foreground "red1"         ))
            ("easy"     . (:weight bold :foreground "forest green" ))
            ("medium"   . (:weight bold :foreground "yellow1"      ))
            ("hard"     . (:weight bold :foreground "sienna"       ))
            ("@work"    . (:weight bold :foreground "royalblue1"   ))
            ("@home"    . (:weight bold :foreground "mediumPurple1"))
            )
          )


<a id="org7360586"></a>

## Misc Features

    (setq org-hide-emphasis-markers nil)
    (add-hook 'org-mode-hook 'visual-line-mode)


<a id="org4b65ba6"></a>

## Better Fonts

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


<a id="org09d5a6c"></a>

## Org Modern

So I don't like this look, but I'm going to include it.

