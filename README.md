
# Table of Contents

1.  [Initial Setup](#orgbed74ea)
    1.  [Add Core Repositories](#org2140b4a)
    2.  [Setup Use-Package](#orga067dbd)
    3.  [Configure Basic Functionality](#orga2c2893)
    4.  [Install Small Packages](#org423c300)
    5.  [Helm](#org2b5f454)
    6.  [Projectile](#org861f7ee)
    7.  [Improved Defaults](#orgc3db05f)
2.  [Core Org Mode Settings](#orgc62a374)
    1.  [Better Babel](#org65a9c90)
    2.  [Auto Lists](#org6be6697)
    3.  [Default Locations](#orgec7e15a)
    4.  [Logging](#org3da68bc)
    5.  [Misc](#orgd6a950e)
    6.  [Indentation](#org1a14b3e)
    7.  [Better Keybindings](#orgbb3aa32)
3.  [Better TODO Settings](#org2c1bc92)
4.  [UI Improvements](#orgf55e6d4)
    1.  [Colorizing TODOs](#org0d857c6)
    2.  [Misc Features](#org9eadb95)
    3.  [Better Fonts](#orgc27708d)


This package is designed to improve the Org Mode experience with better default settings. Out of the box, Org Mode is already very useful. However, the experience can be greatly improved by making some basic changes to enhance your workflow and improve your experience. Many of these settings are recommended in various Org Mode tutorials, blogs, demos, and videos. This just saves you the time of having to make all these changes yourself.


<a id="orgbed74ea"></a>

# Initial Setup

This is the core setup that adds the core repos, configures use-package, sets useful defaults, initializes an auto complete and project management system, and finally sets some nice UI tweaks that greatly enhance the experience.


<a id="org2140b4a"></a>

## Add Core Repositories

Add additional repositories

    (require 'package)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") )
    (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") )


<a id="orga067dbd"></a>

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


<a id="orga2c2893"></a>

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


<a id="org423c300"></a>

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


<a id="org2b5f454"></a>

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


<a id="org861f7ee"></a>

## Projectile

Once again, there are several solutions for this, but I've always liked Projectile so that's what I'm sticking with.

    (use-package projectile
      :bind (:map projectile-mode-map
                  ("C-c p" . projectile-command-map))
      :config
      (projectile-global-mode)
      (setq projectile-completion-system 'helm)
      (helm-projectile-on)
      )
    
    (use-package helm-projectile)
    
    (use-package ac-helm)
    
    (use-package seeing-is-believing)


<a id="orgc3db05f"></a>

## Improved Defaults

Making the UI a little easier to interact with.

    ;; Start with the window maximized
    (toggle-frame-maximized)
    
    ;; show the menu bar
    (menu-bar-mode t)
    
    ;; show columns in addition to lines
    (setq column-number-mode t
          initial-scratch-message nil
          inhibit-startup-screen nil
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


<a id="orgc62a374"></a>

# Core Org Mode Settings


<a id="org65a9c90"></a>

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


<a id="org6be6697"></a>

## Auto Lists

This is something so simple I can't believe it isn't already turned on by default. Calvin Young's [org-autolist](https://github.com/calvinwyoung/org-autolist) is so useful. When you are making a list and you hit return, it automatically adds another bullet for you. It is wonderful.

    (use-package org-autolist
      :hook (org-mode . org-autolist-mode)
      )


<a id="orgec7e15a"></a>

## Default Locations

Org needs to know where to look for things and the most common place to put your org files is in your home directory. While we are at it, let's associate all files ending in `.org` with `org-mode`.

    (setq org-agenda-files '("~/org"))
    
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


<a id="org3da68bc"></a>

## Logging

It is often helpful to record a timemstamp when a TODO item is marked done. You can record a timestamp as well as a note by changing `'time` to `'note`, but that can be overkill for most things.

    (setq org-log-done 'time)


<a id="orgd6a950e"></a>

## Misc

Always open links by hitting return.

    (setq org-return-follows-link  t)


<a id="org1a14b3e"></a>

## Indentation

    (setq org-startup-indented t)


<a id="orgbb3aa32"></a>

## Better Keybindings

We can make things easier on ourselves with some better keybindings.

    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (define-key global-map "\C-cc" 'org-capture)


<a id="org2c1bc92"></a>

# Better TODO Settings

This expands the default TODO keywords by giving us some more robust options. Now there are two more working states and the ending state can be either DONE or WONT-DO.

    (setq org-todo-keywords
          '((sequence "TODO(t)" "IN-PROGRESS(i)" "BLOCKED(b)" "|" "DONE(d)" "WONT-DO(w)" ))
    )


<a id="orgf55e6d4"></a>

# UI Improvements


<a id="org0d857c6"></a>

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


<a id="org9eadb95"></a>

## Misc Features

    (setq org-hide-emphasis-markers nil)
    (add-hook 'org-mode-hook 'visual-line-mode)


<a id="orgc27708d"></a>

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

