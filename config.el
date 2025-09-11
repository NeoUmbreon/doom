;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;;; My configs
;; Clicking on treemacs with single click instead of double click
(after! treemacs
(with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)))

(after! treemacs
  ;; Force one workspace per frame (works like "global" by using one frame)
  (treemacs-set-scope-type 'Frames)

  ;; Disable Doom’s automatic perspective integration
  (remove-hook 'doom-init-ui-hook #'treemacs-add-and-display-current-project-exclusively)
  (remove-hook 'persp-switch-hook #'treemacs--on-persp-switch)
  (remove-hook 'persp-before-switch-functions #'treemacs--on-persp-before-switch))

;; Enable following the file you're in for treemacs
(after! treemacs
  (treemacs-follow-mode 1))

;; Keybind to open treemacs
(map! :leader
      :desc "Open treemacs"
      "o t" #'treemacs)


(defun my/doom-sync-and-restart ()
  "Run `doom sync` asynchronously, then restart Doom Emacs when done."
  (interactive)
  (let ((buf (get-buffer-create "*doom-sync*")))
    (make-process
     :name "doom-sync"
     :buffer buf
     :command '("~/.config/emacs/bin/doom" "sync")
     :sentinel
     (lambda (process event)
       (when (string= event "finished\n")
         ;; Optional: kill the buffer
         (when (buffer-live-p buf)
           (kill-buffer buf))
         ;; Restart Doom
         (doom/restart))))))

;; Restart and sync keybind for doom 
(map! :leader
      :desc "Sync Doom and restart"
      "r s" #'my/doom-sync-and-restart)


;; Kill current tab
(map! :leader
      :desc "Close tab"
      "b k" #'centaur-tabs--kill-this-buffer-dont-ask)

;; Normal mode buffer navigation
(map! :n "M-<left>"  #'centaur-tabs-backward-tab
      :n "M-<right>" #'centaur-tabs-forward-tab)

;; Insert mode buffer navigation
(map! :i "M-<left>"  #'centaur-tabs-backward-tab
      :i "M-<right>" #'centaur-tabs-forward-tab)

;; Visual mode buffer navigation
(map! :v "M-<left>"  #'centaur-tabs-backward-tab
      :v "M-<right>" #'centaur-tabs-forward-tab)

;; Start Doom Emacs maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Open buffer-menu
(map! :leader
      :desc "Open buffer menu"
      "b m" #'buffer-menu)

;; My custom doom dashboard
(setq fancy-splash-image "~/Desktop/Wallpapers/emma1-small.png")

(defun my/open-config-file ()
  "Open my Doom config.el file."
  (interactive)
  (find-file "~/.config/doom/config.el"))

(setq +doom-dashboard-menu-sections
      '(("Open config.el"
         :icon (nerd-icons-octicon "nf-oct-tools" :face 'doom-dashboard-menu-title)
         :action my/open-config-file)
        ("Open Org Agenda"
         :icon (nerd-icons-faicon "nf-fa-book" :face 'doom-dashboard-menu-title)
         :action org-agenda-list)
        ("Find file"
         :icon (nerd-icons-faicon "nf-fa-file" :face 'doom-dashboard-menu-title)
         :action find-file)
        ("Switch buffer"
         :icon (nerd-icons-faicon "nf-fa-exchange" :face 'doom-dashboard-menu-title)
         :action switch-to-buffer)
        ("Quit Doom"
         :icon (nerd-icons-faicon "nf-fa-power_off" :face 'doom-dashboard-menu-title)
         :action save-buffers-kill-terminal)))

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Emma the Emacs Unicorn!")))


;; Send ctrl-c in vterm
(map! :after vterm
      :map vterm-mode-map
      :ni "C-c" #'vterm--self-insert)

;; Use ~/.config/doom/bookmarks as the bookmark file
(setq bookmark-default-file (expand-file-name "bookmarks" doom-user-dir)
      bookmark-save-flag 1)

;; Make vterm open in a toggleable buffer
(map! "C-`"
      (cmd! (if (get-buffer "*vterm*")
                (+popup/toggle)
              (vterm))))

;; Variable to store the last reset date
(defvar my/checkbox-reset-last-date nil
  "The last date checkboxes were reset.")

(defun my/org-reset-checkboxes-in-file (file)
  "Reset all checkboxes in FILE by unchecking them."
  (when (file-exists-p file)
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-list-full-item-re nil t)
         (when (org-at-item-checkbox-p)
           (replace-match "[ ]" nil nil nil 1))))
      (save-buffer))))

(defun my/org-reset-checkboxes-daily ()
  "Reset checkboxes in daily.org once per day."
  (let ((today (format-time-string "%Y-%m-%d")))
    (unless (string= my/checkbox-reset-last-date today)
      (setq my/checkbox-reset-last-date today)
      (my/org-reset-checkboxes-in-file "~/Nextcloud/Notes/org/daily.org"))))

(defun my/open-daily-and-reset ()
  "Split frame: dashboard on left (2/3), daily.org on right (1/3)."
  (let ((buf (find-file-noselect "~/Nextcloud/Notes/org/daily.org")))
    (my/org-reset-checkboxes-daily)
    (when (string= (buffer-name) "*doom*")
      ;; Split: left gets 2/3 of width, right gets 1/3
      (let* ((main (selected-window))
             (total (window-total-width main))
             (left-size (floor (* 2 (/ total 3.0)))))
        (select-window (split-window main left-size 'right))
        (switch-to-buffer buf)
        (set-window-dedicated-p (selected-window) t)
        ;; Go back to dashboard (left side)
        (select-window main)))))

(defvar my/treemacs-opened-once nil
  "Non-nil after we've auto-opened treemacs once on startup/dashboard.")

(defun my/open-daily-reset-and-treemacs ()
  "Open daily.org split, then open Treemacs after a short idle delay."
  (my/open-daily-and-reset)
  (unless my/treemacs-opened-once
    (setq my/treemacs-opened-once t)
    ;; Wait until Emacs is idle for 1.0s, then open treemacs
    (run-with-idle-timer 1.0 nil
                         (lambda ()
                           (when (fboundp 'treemacs)
                             (ignore-errors (treemacs)))))))

;; Use this hook (replace your current +doom-dashboard hook if you had one)
(add-hook '+doom-dashboard-mode-hook #'my/open-daily-reset-and-treemacs)

;; Also run before showing agenda (in case Emacs was left open overnight)
(add-hook 'org-agenda-mode-hook #'my/org-reset-checkboxes-daily)

;; Force reset daily checkboxes manually
(defun my/force-reset-daily-checkboxes ()
  "Force a reset of checkboxes in daily.org, ignoring the date."
  (interactive)
  (setq my/checkbox-reset-last-date nil)
  (my/org-reset-checkboxes-daily))

;; Workaround for "Org Clocking Buffer Definition is void"
(defun org-clocking-buffer (&rest _))

;; Open doom dashboard
(map! :leader
      :desc "Open Doom dashboard"
      "d d" #'+doom-dashboard/open)


;; Set my org files
(setq! org-agenda-files '("~/Nextcloud/Notes/org/daily.org"))
