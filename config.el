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

(map! :leader
      :desc "Sync Doom and restart"
      "r s" #'my/doom-sync-and-restart)

(add-hook 'emacs-startup-hook
          (lambda ()
            (dolist (buf (buffer-list))
              (when (buffer-file-name buf)
                (with-current-buffer buf
                  (unless (get-buffer-window buf)
                    ;; Open the buffer in a hidden window so centaur-tabs picks it up
                    (split-window-right)
                    (other-window 1)
                    (switch-to-buffer buf)
                    (other-window -1)))))))

(map! :leader
      :desc "Close tab"
      "b k" #'centaur-tabs--kill-this-buffer-dont-ask)

(defun my/vterm-project (project-dir)
  "Open a vterm buffer for a specific PROJECT-DIR."
  (interactive "DProject directory: ")
  (let* ((buf-name (format "vterm:%s" (file-name-nondirectory (directory-file-name project-dir))))
         (vterm-buf (get-buffer buf-name)))
    ;; Create the vterm buffer if it doesn’t exist
    (unless vterm-buf
      (setq vterm-buf (vterm buf-name))
      (with-current-buffer vterm-buf
        (cd project-dir))))
  ;; Switch to the buffer
  (switch-to-buffer buf-name))

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

;; Custom doom dashboard
(setq fancy-splash-image "~/Desktop/Wallpapers/emma1-small.png")

(defun my/open-config-file ()
  "Open my Doom config.el file."
  (interactive)
  (find-file "~/.config/doom/config.el"))

(setq +doom-dashboard-menu-sections
      '(("Open config.el"
         :icon (nerd-icons-octicon "nf-oct-tools" :face 'doom-dashboard-menu-title)
         :action my/open-config-file)
        ("Find file"
         :icon (nerd-icons-faicon "nf-fa-file" :face 'doom-dashboard-menu-title)
         :action find-file)
        ("Switch buffer"
         :icon (nerd-icons-faicon "nf-fa-exchange" :face 'doom-dashboard-menu-title)
         :action switch-to-buffer)
        ("Quit Doom"
         :icon (nerd-icons-faicon "nf-fa-power_off" :face 'doom-dashboard-menu-title)
         :action save-buffers-kill-terminal)))

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

(after! treemacs
  (treemacs-follow-mode 1))


(map! :leader
      :desc "Open treemacs"
      "o t" #'treemacs)
