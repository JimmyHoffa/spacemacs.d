;;; packages.el --- Javascript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst myjs-packages '(web-mode tide prettier-js flycheck))

(defun myjs/web-mode-hook ()
  (tide-setup)
  (flycheck-select-checker 'javascript-eslint)
  (when (configuration-layer/package-usedp 'company)
    (setq company-backends '(company-tide))
    (company-mode-on))
  )

(defun myjs/init-web-mode ()
  (use-package tide
    :defer t
    :init (progn
            (add-to-list 'auto-mode-alist
                         '("\\.js.?.?\\'" . web-mode))
            (add-hook 'web-mode-hook 'myjs/web-mode-hook)
            (defun myjs/jump-to-type-def ()
              (interactive)
              (tide-jump-to-definition t))
            (defun myjs/browse-references ()
              (interactive)
              (ring-insert find-tag-marker-ring
                           (point-marker))
              (tide-references)
              (switch-to-buffer-other-window "*tide-references*"))
            (spacemacs/set-leader-keys-for-major-mode
              'web-mode "r" 'myjs/browse-references "g"
              'tide-jump-to-definition "-" 'tide-jump-back
              "t" 'myjs/jump-to-type-def "h" 'tide-documentation-at-point
              "R" 'tide-rename-symbol "s" 'tide-restart-server "e"  'spacemacs/goto-flycheck-error-list))))

(defun myjs/init-tide ()
  (use-package tide
    :defer t
    :config (progn
              (defun myjs/view-prev-ref ()
                (interactive)
                (tide-next-reference-function -1)
                (switch-to-buffer-other-window "*tide-references*"))
              (defun myjs/view-next-ref ()
                (interactive)
                (tide-next-reference-function 1)
                (switch-to-buffer-other-window "*tide-references*"))
              (defun myjs/goto-ref-quit-window ()
                (interactive)
                (tide-goto-reference)
                (switch-to-buffer-other-window "*tide-references*")
                (quit-window))
              (define-key tide-references-mode-map (kbd "<up>") 'myjs/view-prev-ref)
              (define-key tide-references-mode-map (kbd "<down>") 'myjs/view-next-ref)
              (define-key tide-references-mode-map (kbd "RET") 'myjs/goto-ref-quit-window))))

(defun myjs/post-init-flycheck ()
  
  ;; https://emacs.stackexchange.com/questions/14898/flycheck-with-eslint-doesnt-use-eslintrc
  (use-package flycheck
    :ensure t
    :custom
    (flycheck-display-errors-delay 0)
    :config
    (global-flycheck-mode)
    ;; disable json-jsonlist checking for json files
    (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))
    ;; disable jshint since we prefer eslint checking
    (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
    ;; use eslint with web-mode for jsx files
    ;; Workaround for eslint loading slow
    ;; https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
    (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))
  
  (defun flycheck-eslint-config-exists-p () (eq 0 0))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (spacemacs/add-flycheck-hook 'web-mode)
  )

(defun myjs/init-prettier-js ()
  (dolist (mode '(web-mode json-mode tide-mode))
    (spacemacs/set-leader-keys-for-major-mode
      mode "=" 'prettier-js)))
