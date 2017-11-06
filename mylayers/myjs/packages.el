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

(setq myjs-packages
  '(
    coffee-mode
    company
    (company-tern :toggle (configuration-layer/package-usedp 'company))
    evil-matchit
    flycheck
    ggtags
    helm-gtags
    js-doc
    js3-mode
    json-mode
    json-snatcher
    (tern :toggle (spacemacs//tern-detect))
    web-beautify
    skewer-mode
    livid-mode
    ))

(defun javascript/init-coffee-mode ()
  (use-package coffee-mode
    :defer t
    :init
    (progn
      ;; indent to right position after `evil-open-below' and `evil-open-above'
      (add-hook 'coffee-mode-hook '(lambda ()
                                     (setq indent-line-function 'myjs/coffee-indent
                                           evil-shift-width coffee-tab-width))))))

(defun myjs/post-init-company ()
  (spacemacs|add-company-hook js3-mode))

(defun myjs/init-company-tern ()
  (use-package company-tern
    :if (and (configuration-layer/package-usedp 'company)
             (configuration-layer/package-usedp 'tern))
    :defer t
    :init
    (push 'company-tern company-backends-js3-mode)))

(defun myjs/post-init-flycheck ()
  (dolist (mode '(coffee-mode js3-mode json-mode))
    (spacemacs/add-flycheck-hook mode)))

(defun myjs/post-init-ggtags ()
  (add-hook 'js3-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun myjs/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'js3-mode))

(defun myjs/init-js-doc ()
  (use-package js-doc
    :defer t
    :init
    (spacemacs/js-doc-set-key-bindings 'js3-mode)))

(defun myjs/init-js3-mode ()
  (use-package js3-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode)))
    :config
    (progn
      ;; prefixes
      (spacemacs/declare-prefix-for-mode 'js3-mode "mh" "documentation")
      (spacemacs/declare-prefix-for-mode 'js3-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'js3-mode "mr" "refactor")
      (spacemacs/declare-prefix-for-mode 'js3-mode "mz" "folding")
      ;; key bindings
      (spacemacs/set-leader-keys-for-major-mode 'js3-mode
        "w" 'js3-mode-toggle-warnings-and-errors
        "zc" 'js3-mode-hide-element
        "zo" 'js3-mode-show-element
        "zr" 'js3-mode-show-all
        "ze" 'js3-mode-toggle-element
        "zF" 'js3-mode-toggle-hide-functions
        "zC" 'js3-mode-toggle-hide-comments))))

(defun myjs/post-init-evil-matchit ()
  (add-hook `js3-mode `turn-on-evil-matchit-mode))

(defun myjs/init-json-mode ()
  (use-package json-mode
    :defer t))

(defun myjs/init-json-snatcher ()
  (use-package json-snatcher
    :defer t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'json-mode
      "hp" 'jsons-print-path)))

(defun myjs/init-tern ()
  (use-package tern
    :defer t
    :init
    (add-hook 'js3-mode-hook 'tern-mode)
    :config
    (progn
      (spacemacs|hide-lighter tern-mode)
      (when javascript-disable-tern-port-files
        (add-to-list 'tern-command "--no-port-file" 'append))
      (spacemacs//set-tern-key-bindings 'js3-mode))))

(defun myjs/init-web-beautify ()
  (use-package web-beautify
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'js3-mode
        "=" 'web-beautify-js)
      (spacemacs/set-leader-keys-for-major-mode 'json-mode
        "=" 'web-beautify-js)
      (spacemacs/set-leader-keys-for-major-mode 'web-mode
        "=" 'web-beautify-html)
      (spacemacs/set-leader-keys-for-major-mode 'css-mode
        "=" 'web-beautify-css))))

(defun myjs/init-skewer-mode ()
  (use-package skewer-mode
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'skewer-mode
                               'spacemacs/skewer-start-repl
                               "skewer")
      (add-hook 'js3-mode-hook 'skewer-mode)
      )
    :config
    (progn
      (spacemacs|hide-lighter skewer-mode)
      (spacemacs/declare-prefix-for-mode 'js3-mode "ms" "skewer")
      (spacemacs/declare-prefix-for-mode 'js3-mode "me" "eval")
      (spacemacs/set-leader-keys-for-major-mode 'js3-mode
        "'" 'spacemacs/skewer-start-repl
        "ee" 'skewer-eval-last-expression
        "eE" 'skewer-eval-print-last-expression
        "sb" 'skewer-load-buffer
        "sB" 'spacemacs/skewer-load-buffer-and-focus
        "si" 'spacemacs/skewer-start-repl
        "sf" 'skewer-eval-defun
        "sF" 'spacemacs/skewer-eval-defun-and-focus
        "sr" 'spacemacs/skewer-eval-region
        "sR" 'spacemacs/skewer-eval-region-and-focus
        "ss" 'skewer-repl)
      )))

(defun myjs/init-livid-mode ()
  (use-package livid-mode
    :defer t
    :init (spacemacs|add-toggle javascript-repl-live-evaluation
            :mode livid-mode
            :documentation "Live evaluation of JS buffer change."
            :evil-leader-for-mode (js3-mode . "sa"))))
