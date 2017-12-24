;;;;
;; Clojure
;;;;

(require 'ac-cider)

(require 'ac-nrepl)
(defun clojure-auto-complete ()
  (interactive)
  (let ((ac-sources
         `(ac-source-nrepl-ns
           ac-source-nrepl-vars
           ac-source-nrepl-ns-classes
           ac-source-nrepl-all-classes
           ac-source-nrepl-java-methods
           ac-source-nrepl-static-methods
           ,@ac-sources)))
    (auto-complete)))

(defun my-clojure-hook ()
  (auto-complete-mode 1)
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 1000)
  (setq cider-repl-history-file "~/.cider-repl-history")
  (setq company-idle-delay nil) ; never start completions automatically
  (global-set-key (kbd "M-TAB") #'company-complete) ; use M-TAB, a.k.a. C-M-i, as manual trigger
  (setq ac-cider-show-ns nil)
  (define-key clojure-mode-map
      (kbd "C-c \\ ") 'clojure-auto-complete))

(add-hook 'clojure-mode-hook 'my-clojure-hook)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)


(defun bind-my-clojure-keys ()
  (global-set-key (kbd "C-c C-o") 'cider-repl-clear-buffer))

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

(add-hook 'clojure-mode-hook 'projectile-mode)

(add-hook 'clojure-mode-hook 'aggressive-indent-mode)

(add-hook 'clojure-mode-hook 'windmove-default-keybindings)

(add-hook 'clojure-mode-hook 'auto-revert-mode)

(add-hook 'clojure-mode-hook 'bind-my-clojure-keys)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

(add-hook 'clojure-mode-hook 'auto-highlight-symbol-mode)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

;;;;
;; Cider
;;;;

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(add-hook 'cider-repl-mode-hook 'cider-repl-toggle-pretty-printing)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))


;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


(defun cider-refresh ()
  (interactive)
  ;; (cider-interactive-eval (format "(user/reset)"))
  (cider-interactive-eval (format "(do (use 'clojure.tools.namespace.repl) (refresh))"))
)

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

(add-to-list 'exec-path "/Applications/clojure")
