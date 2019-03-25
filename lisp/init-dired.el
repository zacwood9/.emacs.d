(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-mode-line-type 'none))

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode)))

(defun zac/dired-multi-occur (string)
  "Search string in files marked by dired."
  (interactive "MList lines matching regexp: ")
  (require 'dired)
  (multi-occur (mapcar 'find-file (dired-get-marked-files)) string))

(provide 'init-dired)
