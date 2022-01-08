;;; File: emacs-format-file
;;; Stan Warford
;;; 17 May 2006

(defun emacs-format-function ()
  "Format the whole buffer."
  ;(c-set-style "stroustrup")
  ;(setq-default fill-column 80)
  ;(add-hook 'text-mode-hook #'auto-fill-mode)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (write-file "emacs-formatted" nil))
  ;(save-buffer))
