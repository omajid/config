;;; Code:

(defun my-dotnet-next-release ()
  "The month and year for the next (estimated) monthly release of .NET."
  (let* ((now (current-time))
         (two-weeks (seconds-to-time (* 2 7 24 60 60)))
         (computed-time (time-add now two-weeks)))
    (format-time-string "%B %Y" computed-time)))

;;; .yas-setup.el ends here
