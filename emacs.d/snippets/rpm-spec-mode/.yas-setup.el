
(defun my-changelog-version-release ()
  "The version-release for the end of a rpm-changelog entry line.

That string includes the main package version and release, but not the dist tag itself."
  (let* ((command-and-args (concat "rpmspec -q " (buffer-file-name) " --queryformat '%{VERSION}-%{RELEASE}\n'"))
         (rpmspec-version-release-output (shell-command-to-string command-and-args))
         (version-and-release-original (nth 0 (split-string rpmspec-version-release-output "\n" 't)))
         (rpmspec-dist-output (shell-command-to-string "rpmspec -E '%{dist}'"))
         (dist-tag (string-trim rpmspec-dist-output))
         (version-and-release-final (string-trim-right version-and-release-original dist-tag)))
    ;; (print command-and-args)
    ;; (print rpmspec-version-release-output)
    ;; (print version-and-release-original)
    ;; (print rpmspec-dist-output)
    ;; (print version-and-release-final)
    version-and-release-final))
