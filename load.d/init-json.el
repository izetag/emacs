(require 'package)

(defvar json-packages '(json-mode))

(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
