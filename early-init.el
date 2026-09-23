;; -*- lexical-binding: t; -*-

(defvar ls/cache-directory (expand-file-name ".cache/" user-emacs-directory)
  "Directory used for cached files")
(defvar ls/templates-directory (expand-file-name "templates/" user-emacs-directory)
  "Directory used for templates")

(startup-redirect-eln-cache (expand-file-name "eln/" ls/cache-directory))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

(setq package-enable-at-startup nil)
