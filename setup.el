;;; setup.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 djndl1
;;
;; Author: djndl1 <djndl1@gmail.com>
;; Maintainer: djndl1 <djndl1@gmail.com>
;; Created: 六月 08, 2023
;; Modified: 六月 08, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/djn/setup
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(require 'ox-publish)
(setq url-proxy-services '(("http" . "http://djn:freebird@43.128.11.210:8888")
                           ("https" . "http://djn:freebird@43.128.11.210:8888"))
(setq url-proxy-services '(("no_proxy" . ".*")))
(setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/htmlize.css\"/>
         <link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/readtheorg.css\"/>
         <script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script>
         <script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>
         <script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/src/lib/js/jquery.stickytableheaders.min.js\"></script>
         <script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/js/readtheorg.js\"></script>")

(setq org-publish-project-alist
      '(("CSNotes"
        :base-directory "~/Notes/CSNotes"
        :base-extension "org"
        :publishing-directory "~/build/CSNotes"
        :recursive t
        :publishing-function org-html-publish-to-html
        :headline-levels 10
        :with-broken-links t
        :auto-sitemap t
        :auto-preamble t
        :html-head-include-default-style nil
        :html-head-extra
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/htmlize.css\"/>
         <link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/readtheorg.css\"/>
         <script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script>
         <script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>
         <script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/src/lib/js/jquery.stickytableheaders.min.js\"></script>
         <script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/js/readtheorg.js\"></script>"
        :with-sub-superscript nil)))
