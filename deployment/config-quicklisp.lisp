(quicklisp-quickstart:install)
(ql:add-to-init-file)
;; Switch to the 2015-08-04 dist, since jonathan seems to be
;; malfunctioning in newer versions.
(ql-dist:install-dist "http://beta.quicklisp.org/dist/quicklisp/2015-08-04/distinfo.txt" :replace t)
(sb-ext:exit)
