(ql:quickload 'mh-dex)
(sb-ext:save-lisp-and-die "mh-dex-server"
                          :toplevel (lambda ()
                                      (mh-dex.server:dex :start)
                                      (loop while true
                                         do (sleep 100))))
