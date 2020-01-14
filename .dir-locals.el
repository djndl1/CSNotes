((c-mode . ((c-file-style . "linux")))
 (cc-mode . ((c-file-style . "stroustrup")))
 (java-mode . ((c-file-style . "java") (tab-width . 4))))

(dap-register-debug-template
 "gdb a.out"
 (list :type "gdb"
       :request "launch"
       :name "GDB::Run a.out"
       :target (f-join (lsp-workspace-root) "intro/a.out")
       :terminal "xterm"
       :cwd (lsp-workspace-root)))

(dap-register-debug-template "Java debug a.out"
                             (list :type "java"
                                   :request "attach"
                                   :hostName "localhost"
                                   :port 1044))
