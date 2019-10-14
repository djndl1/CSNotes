((c-mode . ((c-file-style . "linux")))
 (cc-mode . ((c-file-style . "stroustrup")))
 (java-mode . ((c-file-style . "java") (tab-width . 2))))

(dap-register-debug-template
 "gdb a.out"
 (list :type "gdb"
       :request "launch"
       :name "GDB::Run a.out"
       :target (f-join (lsp-workspace-root) "intro/a.out")
       :terminal ""
       :cwd (lsp-workspace-root)))

