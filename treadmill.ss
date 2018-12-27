(import :std/net/repl
        :std/format)

(def s (start-repl-server! address: "127.0.0.1:0"))
(let ((port (socket-info-port-number
             (tcp-server-socket-info
              (thread-specific s)))))
  (printf "Running net repl on port ~A.\n" port))
(thread-join! s)
