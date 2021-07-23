(ql:quickload :rove)

(defun run ()
  (and (rove:run :clx/tests :style :spec)))

(uiop:quit (if (run) 0 1))
