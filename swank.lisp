(ql:quickload :swank)
(setf swank::*loopback-interface* "127.0.0.1")
(swank:create-server :port 4005 :style :spawn :dont-close t)
(loop while t do (sleep 1))
