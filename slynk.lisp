(ql:quickload :slynk)
(setf slynk::*loopback-interface* "127.0.0.1")
(slynk:create-server :port 4005 :style :spawn :dont-close t)
(loop while t do (sleep 1))
