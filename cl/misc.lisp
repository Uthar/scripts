
(defun human-readable-age (seconds)
  (cond
    ((< seconds 1) "now")
    ((< seconds 120) (format nil "~d seconds" seconds))
    ((< seconds 3600) (format nil "~d minutes" (truncate seconds 60)))
    ((< seconds (* 3600 36)) (format nil "~d hours" (truncate seconds 3600)))
    ((< seconds (* 3600 24 365)) (format nil "~d days" (truncate seconds (* 3600 24))))
    (t (format nil "~,1f years" (/ seconds 3600 24 365)))))
