(in-package :potato-client-clim)

(defun parse-timestamp (s)
  (local-time:parse-timestring s))

(defun format-timestamp (stream ts)
  (local-time:format-timestring stream ts
                                :format (append local-time:+iso-8601-date-format+
                                                          '("T")
                                                          local-time:+iso-8601-time-format+
                                                          '("Z"))
                                :timezone local-time:+utc-zone+))
