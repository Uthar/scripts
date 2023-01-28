(defsystem http
  :components ((:file concurrent)
               (:file encode)
               (:file socket)
               (:file gnutls)
               (:file client))
  :depends-on (trivial-gray-streams
               split-sequence
               alexandria
               cffi
               bordeaux-threads))
