

(pushnew (hunchentoot-cgi::create-cgi-dispatcher-and-handler
          "/cgi-bin/"
          (merge-pathnames "cgi/" (ch-asdf:asdf-lookup-path "asdf:/hunchy")))
         (hunchentoot-vhost::dispatch-table hunchy::*cyrusharmon-host*) :test #'equal)

