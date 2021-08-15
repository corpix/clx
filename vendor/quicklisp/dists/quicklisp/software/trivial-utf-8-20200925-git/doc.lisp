(in-package :trivial-utf-8)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import 'mgl-pax:writer)
  (use-package :mgl-pax))

(defsection @trivial-utf-8-manual (:title "Trivial UTF-8 Manual")
  (trivial-utf-8 asdf:system)
  (@trivial-utf-8-introduction section)
  (@trivial-utf-8-links section)
  (@trivial-utf-8-reference section))

(defsection @trivial-utf-8-introduction (:title "Introduction")
  "Trivial UTF-8 is a small library for doing UTF-8-based in- and
  output on a Lisp implementation that already supports Unicode -
  meaning CHAR-CODE and CODE-CHAR deal with Unicode character codes.

  The rationale for the existence of this library is that while
  Unicode-enabled implementations usually do provide some kind of
  interface to dealing with character encodings, these are typically
  not terribly flexible or uniform.

  The [Babel][babel] library solves a similar problem while
  understanding more encodings. Trivial UTF-8 was written before Babel
  existed, but for new projects you might be better off going with
  Babel. The one plus that Trivial UTF-8 has is that it doesn't depend
  on any other libraries.

    [babel]: https://common-lisp.net/project/babel/")

(defsection @trivial-utf-8-links (:title "Links")
  "Here is the [official repository][trivial-utf-8-repo] and the
  [HTML documentation][trivial-utf-8-doc] for the latest version.

    [trivial-utf-8-repo]: https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8
    [trivial-utf-8-doc]: http://melisgl.github.io/mgl-pax-world/trivial-utf-8-manual.html")

(defsection @trivial-utf-8-reference (:title "Reference")
  (utf-8-byte-length function)
  (string-to-utf-8-bytes function)
  (utf-8-group-size function)
  (utf-8-bytes-to-string function)
  (read-utf-8-string function)
  (utf-8-decoding-error condition))


;;;; Register in PAX World

(defun pax-sections ()
  (list @trivial-utf-8-manual))
(defun pax-pages ()
  `((:objects
     (, @trivial-utf-8-manual)
     :source-uri-fn
     ,(make-github-source-uri-fn
       :trivial-utf-8
       "https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8/-/"))))
(register-doc-in-pax-world :trivial-utf-8 (pax-sections) (pax-pages))

#+nil
(progn
  (update-asdf-system-readmes @trivial-utf-8-manual :trivial-utf-8)
  (update-asdf-system-html-docs @trivial-utf-8-manual :trivial-utf-8
                                :pages (pax-pages)))
