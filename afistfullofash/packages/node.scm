(define-module (afistfullofash packages node)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages node))

(define-public node-22.16
  (package
    (inherit node-lts)
    (name "node-22.16")
    (version "22.16.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.gz"))
              (sha256
               (base32
                "1cnsxcjp7b6s38bif8cndi76x1plc3x43vxkcjs07hcwyw7jb3qh"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; openssl.cnf is required for build.
                  (for-each delete-file-recursively
                            (find-files "deps/openssl"
                                        (lambda (file stat)
                                          (not (string-contains file "nodejs-openssl.cnf")))))
                  ;; Remove bundled software, where possible
                  (for-each delete-file-recursively
                            '("deps/brotli"
                              "deps/cares"
                              "deps/icu-small"
                              "deps/nghttp2"
                              "deps/ngtcp2"
                              "deps/uv"
                              "deps/zlib"))))))))
