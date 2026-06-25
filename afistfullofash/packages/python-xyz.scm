(define-module (afistfullofash packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)

  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages linux)
  
  #:export (python-mfusepy))


(define python-mfusepy
  (package
    (name "python-mfusepy")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri ((@ (guix build-system pyproject) pypi-uri) "mfusepy" version))
       (sha256
        (base32 "1qbhjp8y271sd2i90d50jv7jwhy7l1wpd0wjjig1lz9xa5acx3ik"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-libfuse-path
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((libfuse (search-input-file inputs "/lib/libfuse3.so.4")))
                (substitute* "mfusepy.py"
                  ;; Inject the absolute store path directly before the CDLL initialization
                  (("_libfuse = ctypes\\.CDLL\\(_libfuse_path\\)")
                   (format #f "_libfuse_path = ~s\n_libfuse = ctypes.CDLL(_libfuse_path)" libfuse)))))))))
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list fuse))
    (home-page "https://github.com/mxmlnkn/mfusepy")
    (synopsis "Python  Ctypes bindings for the high-level API in libfuse 2 and 3")
    (description
     "mfusepy is a Python module that provides a simple interface to FUSE and macFUSE.")
    (license license:asl2.0)))
