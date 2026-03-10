(define-module (afistfullofash packages rust-sources)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  
  #:use-module (gnu packages databases)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  
  #:use-module (afistfullofash packages rust-crates)
  
  #:export (rust-mysqlclient-sys
	    rust-openssl-sys))

(define-public rust-mysqlclient-sys
  (package
    (name "rust-mysqlclient-sys")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mysqlclient-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n5riymgivihmn00rgrk7r0y926jnjms53kgx8m07i6gy0977vcj"))))
    (build-system cargo-build-system)
    (propagated-inputs (list mysql))
    (inputs (append (list pkg-config
			  zlib)
		    (cargo-inputs 'mysqlclient-sys
				  #:module '(afistfullofash packages rust-crates))))
    (arguments
     (list #:skip-build? #t))
    (home-page "https://github.com/sgrif/mysqlclient-sys")
    (synopsis "Auto-generated rust bindings for libmysqlclient")
    (description
     "This package provides Auto-generated rust bindings for libmysqlclient.")
    (license (list license:expat license:asl2.0))))

(define-public rust-openssl-sys
  (package
    (name "rust-openssl-sys")
    (version "0.9.111")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openssl-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08f3mpsabivfi3fd0qv9231qidqy68lr8a4qi32y6xda43av5jl2"))))
    (build-system cargo-build-system)
    (inputs
     (append
      (list openssl)
      (cargo-inputs 'openssl-sys #:module '(afistfullofash packages rust-crates))))
    (arguments
     (list #:skip-build? #t))
    (home-page "https://github.com/rust-openssl/rust-openssl")
    (synopsis "FFI bindings to OpenSSL")
    (description "This package provides FFI bindings to @code{OpenSSL}.")
    (license license:expat)))
