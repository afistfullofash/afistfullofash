(define-module (afistfullofash packages package-management)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)

  #:use-module (gnu packages c)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  
  #:use-module (afistfullofash packages rust-crates)

  #:export (cargo-dist))

(define cargo-dist
  (package
    (name "cargo-dist")
    (version "0.31.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-dist" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w87h94gs1plvd65qy3p2b61k5xslhcasp09f48dsaj7a34gqbra"))))
    (build-system cargo-build-system)
    (native-inputs (list tcc
			 pkg-config))
    (inputs (append
	     (list zlib
		   `(,zstd "lib"))
	     (cargo-inputs 'cargo-dist  #:module '(afistfullofash packages rust-crates))))
    (arguments
     (list
      #:tests? #f))
    (home-page "https://axodotdev.github.io/cargo-dist")
    (synopsis "Shippable application packaging for Rust")
    (description
     "This package provides Shippable application packaging for Rust.")
    (license (list license:expat license:asl2.0))))
