(define-module (afistfullofash packages rust-apps)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)

  
  #:use-module (gnu packages databases)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)


  #:use-module (afistfullofash packages rust-crates)

  #:export (runst
	    whiskers
	    diesel-cli
	    digikam-wallpaper))

(define runst
  (package
    (name "runst")
    (version "0.1.7")
    (source (origin
	      (method url-fetch)
              (uri (crate-uri "runst" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
		"0brvi6an9mx2hd8gn4ynrcmy9ni64n25dvyxr67xyr6swachg7ki"))))
    (build-system cargo-build-system)
    (arguments `(#:install-source? #f))
    (native-inputs (list pkg-config))
    (inputs (append
	     (list dbus
		   `(,zstd "lib")
		   glib
		   cairo
		   pango)
	     (cargo-inputs 'runst #:module '(afistfullofash packages rust-crates))))
    (synopsis "A dead simple notification daemon")
    (description
     "Desktop notifications are small, passive popup dialogs that notify the user of particular events in an asynchronous manner. These passive popups can automatically disappear after a short period of time.")
    (home-page "https://github.com/orhun/runst")
    (license (list license:expat
		   license:asl2.0))))

(define whiskers
  (package
    (name "whiskers")
    (version "2.5.1")
    (source
     (origin
	 (method git-fetch)
	 (uri (git-reference
	       (url "https://github.com/catppuccin/whiskers.git")
	       (commit (string-append "v" version))))
	 (file-name (git-file-name name version))
	 (sha256
	  (base32
	   "0ghgil48c2csr1q82q34mibb5xkv4730m255shpg9b02sg5igc9q"))))
    (build-system cargo-build-system)
    (arguments `(#:install-source? #f))
    (inputs 
     (cargo-inputs 'whiskers #:module '(afistfullofash packages rust-crates)))
    (synopsis "Catppuccin Theme Templating Tool")
    (description
     "Whiskers is a port creation helper tool that is custom-built for Catppuccin, allowing developers to define template files which the palette can be injected into.")
    (home-page "https://whiskers.catppuccin.com/")
    (license license:expat)))

(define diesel-cli
  (package
    (name "diesel-cli")
    (version "2.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diesel_cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gfwzz81w3nhpmj7m3gzi5l55k7finsz4pawz6w5lg64ihqwxysp"))))
    (build-system cargo-build-system)
    (inputs
     (append
      (list zlib
	    pkg-config
	    mysql
	    postgresql
	    sqlite
	    openssl)
      (cargo-inputs 'diesel-cli #:module '(afistfullofash packages rust-crates))))
    (arguments
     (list
      #:tests? #f))
    (home-page "https://diesel.rs")
    (synopsis "Provides the CLI for the Diesel crate")
    (description "This package provides the CLI for the Diesel crate.")
    (license (list license:expat license:asl2.0))))

(define digikam-wallpaper
  (package
    (name "digikam-wallpaper")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/afistfullofash/rust-digikam-orm.git")
	     (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0xhgd1l73bl727yb0ik7wyllypqvzhmdvra2v2l68jmmrv9xmwlr"))))
    (build-system cargo-build-system)
    (inputs (append
	     (list sqlite)
	     (cargo-inputs 'digikam-wallpaper #:module '(afistfullofash packages rust-crates))))
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/afistfullofash/rust-digikam-orm")
    (synopsis "Wallpaper Setting application leveraging digikam.")
    (description "Wallpaper Setting application leveraging digikam.")
    (license license:expat)))
