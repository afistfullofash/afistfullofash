(define-module (afistfullofash packages emacs-xyz)
    
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz)

  #:use-module ((guix licenses) #:prefix license:)

  #:export (emacs-darkman
	    emacs-lsp-scheme
	    emacs-just-mode
	    emacs-just-mode-2026))

(define emacs-lsp-scheme
  (package
    (name "emacs-lsp-scheme")
    (version "0.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacsmirror/lsp-scheme")
             (commit "1vi9wvh2ap0bbalckqa7x0xz9bmr481v1whs5l0bwgw411h7wpaj")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yn5cc6cmj3hwqgmjj44dz847xn5k99kirj36qwc04q7vhl8z8k7"))))
    (build-system emacs-build-system)
    (arguments (list
		#:tests? #f))
    (native-inputs (list emacs-s
			 emacs-dash
			 emacs-f
			 emacs-xcscope
			 emacs-popup
			 emacs-auto-complete
			 emacs-company
			 emacs-helm))
    
    (home-page "https://github.com/xcwen/ac-php")
    (propagated-inputs (list emacs-lsp-mode))
    (synopsis "Emacs Auto Complete & Company mode for PHP")
    (description
     "This package provides Auto Complete and Company back-ends for PHP.")
    (license license:gpl3+)))

(define emacs-darkman
  (package
    (name "emacs-darkman")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://git.sr.ht/~grtcdr/darkman.el/archive/" version ".tar.gz"))
       (sha256
        (base32 "08vlx2nsvsc75fd8xvllc6dibw8yakys1q0a41hcl8sdcnprh26w"))))
    (build-system emacs-build-system)
    (arguments (list
		#:tests? #f))
    (home-page "https://darkman.grtcdr.tn/")
    (synopsis "Darkman for Emacs")
    (description
     "Darkman for Emacs")
    (license license:isc)))

(define emacs-just-mode
  (package
    (name "emacs-just-mode")
    (version "0.1.8")
    (source
     (origin
	 (method git-fetch)
	 (uri (git-reference
	       (url "https://github.com/leon-barrett/just-mode.el.git")
	       (commit version)))
	 (file-name (git-file-name name version))
	 (sha256
	  (base32
	   "103jwkmg3dphmr885rpbxjp3x8xw45c0zbcvwarkv4bjhph8y4vh"))))
    (build-system emacs-build-system)
    (arguments (list
		#:tests? #f))
    (home-page "https://github.com/leon-barrett/just-mode.el")
    (synopsis "Justfile Emacs Major Mode")
    (description
     "Justfile Emacs Major Mode")
    (license license:gpl3)))

(define emacs-just-mode-2026
  (let ((commit "b6173c7bf4d8d28e0dbd80fa41b9c75626885b4e")
	(version "2026")
	(revision "0"))
    (package
      (inherit emacs-just-mode)
      (name "emacs-just-mode")
      (version (git-version version revision commit))
      (source
       (origin
	 (method git-fetch)
	 (uri (git-reference
		(url "https://github.com/leon-barrett/just-mode.el.git")
		(commit commit)))
	 (file-name (git-file-name name version))
	 (sha256
	  (base32
	   "1czf779akdcx72ma7x9v70kjbic73312fi1czbzvlvxr01pjpyj0")))))))
