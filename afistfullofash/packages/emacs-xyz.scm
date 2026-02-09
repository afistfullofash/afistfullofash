(define-module (afistfullofash packages emacs-xyz)
    
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz)

  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-lsp-scheme
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



(define-public emacs-nyan-mode-1.1.4
  (package
    (inherit emacs-nyan-mode)
    (name "emacs-nyan-mode-1.1.4")
    (version "1.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TeMPOraL/nyan-mode/")
             (commit "09904af23adb839c6a9c1175349a1fb67f5b4370")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03xp4dvq3y3q9xyb6pm9m5gb756rvbxcqk52ind08n7prqv4w1lp"))))))

(define-public emacs-darkman
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
