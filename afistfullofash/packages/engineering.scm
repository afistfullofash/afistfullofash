(define-module (afistfullofash packages engineering)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  
  #:export (freecad-woodworking-addon))

(define freecad-woodworking-addon
  (package
    (name "freecad-woodworking-addon")
    (version "2.0")
    (source 
       (origin
	 (method git-fetch)
	 (uri (git-reference
	       (url "https://github.com/dprojects/Woodworking.git")
	       (commit version)))
	 (file-name (git-file-name name version))
	 (sha256
	  (base32
	   "1dad0va9kaxj4j5whfg3lb5cwrbazsr173rqf5sb4q0y37s4w9gk"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("." "share/FreeCAD/Mod/Woodworking/"))))
    (home-page "https://github.com/dprojects/Woodworking.git")
    (description "FreeCAD Woodworking Addon")
    (synopsis "FreeCAD Woodworking Addon")
    (license license:expat)))
