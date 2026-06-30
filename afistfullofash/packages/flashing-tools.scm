(define-module (afistfullofash packages flashing-tools)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages linux)
  #:use-module (afistfullofash packages python-xyz)
  
  #:export (mtkclient))

(define-public mtkclient
  (package
    (name "mtkclient")
    (version "2.1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/bkerler/mtkclient.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dm07aiy32p0q8zqk61bf8yhh173dg4xws6iw9f1i6lx1z5nv3zi"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      ; Shiboken6/PySide6 lack standard .dist-info metadata, breaking automated checks
      #~(modify-phases %standard-phases
	  (delete 'sanity-check))))
    (native-inputs
     (list python-certifi
           python-pytest
           python-setuptools
	   python-hatchling))
    (propagated-inputs
     (list python-pyusb
	   python-pycryptodome
	   python-pycryptodomex
	   python-colorama
	   python-shiboken-6
	   python-pyside-6
	   python-pyserial
	   python-fusepy
	   fuse-2))
    (home-page "https://github.com/bkerler/mtkclient")
    (synopsis "Mediatek Flash and Repair Utility")
    (description
     "Mediatek Flash and Repair Utility")
    (license license:gpl3)))

mtkclient
