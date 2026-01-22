(define-module (afistfullofash packages kustomize)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public kustomize
  (package
    (name "kustomize")
    (version "5.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
		    "https://github.com/kubernetes-sigs/kustomize/releases/download/kustomize%2Fv"
		    version
		    "/kustomize_v"
		    version
		    "_linux_amd64.tar.gz"))
              (sha256
               (base32
                "1vy504gr9624yzws73ry8f9mp9k6cxcxbcfjli52incd6l3q7yjd"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:install-plan
      #~'(("kustomize" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (copy-file source "./kustomize")
              (chmod "kustomize" #o644)))
          (add-before 'install 'chmod
            (lambda _
              (chmod "kustomize" #o555))))))
    (home-page "https://github.com/kubernetes-sigs/kustomize")
    (supported-systems '("x86_64-linux"))
    (synopsis "Kustomize command line tool")
    (description
     "kustomize lets you customize raw, template-free YAML files for multiple purposes, leaving the original YAML untouched and usable as is.")
    (license license:asl2.0)))


kustomize
