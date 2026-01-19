(define-module (afistfullofash packages terraform)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (afistfullofash licenses)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system trivial)

  #:export (terraform
	    opentofu
	    opentofu-terraform-wrapper))

(define terraform
  (package
   (name "terraform")
   (version "1.14.3")
   (source (origin
            (method url-fetch/zipbomb)
            (uri (string-append "https://releases.hashicorp.com/terraform/" version "/terraform_" version "_linux_amd64.zip"))
            (sha256
             (base32
              "0kfvkrlbccdb5jlxp0ybjxsxh8qhrjifrb1j8ywnifsi49h2m2qp"))))
   (build-system copy-build-system)
   (arguments
    '(#:install-plan '(("terraform" "bin/terraform"))))
   (synopsis "Terraform is an infrastructure as code tool that lets you build, change, and version infrastructure safely and efficiently. This includes low-level components like compute instances, storage, and networking; and high-level components like DNS entries and SaaS features.")
   (description
    "Terraform is an infrastructure as code tool that lets you build, change, and version infrastructure safely and efficiently. This includes low-level components like compute instances, storage, and networking; and high-level components like DNS entries and SaaS features.")
   (home-page "https://developer.hashicorp.com/terraform")
   (license busl-1.1)))

(define opentofu
  (package
   (name "opentofu")
   (version "1.11.3")
   (source (origin
             (method url-fetch/tarbomb)
	     (uri (string-append "https://github.com/opentofu/opentofu/releases/download/v" version "/tofu_" version "_linux_amd64.tar.gz"))
            (sha256
             (base32
              "00xdk5rm6r6ja8ddjaffzra7j9q2dihz6dmra4an7yjc5m8ngda6"))))
   (build-system copy-build-system)
   (arguments
    '(#:install-plan '(("tofu" "bin/tofu"))))
   (synopsis "OpenTofu is an OSS tool for building, changing, and versioning infrastructure safely and efficiently. OpenTofu can manage existing and popular service providers as well as custom in-house solutions.")
   (description
    "OpenTofu is an OSS tool for building, changing, and versioning infrastructure safely and efficiently. OpenTofu can manage existing and popular service providers as well as custom in-house solutions.")
   (home-page "https://opentofu.org/")
   (license license:mpl2.0)))

(define opentofu-terraform-wrapper
  (package
    (inherit opentofu)
    (name "opentofu-terraform-wrapper")
    (source #f)
   (build-system trivial-build-system)
   (arguments
    (list #:modules '((guix build utils))
	  #:builder
	  #~(begin
	      (use-modules (guix build utils))
	      (let ((bin (string-append #$output "/bin"))
		    (opentofu (string-append #$opentofu "/bin/tofu")))
		(mkdir-p bin)
		(symlink opentofu (string-append bin "/terraform"))))))
   (propagated-inputs (list opentofu))
   (synopsis "Wrapper for OpenTofu")
   (description
    "This package provides a wrapper for opentofu so if can be invoked by @command{terraform} instead of @{tofu}.

To function properly this package should not be installed together with the @code{opentofu} package: this package uses the @code{opentofu} package as a propagated input, so installing this package already makes @code{tofu} available as a command.")))
