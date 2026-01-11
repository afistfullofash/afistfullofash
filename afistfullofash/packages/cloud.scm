(define-module (afistfullofash packages cloud)

  #:use-module (guix packages)
  #:use-module (guix download)
  
  #:use-module (guix build-system copy)
  

  #:use-module (guix gexp)
  #:use-module (guix utils)
  
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  
  #:use-module (gnu packages python)

  #:use-module ((guix licenses) #:prefix license:)
  
  #:export (google-cloud-cli))

(define-public google-cloud-cli
  (package
    (name "google-cloud-cli")
    (version "551.0.0")
    (source
     (origin
       (method url-fetch)
       
       (uri (string-append
	     "https://dl.google.com/dl/cloudsdk/release/downloads/for_packagers/linux/google-cloud-cli_"
	     version
	     ".orig_amd64.tar.gz"))
            (sha256
             (base32
              "0c9sigkf8xpqvcpam8xzyv689gaji77vwy1k0l31asrmi563vc6a"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("bin" "bin")
			("lib" "lib")
			("data/cli/gcloud_completions.py" "data/cli/gcloud_completions.py")
			("platform/ext-runtime" "platform/ext-runtime")
			("platform/gcloud-crc32c_licenses" "platform/gcloud-crc32c_licenses"))))
    (propagated-inputs (list python-wrapper))
    (home-page "https://cloud.google.com/cli/")
    (synopsis
     "A core set of command-line tools for the Google Cloud Platform. Includes only gcloud core (with beta and alpha commands), gcloud-crc32c and man pages")
    (description
     "A core set of command-line tools for the Google Cloud Platform. Includes only gcloud core (with beta and alpha commands), gcloud-crc32c and man pages")
    (license #f)))
