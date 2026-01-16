(define-module (afistfullofash packages cloud)

  #:use-module (guix packages)
  #:use-module (guix download)
  
  #:use-module (guix build-system copy)
  

  #:use-module (guix gexp)
  #:use-module (guix utils)
  
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  
  #:use-module (gnu packages python)
  #:use-module (nongnu packages k8s)

  #:use-module ((guix licenses) #:prefix license:)
  
  #:export (google-cloud-cli
	    google-cloud-cli-auth-plugin
	    google-cloud-cli-config-connector))

;; This is shared across all cli components regardless of origin
(define %google-cloud-cli-version "551.0.0")

;; Don't require a redefinition for gcloud, gsutil and bg
(define %google-cloud-cli-origin
  (origin
       (method url-fetch)
       
       (uri (string-append
	     "https://dl.google.com/dl/cloudsdk/release/downloads/for_packagers/linux/google-cloud-cli_"
	     %google-cloud-cli-version
	     ".orig_amd64.tar.gz"))
            (sha256
             (base32
              "0c9sigkf8xpqvcpam8xzyv689gaji77vwy1k0l31asrmi563vc6a"))))

(define %google-cloud-cli-component-base-url "https://dl.google.com/dl/cloudsdk/channels/rapid/components/")

(define %google-cloud-cli-sdk-root "/opt/google-cloud-sdk")

(define (google-cloud-cli-install-plan paths)
  ;; ((thing extend))
  (map (lambda (path-pair)
	 (list (car path-pair)
	       (string-append %google-cloud-cli-sdk-root "/" (cadr path-pair))))
       paths))

(define-public google-cloud-cli
  (package
    (name "google-cloud-cli")
    (version "551.0.0")
    (source
     %google-cloud-cli-origin)
    (build-system copy-build-system)
    (arguments
     `(#:phases
       ,#~(modify-phases %standard-phases
	    (add-after 'install 'enable-alpha-and-beta-components
	      (lambda* (#:key inputs outputs #:allow-other-keys)
		(let* ((out (assoc-ref outputs "out"))
		      (alpha-file #$(local-file 
				     "files/google-cloud-cli/alpha.__init__.py"))
		      (alpha-dest (string-append out #$%google-cloud-cli-sdk-root
				     "/lib/surface/alpha/__init__.py"))
		      (beta-file #$(local-file 
				    "files/google-cloud-cli/beta.__init__.py"))
		      (beta-dest (string-append out #$%google-cloud-cli-sdk-root
						"/lib/surface/beta/__init__.py")))
		  (mkdir-p alpha-dest)
		  (copy-file alpha-file alpha-dest)

		  (mkdir-p beta-dest)
		  (copy-file beta-file beta-dest))))
	    (add-after 'install 'configure-properties
	      (lambda* (#:key inputs outputs #:allow-other-keys)
		(let* ((out (assoc-ref outputs "out"))
		       (properties-file #$(local-file "files/google-cloud-cli/properties"))
		       (dest (string-append out
					    #$%google-cloud-cli-sdk-root
					    "/properties")))
		  (copy-file properties-file dest))))
            (add-after 'install 'wrap-gcloud
              (lambda* (#:key inputs outputs #:allow-other-keys)
		(let* ((out (assoc-ref outputs "out"))
		      (gcloud (string-append out #$%google-cloud-cli-sdk-root "/bin/gcloud"))
		      (python (assoc-ref inputs "python-wrapper")))
		  ;; This creates the /bin/gcloud entry point with the env var
		  (wrap-program gcloud
		    `("CLOUDSDK_PYTHON" = (,(string-append python "/bin/python")))
		    `("CLOUDSDK_ROOT_DIR" = (,(string-append out #$%google-cloud-cli-sdk-root)))
                    `("CLOUDSDK_COMPONENT_MANAGER_DISABLE_UPDATE_CHECK" = ("true")))

		  (mkdir-p (string-append out "/bin"))
		  (symlink gcloud (string-append out "/bin/gcloud"))))))
       #:install-plan ',(google-cloud-cli-install-plan
			 (list '("bin" "bin")
			       '("lib" "lib")
			       '(".install" ".install")                            
			       '("data/cli/gcloud_completions.py" "data/cli/gcloud_completions.py")
			       '("platform/ext-runtime" "platform/ext-runtime")
			       '("platform/gcloud-crc32c_licenses" "platform/gcloud-crc32c_licenses")))))
    (propagated-inputs (list python-wrapper))
    (home-page "https://cloud.google.com/cli/")
    (synopsis
     "A core set of command-line tools for the Google Cloud Platform. Includes only gcloud core (with beta and alpha commands), gcloud-crc32c and man pages")
    (description
     "A core set of command-line tools for the Google Cloud Platform. Includes only gcloud core (with beta and alpha commands), gcloud-crc32c and man pages")
    (license license:asl2.0)))

(define-public google-cloud-cli-gke-gcloud-auth-plugin
  (package
    (name "google-cloud-cli-gke-gcloud-auth-plugin")
    (version %google-cloud-cli-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://dl.google.com/dl/cloudsdk/release/downloads/for_packagers/linux/google-cloud-cli-gke-gcloud-auth-plugin_"
	     %google-cloud-cli-version
	     ".orig_amd64.tar.gz"))
       (sha256
	(base32
         "0lvimdwz5ppkr5s9k91p1kaag4nz3xh3j3i79qgiilfpy644xvq7"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("bin" "bin"))))
    (propagated-inputs (list kubectl google-cloud-cli))
    (home-page "https://cloud.google.com/blog/products/containers-kubernetes/kubectl-auth-changes-in-gke")
    (synopsis "A google-cloud-cli component that provides a kubectl authentication plugin for GKE.")
    (description "A google-cloud-cli component that provides a kubectl authentication plugin for GKE.")
    (license license:asl2.0)))

(define-public google-cloud-cli-config-connector
  (package
    (name "google-cloud-cli-config-connector")
    (version %google-cloud-cli-version)
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append
	    %google-cloud-cli-component-base-url
	     "google-cloud-sdk-config-connector-linux-x86_64-20251024121634.tar.gz"))
       (sha256
	(base32
         "18c9xgh7b307j35mw567dcavkji2hnmaydi3zib9d3in47d36nq3"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("bin" "bin")
			("platform" "platform"))))
    (propagated-inputs (list kubectl google-cloud-cli))
    (home-page "https://cloud.google.com/blog/products/containers-kubernetes/kubectl-auth-changes-in-gke")
    (synopsis "Google Cloud Config Connector. See https://cloud.google.com/config-connector/docs/overview")
    (description "Google Cloud Config Connector. See https://cloud.google.com/config-connector/docs/overview")
    (license license:asl2.0)))
