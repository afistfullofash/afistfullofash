(define-module (afistfullofash packages mail)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  
  #:use-module (gnu packages check)
  #:use-module (gnu packages mail)

  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)

  #:export (lieer))

(define lieer
  (package
    (name "lieer")
    (version "1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/gauteh/lieer")
              (commit (string-append "v" version))))
       (sha256
        (base32
         "1da36mhzb7gzfw2xv80c3cnz7hr7xfm4h6zpmv46n6i8gvmrhzsk"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-google-api-client
	   python-google-auth-oauthlib
	   python-notmuch2
	   python-tqdm))
    (home-page "https://lieer.gaute.vetsj.com/")
    (synopsis "Fast email-fetching, sending, and two-way tag synchronization between notmuch and GMail")
    (description
     "This program can pull, and send, email and labels (and changes to labels) from your GMail account and store them locally in a maildir with the labels synchronized with a notmuch database. The changes to tags in the notmuch database may be pushed back remotely to your GMail account.")
    (license license:gpl3+)))
