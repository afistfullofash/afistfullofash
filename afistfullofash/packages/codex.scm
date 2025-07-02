(define-module (afistfullofash packages codex)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy))

(define-public codex
  (package
   (name "codex")
   (version "0.2.0")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (string-append "https://github.com/openai/codex/releases/download/rust-v" version "/codex-x86_64-unknown-linux-gnu.tar.gz"))
            (sha256
             (base32
              "18xj9489lh6sr95g66d6cww3l0p141snb8i12fkznw41z09ql8dv"))))
   (build-system copy-build-system)
   (synopsis "A coding agent from OpenAI that runs locally on your computer.")
   (description
    "A coding agent from OpenAI that runs locally on your computer.")
   (home-page "https://github.com/openai/codex")
   (license license:asl2.0)))
