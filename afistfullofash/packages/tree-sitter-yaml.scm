(define-module (afistfullofash packages tree-sitter-yaml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages crates-io)

  #:use-module (afistfullofash packages crates-io))

(define-public tree-sitter-yaml
  (package
    (name "tree-sitter-yaml")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tree-sitter-yaml" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i69iwniz530lks3pzzxp1l5q7ks2vnkmaiq4dmchmsyl3r96n1x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-tree-sitter-language" ,rust-tree-sitter-language-0.1))
       #:cargo-development-inputs (("rust-tree-sitter" ,rust-tree-sitter-0.25))))
    (home-page "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
    (synopsis "YAML grammar for tree-sitter")
    (description "This package provides YAML grammar for tree-sitter.")
    (license license:expat)))
