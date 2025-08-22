(define-module (afistfullofash packages node)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages node))

(define-public node-22.16
  (package
    (inherit node-bootstrap)
    (version "22.16.0")))
