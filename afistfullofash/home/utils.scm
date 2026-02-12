(define-module (afistfullofash home utils)
  #:use-module (gnu home)
  #:use-module (guix gexp)
  #:use-module (guix utils)

  #:export (home-directory
	    home-file-path
	    
	    environment-variable-seperated-path))

(define home-directory (getenv "HOME"))

(define (home-file-path file)
  (string-append home-directory file))

(define (environment-variable-seperated-path items)
  (string-join (map home-file-path items) ":"))
