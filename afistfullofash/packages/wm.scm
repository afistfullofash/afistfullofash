(define-module (afistfullofash packages wm)
  #:use-module (guix packages)
  #:use-module (guix utils)

  #:use-module (gnu packages commencement)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages lisp-xyz)
  
  #:export (stumpwm-extension-builder
	    stumpwm-with-user-extensions))

(define %default-stumpwm-extensions
  `((,sbcl-trivia . "trivia")
    (,sbcl-local-time . "local-time")
    (,sbcl-stumpwm-battery-portable . "battery-portable")
    (,sbcl-stumpwm-notify . "notify")
    (,sbcl-stumpwm-pamixer . "pamixer")
    (,sbcl-stumpwm-ttf-fonts . "ttf-fonts")
    (,sbcl-clx-truetype . "clx-truetype")
    (,sbcl-slynk . "slynk")))

(define-public (stumpwm-extension-builder extensions)
  (let ((extension-packages (map car extensions))
	(extension-systems (map cdr extensions)))
    (package
      (inherit stumpwm)
      (name "stumpwm-with-extensions")
      (inputs
       (append (list stumpwm)
	       extension-packages))
      (native-inputs
       (list pkg-config
             gcc-toolchain
	     autoconf
	     texinfo))
      (arguments
       (substitute-keyword-arguments (package-arguments stumpwm)
	 ((#:phases phases)
          `(modify-phases ,phases
             (replace 'build-program
               (lambda* (#:key inputs outputs #:allow-other-keys)
		 (let* ((out (assoc-ref outputs "out"))
			(program (string-append out "/bin/stumpwm")))
                   ;; Avoid SBCL poking homedir during image build
                   (setenv "HOME" "/tmp")
                   (build-program program outputs
				  ;; Start stumpwm normally
				  #:entry-program '((stumpwm:stumpwm) 0)
				  ;; ASDF systems to pre-bundle
				  #:dependencies (append '("stumpwm")
							 ',extension-systems)
				  ;; Where to find those systems
				  #:dependency-prefixes
				  (map cdr (filter (lambda (input)
                                                     (string-prefix? "/gnu/store"
								     (cdr input)))
                                                   inputs))))))
             (delete 'copy-source)
             (delete 'build)
             (delete 'check)
             (delete 'remove-temporary-cache)
             (delete 'cleanup))))))))

(define-public stumpwm-with-user-extensions
  (stumpwm-extension-builder %default-stumpwm-extensions))
