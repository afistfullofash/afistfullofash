(define-module (afistfullofash-tests home-services)

  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (guix gexp)

  #:use-module (afistfullofash home services notification)
  #:use-module (afistfullofash packages themes)

  #:export (dunst-service-home-no-config
	    dunst-service-home-with-config
	    dunst-service-defun-home-dunst-config-modification
	    runst-service-home))

(define dunst-service-home-no-config
  (home-environment
   (services
    (list
     (service home-dunst-service-type)))))

(define %dunst-config
    (home-dunst-configuration
	     (base-config dunst-dracula-theme)
	     (config
	      (home-dunst-extra-config
	       (global
		(home-dunst-global-config
		 (frame-width 5)
		 (frame-color "#BD93F9")))))))

(define dunst-service-home-with-config
  (home-environment
   (services
    (list
     (service home-dunst-service-type
	      %dunst-config)))))

(define dunst-service-defun-home-dunst-config-modification
  (home-dunst-config-modification %dunst-config))

(define runst-service-home
  (home-environment
   (services
    (list
     home-runst-service))))

