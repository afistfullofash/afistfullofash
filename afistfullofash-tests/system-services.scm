(define-module (afistfullofash-tests system-services)
  #:use-module (gnu) 
  #:use-module (gnu system)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (guix channels)

  #:use-module (afistfullofash services firmware)
 
  
  #:export (service-test-operating-system))

(define %afistfullofash-channel-services
  (list (service fwupd-service-type)))

(define service-test-operating-system
  (operating-system
    (host-name "service-test")
    (locale "en_AU.utf8")
    (timezone "Australia/Sydney")
    (keyboard-layout (keyboard-layout "us"))
    (users (cons* (user-account
		   (name "test")
		   (comment "test")
		   (group "users")
		   (home-directory "/home/test"))
		  %base-user-accounts))

    (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/sdX"))))
    
    (file-systems (cons (file-system
                        (device (file-system-label "my-root"))
                        (mount-point "/")
                        (type "ext4"))
			%base-file-systems))
    
    (services (append %afistfullofash-channel-services
		      %base-services))))
