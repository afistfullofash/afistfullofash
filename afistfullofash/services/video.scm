(define-module (afistfullofash services video)
  #:use-module (guix gexp)
  
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)

  #:use-module (afistfullofash packages video)
  
  #:export (motioneye-service-type))

(define motioneye-service-type
  (service-type
    (name 'motioneye)
   (extensions
    (list
     ;; 3. Define the Shepherd daemon
     (service-extension shepherd-root-service-type
                        (lambda _
                          (list (shepherd-service
				  (provision '(motioneye))
				  (requirement '(networking))
                                 (start #~(make-forkexec-constructor
                                           (list #$(file-append motioneye "/bin/meyectl")
						 "startserver "
						 "-c"
						 "/etc/motioneye/motioneye.conf")))

                                 (stop #~(make-kill-destructor))))))))
   (default-value #f)
   (description "Motioneye Web Frontend for Motion.")))
