(define-module (afistfullofash services runst)
  #:use-module (afistfullofash packages runst))

(define-public runst-service
  (simple-service
   'runst home-shepherd-service-type
   (list
    (shepherd-service
     (documentation "Run the runst notification daemon")
     (requirement '(x11-display))
     (auto-start? #t)
     (provision '(runst))
     (start #~(make-forkexec-constructor
               (list #$(file-append runst "/bin/runst"))))
     (stop #~(make-kill-destructor))))))
