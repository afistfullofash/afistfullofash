(define-module (afistfullofash home services mail)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)

  #:use-module (gnu packages mail)
  
  #:export (home-hydroxide-service))


(define home-hydroxide-service
  (simple-service
   'hydroxide home-shepherd-service-type
   (list
    (shepherd-service
     (documentation "Run hydroxide proton bridge")
     (requirement '())
     (auto-start? #t)
     (one-shot? #f)
     (provision '(hydroxide))
     (start #~(make-forkexec-constructor
               (list #$(file-append hydroxide "/bin/hydroxide") "imap")))
     (stop #~(make-kill-destructor))))))

