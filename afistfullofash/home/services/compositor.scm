(define-module (afistfullofash home services compositor)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages compton)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)

  #:export (home-picom-service-type))

(define home-picom-service-type
  (service-type
   (name 'picom)
   (description "Run picom compositor.")
   (extensions
    (list
     ;; Ensure picom is available in your Home profile
     (service-extension home-profile-service-type
                        (lambda (_) (list picom
					  xwininfo
					  xprop)))

     ;; Provide the shepherd service
     (service-extension home-shepherd-service-type
                        (lambda (_)
                          (list
                           (shepherd-service
                            (documentation "picom compositor")
                            (provision '(picom))
                            (modules '((shepherd service)))
                            (start
                             #~(let* ((home (or (getenv "HOME") ""))
                                      (state (or (getenv "XDG_STATE_HOME")
                                                 (string-append home "/.local/state")))
                                      (log  (string-append state "/log/picom.log")))
                                 (make-forkexec-constructor
                                  (list #$(file-append picom "/bin/picom")
                                        "--backend" "glx" "--transparent-clipping")
                                  #:log-file log)))
                            (stop #~(make-kill-destructor))))))))
   (default-value #f)))
