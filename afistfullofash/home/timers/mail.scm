(define-module (afistfullofash home timers mail)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:use-module (gnu packages mail)
  #:use-module (afistfullofash home utils)
  #:use-module (afistfullofash packages mail)
  
  #:export (home-mail-sync-timer))

(define home-mail-sync-timer
  (let ((email-sync-script
	 (program-file
	  "sync-mail"
	  (let* ((mail-dir (home-file-path "/mail"))
		 (work-mail-dir (string-append mail-dir "/work/"))
		 (notmuch-config-file (home-file-path "/.config/notmuch/default/config"))
		 (gmi-bin (file-append lieer "/bin/gmi"))
		 (mbsync-bin (file-append isync "/bin/mbsync"))
		 (notmuch-bin (file-append notmuch "/bin/notmuch"))
		 (afew-bin (file-append afew "/bin/afew")))
	    #~(begin
		(unless (zero? (system* #$gmi-bin "pull" "-C" #$work-mail-dir))
		  (error "Lieer pull failed"))
		
		(unless (zero? (system* #$mbsync-bin "-a" "--pull"))
		  (error "mbsync pull failed"))

		(unless (zero? (system* #$notmuch-bin "new"))
		  (error "Notmuch indexing failed"))

		(unless (zero? (system* #$afew-bin
					"--notmuch-config"
					#$notmuch-config-file
					"--tag"
					"--new"))
		  (error "Afew Tagging failed"))

		(unless (zero? (system* #$gmi-bin "push" "-C" #$work-mail-dir))
		  (error "Lieer push failed"))

		(unless (zero? (system* #$mbsync-bin "-a" "--push"))
		  (error "mbsync push failed")))))))
    (shepherd-timer '(mail-sync)
		    "*/2 * * * *"
		    #~(#$email-sync-script)
		    #:requirement '())))
