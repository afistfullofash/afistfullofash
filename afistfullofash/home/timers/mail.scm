(define-module (afistfullofash home timers mail)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:use-module (gnu packages wm)
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
		 (afew-bin (file-append afew "/bin/afew"))
		 (dunstify (file-append dunst "/bin/dunstify"))
		 (send-notification
		  (lambda (title message)
		    (system* #$dunstify "-a" title "-u" "info" message)))
		 (send-error-notification
		  (lambda (title message)
		    (system* #$dunstify "-a" title "-u" "critical" message))))
	    #~(begin
		(use-modules (afistfullofash home utils))
		
		(unless (zero? (system* #$gmi-bin "pull" "-C" #$work-mail-dir))
		  (begin (send-error-notification "Email Sync" "gmi pull failed")
			 (error "Lieer pull failed")))
		
		(unless (zero? (system* #$mbsync-bin "-a" "--pull"))
		  (begin
		    (send-error-notification "Email Sync" "mbsync pull failed")
		    (error "mbsync pull failed")))

		(unless (zero? (system* #$notmuch-bin "new"))
		  (begin
		    (send-error-notification "Email Sync" "notmuch new failed")
		    (error "Notmuch indexing failed")))

		(unless (zero? (system* #$afew-bin
					"--notmuch-config"
					#$notmuch-config-file
					"--tag"
					"--new"))
		  (begin
		    (send-error-notification "Email Sync" "afew tagging failed")
		    (error "Afew Tagging failed")))

		(unless (zero? (system* #$gmi-bin "push" "-C" #$work-mail-dir))
		  (begin
		    (send-error-notification "Email Sync" "gmi push failed")
		    (error "Lieer push failed")))

		(unless (zero? (system* #$mbsync-bin "-a" "--push"))
		  (begin
		    (send-error-notification "Email Sync" "mbsync push failed")
		    (error "mbsync push failed")))
		(send-notification #$dunstify "Email Sync" "sync completed"))))))
    (shepherd-timer '(mail-sync)
		    "*/2 * * * *"
		    #~(#$email-sync-script)
		    #:requirement '())))
