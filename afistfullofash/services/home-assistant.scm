(define-module (afistfullofash services home-assistant)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)

  #:use-module (gnu packages guile-xyz)

  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
        
  #:use-module (afistfullofash services utils)

  #:export (home-assistant-service-type
            home-assistant-configuration

	    ha-configuration
	    ha-configuration?

	    ha-frontend
	    ha-frontend?

	    ha-automations
	    ha-automations?
	    
	    ha-scripts
	    ha-scripts?

	    ha-scenes
	    ha-scenes?
	    
            ;; ;; Records for your specific hardware
            ;; ha-universal-tv
            ;; ha-universal-tv?
            
            ;; ;; Sensors and Templates from your config
            ;; ha-sensor
            ;; ha-template-sensor
	    ))

(define-record-type* <home-assistant-configuration>
  home-assistant-configuration make-home-assistant-configuration
  home-assistant-configuration?
  (package       ha-configuration-package (default #f))
  (config-dir    ha-configuration-dir (default "/var/lib/homeassistant"))
  (configuration ha-configuration-data (default (ha-configuration))))

(define-record-type* <ha-configuration>
  ha-configuration make-ha-configuration
  ha-configuration?
  
  (frontend ha-frontend-data (default (ha-frontend)))
  (automation ha-automation-data (default (ha-automations)))
  (script ha-script-data (default (ha-scripts)))
  (scene ha-scene-data (default (ha-scenes)))

  (media-players ha-configuration-media-players (default '()))
  (sensors       ha-configuration-sensors (default '())))

(define-record-type* <ha-path>
  ha-path make-ha-path
  ha-path?
  (file ha-path-file (default #f))
  (directory ha-path-directory (default #f)))

(define (path->yaml path)
  (cond ((ha-path-directory path)
	 (format #f "!include_dir_merged_named ~a~%"
		 (ha-path-directory path)))
	((ha-path-file path)
	 (format #f "!include ~a~%"
		 (ha-path-file path)))
	(else (error (format #f "~a is not a path" path)))))

(define-record-type* <ha-frontend>
  ha-frontend make-ha-frontend
  ha-frontend?
  (themes ha-theme-data (default (ha-path (directory "themes")))))

(define-record-type* <ha-theme>
  ha-theme make-ha-theme
  ha-theme?
  (inline ha-theme-inline (default #f)))

(define (frontend->yaml frontends)
  (let* ((theme (ha-theme-data frontends))
	 (theme-string (cond ((ha-path? theme) (path->yaml theme))
			     ((ha-theme? theme) (concatenate (ha-theme-inline theme)))
			     (else (error (format #f "~a is not a valid theme"))))))
    #~'("frontend" . ("themes" . #$theme-string))))

(define-record-type* <ha-automation>
  ha-automations make-ha-automations
  ha-automations?
  (automations ha-automations-data (default (ha-path (file "automations.yaml")))))


(define (automation->yaml automations)
  (let* ((automation (ha-automations-data automations))
	  (automation-string (cond ((ha-path? automation)
				    (path->yaml automation))
				   ((ha-automations? automation)
				    (concatenate automation))
				   (else
				    (error
				     (format #f "~a is not a valid automation"
					     automation))))))
    #~'("automation" . #$automation-string)))
 
(define-record-type* <ha-scripts>
  ha-scripts make-ha-scripts
  ha-scripts?
  (scripts ha-scripts-data (default (ha-path (file "scripts.yaml")))))

(define (script->yaml scripts)
  (let* ((script (ha-scripts-data scripts))
	  (script-string (cond ((ha-path? script)
				    (path->yaml script))
				   ((ha-scripts? script)
				    (concatenate script))
				   (else
				    (error
				     (format #f "~a is not a valid script"
					     script))))))
    #~'("scene" . #$script-string)))

(define-record-type* <ha-scenes>
  ha-scenes make-ha-scenes
  ha-scenes?
  (scenes ha-scenes-data (default (ha-path (file "scenes.yaml")))))


(define (scene->yaml scenes)
  (let* ((scene (ha-scenes-data scenes))
	(scene-string (cond ((ha-path? scene)
			     (path->yaml scene))
			    ((ha-scenes? scene)
			     (concatenate scene))
			    (else
			     (error
			      (format #f "~a is not a valid scene"
				      scene))))))
    #~'("scene" . #$scene-string)))

(define-record-type* <ha-media-player>
  ha-media-player make-ha-media-player
  ha-media-player?
  (name     ha-media-player-name)
  (children ha-media-player-children))

(define-record-type* <ha-media-player>
  ha-media-player make-ha-media-player
  ha-media-player?
  (name     ha-media-player-name)
  (children ha-media-player-children))



;; This procedure generates the YAML fragment for the media players
(define (tv->yaml tv)
  #~(format #f "
  - platform: universal
    name: ~a
    children:
      - ~a
      - ~a" #$(ha-universal-tv-name tv) #$(ha-universal-tv-remote tv) #$(ha-universal-tv-cast tv)))

(define (serialize-ha-configuration config)
  (match-record config <ha-configuration>
		(frontend automation script scene media-players sensors)
		`(
		  ("default_config" . #f)
		  ,(frontend->yaml frontend)
		  ,(automation->yaml automation)
		  ,(script->yaml script)
		  ,(scene->yaml scene))))

(define home-assistant-service-type
  (service-type
   (name 'home-assistant)
   (description "Manages Home Assistant Configuration. This service does not provide a Home Assistant Package")
   (extensions
    (list
     (service-extension
      activation-service-type
      (lambda (config)
        #~(begin
            (use-modules (guix build utils))
            (mkdir-p #$(ha-configuration-dir config)))))
     (service-extension
	   etc-service-type
	   (lambda (config)
	     (list `("homeassistant/configuration.yaml"
		     ,(computed-file
		       "configuration.yaml"
		       (with-imported-modules '((afistfullofash services utils))
			 #~(begin
			     (use-modules (afistfullofash services utils))
			     (call-with-output-file #$output
					; Use the module
			       (lambda (port)
				 (display (scm->yaml #$(serialize-ha-configuration
							(ha-configuration-data config))) port))))))))))
))
   (compose concatenate)
   (extend (lambda (config extensions)
	     (let ((inner-data (ha-configuration-data config)))
               (home-assistant-configuration
		(inherit config)
		(configuration
		 (ha-configuration
		  (inherit inner-data)
		  (frontend (ha-frontend-data inner-data))
		  (automation (ha-automation-data inner-data))
		  (script (ha-script-data inner-data))
		  (scene (ha-scene-data inner-data))))))))))


