(define-module (afistfullofash packages themes)
  #:use-module (guix build-system copy)

  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  #:use-module (gnu packages compression)

  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (afistfullofash packages utils)
  
  #:export (alacritty-dracula-theme
	    dunst-dracula-theme
	    gtk-dracula-icons
	    lsd-dracula-theme
	    qt5-dracula-theme
	    starship-dracula-theme
	    xresources-dracula-theme
	    gtk-dracula-theme))

(define gtk-dracula-theme
  (package
    (name "gtk-dracula-theme")
    (version "4.0.0")
    (source (origin
	      (method url-fetch/xzbomb)
	      (uri (string-append
		    "https://github.com/dracula/gtk/releases/download/v"
		    version
		    "/Dracula.tar.xz"))
	      (sha256
	       (base32
		"0vqvj600qk6anjnqm1lqh171vag8qy38c0r5qsnxsgr43c2x96qr"))))
    (build-system copy-build-system)
    (native-inputs (list xz))
    (arguments
     '(#:install-plan '(("Dracula" "Dracula")
			("Dracula-slim-standard-buttons/" "Dracula-slim-standard-buttons/")
			("Dracula-alt-style/" "Dracula-standard-buttons/")
			("Dracula-slim/" "Dracula-slim/"))))
    (home-page "https://draculatheme.com/gtk")
    (synopsis "Dracula GTK Theme")
    (description "Dracula GTK Theme")
    (license license:gpl3)))

(define gtk-dracula-icons
  (package
    (name "gtk-dracula-icons")
    (version "0.0.0")
    (source (origin
	     (method url-fetch/zipbomb)
	     (uri "https://github.com/dracula/gtk/files/5214870/Dracula.zip")
	     (sha256
	      (base32
	       "1dnc1g1qw9r7glilw1gg11b4f6icfxckkjrj5rhmzzmlxwcjib9k"))))
    (build-system copy-build-system)
    (arguments '(#:install-plan '(("Dracula" "Dracula"))))
    (home-page "https://draculatheme.com/gtk")
    (synopsis "Dracula GTK Icons")
    (description "Dracula GTK Icons")
    (license license:gpl3)))

(define qt5-dracula-theme
  (let ((commit "7b25ee305365f6e62efb2c7aca3b4635622b778c")
	(version "0.0.0")
	(revision "0"))
    (package
      (name "qt5-dracula-theme")
      (version (git-version version revision commit))
      (source (origin
	       (method git-fetch)
	       (uri (git-reference
		      (url "https://github.com/dracula/qt5.git")
		      (commit commit)))
	       (file-name (git-file-name name version))
	       (sha256
		(base32
		 "00qlajbxj25w1bdhj8wc5r57g25gas6f1ax6wrzb4xcypw0j7xdm"))))
      (build-system copy-build-system)
      (arguments '(#:install-plan '(("Dracula.conf" "Dracula.conf"))))
      (home-page "https://draculatheme.com/qt5")
      (synopsis "Dracula QT5 Theme")
      (description "Dracula QT5 Theme")
      (license license:expat))))

(define alacritty-dracula-theme
  (let ((commit "c8a3a13404b78d520d04354e133b5075d9b785e1")
	(version "0.0.0")
	(revision "0"))
    (package
      (name "alacritty-dracula-theme")
      (version (git-version version revision commit))
      (source 
       (origin
	 (method git-fetch)
	 (uri (git-reference
		(url "https://github.com/dracula/alacritty.git")
		(commit commit)))
	 (file-name (git-file-name name version))
	 (sha256
	  (base32
	   "1pmk7m2bcwmnmhrbvnnm2znmyyh3vp42vvl1phvfbkz5yqz5jf2b"))))
      (build-system copy-build-system)
      (arguments '(#:install-plan '(("dracula.toml" "dracula.toml"))))
      (home-page "https://draculatheme.com/alacritty")
      (synopsis "Dracula Alacritty Theme")
      (description "Dracula Alacritty Theme")
      (license license:expat))))

(define starship-dracula-theme
  (let ((commit "920e9f46ccc25beee15ed7fe0baddabdfeaaf92a")
	(version "0.0.0")
	(revision "0"))
    (package
      (name "starship-dracula-theme")
      (version (git-version version revision commit))
      (source (origin
	       (method git-fetch)
	       (uri (git-reference
		      (url "https://github.com/dracula/starship.git")
		      (commit commit)))
	       (file-name (git-file-name name version))
	       (sha256
		(base32
		 "13i6alr7djb9h3vzav199i2kkxmzn004815z5cbc41lf7xvx2nc0"))))
      (build-system copy-build-system)
      (arguments '(#:install-plan '(("starship.theme.toml" "starship.theme.toml")
				    ("starship.toml" "starship.toml"))))
      (home-page "https://draculatheme.com/starship")
      (description "Dracula Theme for Starship")
      (synopsis "Dracula Theme for Starship")
      (license license:expat))))

(define lsd-dracula-theme
  (let ((commit "2b87711bdce8c89a882db720e4f47d95877f83a7")
	(version "0.0.0")
	(revision "0")) 
    (package
      (name "lsd-dracula-theme")
      (version (git-version version revision commit))
      (source (origin
	       (method git-fetch)
	       (uri (git-reference
		      (url "https://github.com/dracula/lsd.git")
		      (commit commit)))
	       (file-name (git-file-name name version))
	       (sha256
		(base32
		 "10id0n5c9jyrah295dv2zahl97851kp24d513k3pyxbsy9nv0qml"))))
      (build-system copy-build-system)
      (arguments '(#:install-plan '(("colors.yaml" "colors.yaml")
				    ("config.yaml" "config.yaml"))))
      (home-page "https://draculatheme.com/lsd")
      (description "Dracula theme for lsd")
      (synopsis "Dracula Theme for lsd")
      (license license:expat))))

(define xresources-dracula-theme
  (let ((commit "539ef24e9b0c5498a82d59bfa2bad9b618d832a3")
	(version "0.0.0")
	(revision "0" ))
    (package
      (name "xresources-dracula-theme")
      (version (git-version version revision commit))
      (source (origin
	       (uri (git-reference
		      (url "https://github.com/dracula/xresources.git")
		      (commit commit)))
	       (file-name (git-file-name name version))
	       (method git-fetch)
	       (sha256
		(base32 "1dkfa2q392vy7ky5kx0vd44xcb9c7x15z38x4acfma3f16q6vyg9"))))
      (build-system copy-build-system)
      (arguments '(#:install-plan '(("Xresources" "Xresources"))))
      (home-page "https://draculatheme.com/xresources")
      (description "Dracula theme for Xresources")
      (synopsis "Dracula theme for Xresources")
      (license license:expat))))

(define dunst-dracula-theme
  (let ((commit "907f345d81dba9566eff59dd89afb321118da180")
	(revision "0")
	(version "0.0.0"))
    (package
      (name "dunst-dracula-theme")
      (version (git-version version revision commit))
      (source (origin
		(uri (git-reference
		       (url "https://github.com/dracula/dunst.git")
		       (commit commit)))
		(file-name (git-file-name name version))
		(method git-fetch)
		(sha256
		 (base32
		  "0m8qzwlmacqk27l24iqyimyjgsz5ypmvs39hd5fl7if6b1vlcrwx"))))
      (build-system copy-build-system)
      (arguments '(#:install-plan '(("dunstrc" "dunstrc"))))
      (home-page "https://draculatheme.com/dunst")
      (synopsis "Dracula theme for dunst")
      (description "Dracula theme for dunst")
      (license license:expat))))
