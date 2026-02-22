(define-module (afistfullofash packages themes)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build utils)

  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages check)
  #:use-module (gnu packages web)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages image)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust-apps)

  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (afistfullofash packages utils)
  #:use-module (afistfullofash packages rust-apps)
  
  #:export (alacritty-catppuccin-theme
	    alacritty-dracula-theme
	    bat-catppuccin-theme
	    catppuccin-papirus-folders-icons
	    dunst-dracula-theme
	    dunst-catppuccin-theme
	    gtk-dracula-icons
	    gtk-dracula-theme
	    gtk-dracula-theme-4
	    gtk-dracula-theme-2026
	    dracula-cursors
	    gtk-catppuccin-theme
	    lsd-dracula-theme
	    qt5-dracula-theme
	    starship-catppuccin-theme
	    starship-dracula-theme
	    xresources-dracula-theme
	    xresources-catppuccin-theme))


(define alacritty-dracula-theme
  (let ((commit "c8a3a13404b78d520d04354e133b5075d9b785e1")
	(version "0.0.0")
	(revision "1"))
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
      (arguments '(#:install-plan '(("dracula.toml" "/share/themes/Dracula/alacritty/"))))
      (home-page "https://draculatheme.com/alacritty")
      (synopsis "Dracula Alacritty Theme")
      (description "Dracula Alacritty Theme")
      (license license:expat))))

(define alacritty-catppuccin-theme
  (let ((commit "f6cb5a5c2b404cdaceaff193b9c52317f62c62f7")
	(version "0.0.0")
	(revision "1"))
    (package
      (name "alacritty-catppuccin-theme")
      (version (git-version version revision commit))
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/catppuccin/alacritty.git")
		      (commit commit)))
		(file-name (git-file-name name version))
		(sha256
		 (base32
		  "1r2z223hza63v5lmzlg3022mlar67j3a2gh41rsaiqwja2wyiihz"))))
      (build-system copy-build-system)
      (arguments '(#:install-plan '(("catppuccin-frappe.toml" "/share/themes/catppuccin/alacritty/")
				    ("catppuccin-latte.toml" "/share/themes/catppuccin/alacritty/")
				    ("catppuccin-macchiato.toml" "/share/themes/catppuccin/alacritty/")
				    ("catppuccin-mocha.toml" "/share/themes/catppuccin/alacritty/"))))
      (home-page "https://github.com/catppuccin/alacritty")
      (description "Catppuccin Theme for Alacritty")
      (synopsis "Catppuccin Theme for Alacritty")
      (license license:expat))))



(define bat-catppuccin-theme
  (let ((commit "6810349b28055dce54076712fc05fc68da4b8ec0")
	(version "0.0.0")
	(revision "0"))
    (package
      (name "bat-catppuccin-theme")
      (version (git-version version revision commit))
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/catppuccin/bat.git")
		      (commit commit)))
		(file-name (git-file-name name version))
		(sha256
		 (base32
		  "1y5sfi7jfr97z1g6vm2mzbsw59j1jizwlmbadvmx842m0i5ak5ll"))))
      (build-system copy-build-system)
      (arguments '(#:install-plan '(("themes/" "/share/themes/catppuccin/bat/"))))
      (home-page "https://github.com/catppuccin/bat")
      (description "Catppuccin Theme for Bat")
      (synopsis "Catppuccin Theme for Bat")
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
      (arguments '(#:install-plan '(("dunstrc" "/share/themes/Dracula/dunst/dunstrc"))))
      (home-page "https://draculatheme.com/dunst")
      (synopsis "Dracula theme for dunst")
      (description "Dracula theme for dunst")
      (license license:expat))))

(define dunst-catppuccin-theme
  (let ((commit "5955cf0213d14a3494ec63580a81818b6f7caa66")
	(revision "2")
	(version "0.0.0"))
    (package
      (name "dunst-catppuccin-theme")
      (version (git-version version revision commit))
      (source (origin
		(uri (git-reference
		      (url "https://github.com/catppuccin/dunst.git")
		      (commit commit)))
		(file-name (git-file-name name version))
		(method git-fetch)
		(sha256
		 (base32
		  "1rpxrnhphcxm93s2wc7wbd9cxjmv79r2m6ip0a6rj7lh9v0ps6mc"))))
      (build-system copy-build-system)
      (arguments '(#:install-plan '(("themes/" "/share/themes/catppuccin/dunst/"))))
      (home-page "https://github.com/catppuccin/dunst")
      (synopsis " Catppuccin themes for dunst")
      (description "Catppuccin themes for dunst")
      (license license:expat))))

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
    (arguments '(#:install-plan '(("Dracula" "share/icons/Dracula"))))
    (home-page "https://draculatheme.com/gtk")
    (synopsis "Dracula GTK Icons")
    (description "Dracula GTK Icons")
    (license license:gpl3)))

(define papirus-folders
  (package
    (name "papirus-folders")
    (version "1.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/PapirusDevelopmentTeam/papirus-folders.git")
	     (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "1c7l3vddn0gf8d0dfpickk0k756p7nml5mrvwnjsqdhx9s2dhk56"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("papirus-folders" "bin/")
         ("completion/papirus-folders" "share/bash-completion/completions/")
	 ("completion/_papirus-folders" "share/zsh/site-functions/")
	 ("completion/_papirus-folders" "share/zsh/vendor-completions/"))))
    (home-page "https://github.com/PapirusDevelopmentTeam/papirus-folders")
    (synopsis "Papirus Icons papirus-folders script")
    (description "papirus-folders is a handy bash script that lets you change the colors of folders in the Papirus icon theme and similar themes based on version 20171007 or newer. It's an easy way to customize how your folders look.")
    (license license:expat)))

(define catppuccin-papirus-folders-icons
  (let ((commit "f83671d17ea67e335b34f8028a7e6d78bca735d7")
	(version "0.0.0")
	(revision "0"))
    (package
      (name "catppuccin-papirus-folders-icons")
      (version (git-version version revision commit))
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/catppuccin/papirus-folders.git")
		      (commit commit)))
		(file-name (git-file-name name version))
		(sha256
		 (base32
		  "07nr8x9r4ndwywqd92a5gpcb2mid6map24s6wjz18chs7g1ms9hn"))))
      (build-system gnu-build-system)
      (inputs (list papirus-folders))
      (propagated-inputs (list papirus-icon-theme))
      (arguments
       (list
	#:tests? #f
	#:phases
	#~(modify-phases %standard-phases
	    (delete 'configure)
            (delete 'check) 
	    (replace 'build
              (lambda* (#:key inputs #:allow-other-keys)
		(use-modules (guix build utils))
		(let* ((papirus-icons (string-append
				      (assoc-ref inputs "papirus-icon-theme")
				      "/share/icons/"))
		      
		      (papirus-folders (string-append
					(assoc-ref inputs "papirus-folders")
					"/bin/papirus-folders"))

		      (papirus-varients (list "Papirus"
					      "Papirus-Dark"
					      "Papirus-Light"))

		      (catppuccin-themes (list "latte"
						"frappe"
						"macchiato"
						"mocha"))

		      (catppuccin-colors (list "rosewater"
					       "maroon"
					       "pink"
					       "teal"
					       "peach"
					       "sapphire"
					       "red"
					       "flamingo"
					       "green"
					       "mauve"
					       "sky"
					       "yellow"
					       "blue"
					       "lavender"))

		      (build-root (getcwd))
		      (build-dir (string-append build-root "/.icons/"))
		      (dist-dir (string-append build-root "/dist/"))
		      (catppuccin-source-dir (string-append build-root "/src")))
		  ;; Papirus Folders uses this to work out where
		  ;; The theme directory is
		  (setenv "USER_HOME" build-root)
		  
		  (mkdir dist-dir)
                  
		  (map
		   (lambda (papirus-theme-name)
		     (map
		      (lambda (catppuccin-theme-name)
                        (map
			 (lambda (catppuccin-color-name)
                           (let ((papirus-build-dir
				  (string-append build-dir
						 papirus-theme-name))
				 (catppuccin-icon-theme-name
				  (string-append "cat-"
						 catppuccin-theme-name
						 "-"
						 catppuccin-color-name)))
			     (mkdir-p build-dir)
                             ;; Merge the theme directories
			     ;; Do this clean every run
			     (copy-recursively papirus-icons build-dir)
			     (copy-recursively catppuccin-source-dir papirus-build-dir)
                             
			     (invoke papirus-folders
				     "-C"
				     catppuccin-icon-theme-name
				     "--theme"
				     papirus-theme-name)
                             
			     ;; Copy the theme to a built dir
			     (copy-recursively papirus-build-dir
					       (string-append
						dist-dir
						catppuccin-icon-theme-name
						"-"
						papirus-theme-name))
			     ;; Clean Up after ourselves
			     (delete-file-recursively build-dir)))
			 catppuccin-colors))
		      catppuccin-themes))
		   papirus-varients))))
	    (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
		(let ((out (string-append #$output "/share/icons")))
                  (mkdir-p out)
                  (copy-recursively (string-append (getcwd) "/dist/")
				    out)))))))
      (home-page "https://github.com/catppuccin/papirus-folders")
      (synopsis "Catppuccin for Papirus Folders Icons")
      (description " Catppuccin for Papirus Folders Icons.

Please note that this also brings in the papirus-icons-theme package with it as a propogated input")
      (license license:expat))))

(define gtk-dracula-theme-4
  (package
    (name "gtk-dracula-theme")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/dracula/gtk.git")
	     (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0p57lpv9023byqy31j5lcv7z0g720as34g11vpffagfdc178dzvq"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("." "share/themes/Dracula"))))
    (home-page "https://draculatheme.com/gtk")
    (synopsis "Dracula GTK Theme")
    (description "Dracula GTK Theme")
    (license license:gpl3)))

(define gtk-dracula-theme gtk-dracula-theme-4)

(define gtk-dracula-theme-2026
  (let ((commit "d4163a5e6e598053567038c53777bf7682ecd17f")
	(version "4.0.0")
	(revision "2026"))
    (package
      (inherit gtk-dracula-theme-4)
      (version (git-version version revision commit))
      (source
       (origin
	 (method git-fetch)
	 (uri (git-reference
	       (url "https://github.com/dracula/gtk.git")
	       (commit commit)))
	 (file-name (git-file-name "gtk-dracula-theme" commit))

	 (sha256
	  (base32
	   "05j58zhbw5m54pw5wxqbs4r1cg88zvc8fian3v15q2fk7gq4ia8m")))))))

(define dracula-cursors
  (package
    (inherit gtk-dracula-theme-2026)
    (name "dracula-cursors")
    (arguments
     '(#:install-plan '(("kde/cursors/Dracula-cursors" "share/icons/"))))))

(define-public python-catppuccin
  (package
    (name "python-catppuccin")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "catppuccin" version))
       (sha256
        (base32 "0d38gjn6661pb9jl4bg6l698aw4llir44xacrg8nj8xw6nzz6d9h"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hatchling
			 python-rich
			 python-matplotlib
			 python-pygments
			 python-pytest))
    
    (home-page #f)
    (synopsis "ð Soothing pastel theme for Python.")
    (description "ð Soothing pastel theme for Python.")
    (license license:expat)))

(define gtk-colloid-theme-src
  (let ((name "gtk-colloid-theme-src")
	(commit "1a13048ea1bd4a6cb9b293b537afd16bf267e773"))
    (origin
      (method git-fetch)
      (uri (git-reference
	    (url "https://github.com/vinceliuice/Colloid-gtk-theme.git")
	    (commit commit)))
      (file-name (git-file-name name commit))
      (sha256
       (base32
	"1ixdqrwvxxjl6caqfcq6fxhlzgmqwbwybqjnhvd81vs84cw2i0fd")))))

(define gtk-catppuccin-theme
  (package
    (name "gtk-catppuccin-theme")
    (version "1.0.3")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/catppuccin/gtk.git")
		    (commit (string-append   "v" version))))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"0vwa2paicvyb3ii3j97cg47n6rs75jsjx7g159a3wmwmkasrkb7h"))))
    (build-system pyproject-build-system)
    (inputs (list python-catppuccin
		  sassc
		  inkscape
		  optipng
		  gtk-colloid-theme-src))
    (native-inputs (list util-linux
			 git-minimal))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
	  (add-after 'unpack 'replace-colloid-submodule
            (lambda* (#:key outputs #:allow-other-keys)
	      (let ((colloid-dir "./sources/colloid"))
		(delete-file-recursively colloid-dir)
		(copy-recursively #$gtk-colloid-theme-src colloid-dir)
		(for-each make-file-writable (find-files colloid-dir)))))
	  (replace 'build
            (lambda* (#:key inputs outputs #:allow-other-keys)
	      (let ((out "./dist")
		    (python (string-append (assoc-ref inputs "python")
					   "/bin/python")))
		(mkdir-p out)
		(invoke python "sources/patches/xfwm4/generate_assets.py")

		
		;; Build themes sequentially for reliability in the build container
		(for-each (lambda (flavor)
			    (format #t "Building catppuccin flavor: ~a...~%" flavor)
			    (unless (zero? (system* python "./build.py" flavor 
						    "--all-accents" "-d" out))
			      (error (format #f "Failed to build catppuccin flavor ~a" flavour))))
			  '("mocha" "macchiato" "frappe" "latte")))))
	  (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (use-modules (guix build utils))
	      (let* ((out (assoc-ref outputs "out"))
		     (theme-out-dir (string-append out "/share/themes/")))
                (mkdir-p out)
                (copy-recursively "dist" out))))
	  (delete 'add-install-to-pythonpath)
	  (delete 'add-install-to-path)
	  (delete 'create-entrypoints)
	  (delete 'wrap)
	  (delete 'sanity-check)
	  (delete 'compile-bytecode)
	  (delete 'patch-shebangs)
	  (delete 'rename-pth-file)
	  (delete 'validate-runpath)
	  (delete 'validate-documentation-location)
	  (delete 'delete-info-dir-file)
	  (delete 'patch-dot-desktop-files)
	  (delete 'make-dynamic-linker-cache)
	  (delete 'install-license-files)
	  (delete 'reset-gzip-timestamps)
	  (delete 'compress-documentation))))
    (home-page "https://github.com/catppuccin/gtk/")
    (synopsis "Catppuccin GTK Theme")
    (description "Catppuccin GTK Theme")
    (license license:gpl3)))

(define lsd-dracula-theme
  (let ((commit "2b87711bdce8c89a882db720e4f47d95877f83a7")
	(version "0.0.0")
	(revision "1")) 
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
      (arguments '(#:install-plan '(("colors.yaml" "/share/themes/Dracula/lsd/")
				    ("config.yaml" "/share/themes/Dracula/lsd/"))))
      (home-page "https://draculatheme.com/lsd")
      (description "Dracula theme for lsd")
      (synopsis "Dracula Theme for lsd")
      (license license:expat))))

(define qt5-dracula-theme
  (let ((commit "7b25ee305365f6e62efb2c7aca3b4635622b778c")
	(version "0.0.0")
	(revision "1"))
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
      (arguments '(#:install-plan '(("Dracula.conf" "share/color-schemes/"))))
      (home-page "https://draculatheme.com/qt5")
      (synopsis "Dracula QT5 Theme")
      (description "Dracula QT5 Theme")
      (license license:expat))))

(define starship-dracula-theme
  (let ((commit "920e9f46ccc25beee15ed7fe0baddabdfeaaf92a")
	(version "0.0.0")
	(revision "1"))
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
      (arguments '(#:install-plan '(("starship.theme.toml" "/share/themes/Dracula/starship/")
				    ("starship.toml" "/share/themes/Dracula/starship/"))))
      (home-page "https://draculatheme.com/starship")
      (description "Dracula Theme for Starship")
      (synopsis "Dracula Theme for Starship")
      (license license:expat))))

(define starship-catppuccin-theme
  (let ((commit "5906cc369dd8207e063c0e6e2d27bd0c0b567cb8")
	(version "0.0.0")
	(revision "2"))
    (package
      (name "starship-catppuccin-theme")
      (version (git-version version revision commit))
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/catppuccin/starship.git")
		      (commit commit)))
		(file-name (git-file-name name version))
		(sha256
		 (base32
		  "0j3bc9caf6ayg7m8s0hshypgqiiy8bm9kakxwa5ackk955nf7c8l"))))
      (build-system copy-build-system)
      (arguments '(#:install-plan '(("starship.toml" "share/themes/catppuccin/starship/")
				    ("themes/" "share/themes/catppuccin/starship/"))))
      (home-page "https://github.com/catppuccin/starship")
      (description "Catppuccin Themes for Starship")
      (synopsis "Catppuccin Themes for Starship")
      (license license:expat))))

(define xresources-dracula-theme
  (let ((commit "539ef24e9b0c5498a82d59bfa2bad9b618d832a3")
	(version "0.0.0")
	(revision "2"))
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
      (arguments '(#:install-plan '(("Xresources" "/share/themes/Dracula/xresources/"))))
      (home-page "https://draculatheme.com/xresources")
      (description "Dracula theme for Xresources")
      (synopsis "Dracula theme for Xresources")
      (license license:expat))))

(define xresources-catppuccin-theme
  (let ((commit "41afcd788311ea2fce124029d9a02e2d65e0b3e6")
	(version "0.0.0")
	(revision "0"))
    (package
      (name "xresources-catppuccin-theme")
      (version (git-version version revision commit))
      (source (origin
		(uri (git-reference
		      (url "https://github.com/catppuccin/xresources.git")
		      (commit commit)))
		(file-name (git-file-name name version))
		(method git-fetch)
		(sha256
		 (base32 "0mn88af5y8z4riasg045q1xqfblcqv0f56j9fmy8c6f3k1nmzv7m"))))
      (build-system gnu-build-system)
      (native-inputs (list whiskers
			   just
			   bash-minimal))
      (arguments
       (list
	#:tests? #f ; There are no 'make check' tests
	#:phases
	#~(modify-phases %standard-phases
            ;; 1. Remove phases that require a Makefile/Configure script
            (delete 'configure)
            (delete 'check) 
            ;; 2. Replace 'build' with our just command
            (replace 'build
              (lambda* (#:key inputs #:allow-other-keys)
		(invoke "just" "build")))
            ;; 3. Replace 'install' to move the generated file
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
		(let ((dest (string-append #$output "/share/themes/catppuccin/Xresources")))
                  (mkdir-p dest)
                  (copy-recursively (string-append "themes/")
				    dest)))))))
      (home-page "https://github.com/catppuccin/xresources")
      (description "Cattapuccin theme for Xresources")
      (synopsis "Cattapuccin theme for Xresources")
      (license license:expat))))
