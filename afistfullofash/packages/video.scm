(define-module (afistfullofash packages video)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)

  #:use-module (afistfullofash packages python-web)

  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages video)
  
  #:export (motion-with-ffmpeg4
	    motioneye))

(define motion-with-ffmpeg4
  (package
    (inherit motion)
    (inputs
     (modify-inputs (package-inputs motion)
       (replace "ffmpeg" ffmpeg-4)))))

(define motioneye
  (package
    (name "motioneye")
    (version "0.43.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/motioneye-project/motioneye")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17ilv39zlnfiaasd35f2gz35xw349iqhnp7b6vijmrlgcdia0hvj"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-babel
                             python-boto3
                             python-jinja2
                             python-pillow
                             python-pycurl
                             python-tornado-6.5
			     motion-with-ffmpeg4))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/motioneye-project/motioneye")
    (synopsis "motioneye, a multilingual web interface for motion.")
    (description "motioneye, a multilingual web interface for motion.")
    (license license:gpl3)))
