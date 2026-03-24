(define-module (afistfullofash packages python-web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)

  #:use-module (gnu packages check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  
  #:export (python-tornado-6.5))


(define python-tornado-6.5
  (package
    (name "python-tornado")
    (version "6.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tornado" version))
       (sha256
        (base32 "1s9hilgbj4gr0wprd6vpjqpvgiknxlb595861ksz3mqvm4z8yaqr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; AttributeError: 'TestIOStreamWebMixin' object has no attribute 'io_loop'
      #~(list "--ignore=tornado/test/iostream_test.py")))
    (native-inputs
     (list python-certifi
           python-pytest
           python-setuptools))
    (home-page "https://www.tornadoweb.org/")
    (synopsis "Python web framework and asynchronous networking library")
    (description
     "Tornado is a Python web framework and asynchronous networking library,
originally developed at FriendFeed.  By using non-blocking network I/O,
Tornado can scale to tens of thousands of open connections, making it ideal
for long polling, WebSockets, and other applications that require a long-lived
connection to each user.")
    (license license:asl2.0)))
