(define-module (afistfullofash licenses)
  #:use-module (srfi srfi-9)
  #:use-module (guix licenses)
  #:export (busl-1.1))

(define license (@@ (guix licenses) license))

(define busl-1.1
  (license "Bussiness Source License"
	   "https://mariadb.com/bsl11/"
	   "The Business Source License is not an Open Source license. However, the Licensed Work will eventually be made available under an Open Source License, as stated in this License. This is a paramaterized license. The license parameters are: restrictions on usage, a change date, and the open source license that will govern usage of the software after the change date."))


