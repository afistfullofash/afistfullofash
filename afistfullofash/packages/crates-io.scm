(define-module (afistfullofash packages crates-io)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-shell)
  #:use-module (gnu packages crates-io)
  
  )

(define-public rust-wast-35
  (package
    (name "rust-wast")
    (version "35.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wast" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s2d43g326dw21bygpalzjnr1fi83lx4afimg1h5hilrnkql1w9f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-leb128" ,rust-leb128-0.2))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wast")
    (synopsis
     "Customizable Rust parsers for the WebAssembly Text formats WAT and WAST")
    (description
     "This package provides Customizable Rust parsers for the @code{WebAssembly} Text formats WAT and WAST.")
    (license (list license:asl2.0))))

(define-public rust-witx-0.9
  (package
    (name "rust-witx")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "witx" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jzgmayh2jjbv70jzfka38g4bk4g1fj9d0m70qkxpkdbbixg4rp3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wast" ,rust-wast-35))))
    (home-page "https://github.com/WebAssembly/WASI")
    (synopsis "Parse and validate witx file format")
    (description "This package provides Parse and validate witx file format.")
    (license license:asl2.0)))

(define-public rust-wiggle-generate-29
  (package
    (name "rust-wiggle-generate")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wiggle-generate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00g12vfj0rfvzbq3c5cxgm057gg6mfn58jwg2g414fb70pfngwic"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-heck" ,rust-heck-0.5)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-shellexpand" ,rust-shellexpand-2)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-witx" ,rust-witx-0.9))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Library crate for wiggle code generator")
    (description
     "This package provides Library crate for wiggle code generator.")
    (license (list license:asl2.0))))

(define-public rust-wiggle-macro-29
  (package
    (name "rust-wiggle-macro")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wiggle-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0315xqcj3a4rw1d81848mc61237z2gvhjf3z6wnnr3hrsirw9i88"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-wiggle-generate" ,rust-wiggle-generate-29))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Wiggle code generator")
    (description "This package provides Wiggle code generator.")
    (license (list license:asl2.0))))

(define-public rust-wiggle-29
  (package
    (name "rust-wiggle")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wiggle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07bmwswj7y8wpa01j95l514icnbrl2lj0ls6c7155732r5dz76jb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-wasmtime" ,rust-wasmtime-29)
                       ("rust-wiggle-macro" ,rust-wiggle-macro-29)
                       ("rust-witx" ,rust-witx-0.9))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Runtime components of wiggle code generator")
    (description
     "This package provides Runtime components of wiggle code generator.")
    (license (list license:asl2.0))))

(define-public rust-libssh2-sys-0.3
  (package
    (name "rust-libssh2-sys")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libssh2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f8i31h3666rl6sq7v64ajdq03hmylkh6c1vaf9828aaml2ly3i2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libz-sys" ,rust-libz-sys-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "https://github.com/alexcrichton/ssh2-rs")
    (synopsis "Native bindings to the libssh2 library")
    (description
     "This package provides Native bindings to the libssh2 library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ssh2-0.9
  (package
    (name "rust-ssh2")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ssh2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j38p804b8sbgnfw1x8j2mkvh6yva7li36b2la8lw3ca7cxx311g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libssh2-sys" ,rust-libssh2-sys-0.3)
                       ("rust-parking-lot" ,rust-parking-lot-0.12))))
    (home-page "https://github.com/alexcrichton/ssh2-rs")
    (synopsis
     "Bindings to libssh2 for interacting with SSH servers and executing remote
commands, forwarding local ports, etc.")
    (description
     "This package provides Bindings to libssh2 for interacting with SSH servers and executing remote
commands, forwarding local ports, etc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-char-device-0.16
  (package
    (name "rust-char-device")
    (version "0.16.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "char-device" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qsmmh886sjxl8izdd1fm4y08yflq0mxvj030sfx70n1bmhixksm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-io-extras" ,rust-io-extras-0.18)
                       ("rust-io-lifetimes" ,rust-io-lifetimes-2)
                       ("rust-rustix" ,rust-rustix-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-winx" ,rust-winx-0.36))))
    (home-page "https://github.com/sunfishcode/char-device")
    (synopsis "Character Device I/O")
    (description "This package provides Character Device I/O.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-system-interface-0.27
  (package
    (name "rust-system-interface")
    (version "0.27.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "system-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ic7qxkgxh8hbphcawcz2xdnb5lmlirkhj4158f5466ffkv94ifc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cap-async-std" ,rust-cap-async-std-3)
                       ("rust-cap-fs-ext" ,rust-cap-fs-ext-3)
                       ("rust-cap-std" ,rust-cap-std-3)
                       ("rust-char-device" ,rust-char-device-0.16)
                       ("rust-fd-lock" ,rust-fd-lock-4)
                       ("rust-io-lifetimes" ,rust-io-lifetimes-2)
                       ("rust-os-pipe" ,rust-os-pipe-1)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-socketpair" ,rust-socketpair-0.19)
                       ("rust-ssh2" ,rust-ssh2-0.9)
                       ("rust-windows-sys" ,rust-windows-sys-0.59)
                       ("rust-winx" ,rust-winx-0.36))))
    (home-page "https://github.com/bytecodealliance/system-interface")
    (synopsis "Extensions to the Rust standard library")
    (description
     "This package provides Extensions to the Rust standard library.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-cap-time-ext-3
  (package
    (name "rust-cap-time-ext")
    (version "3.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cap-time-ext" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nqskcskhki1chj3ylln333nh0xrbp3phsa6l3d8a03pp0hga6j9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ambient-authority" ,rust-ambient-authority-0.0.2)
                       ("rust-cap-primitives" ,rust-cap-primitives-3)
                       ("rust-cap-std" ,rust-cap-std-3)
                       ("rust-iana-time-zone" ,rust-iana-time-zone-0.1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustix" ,rust-rustix-1)
                       ("rust-winx" ,rust-winx-0.36))))
    (home-page "https://github.com/bytecodealliance/cap-std")
    (synopsis "Extension traits for `SystemClock` and `MonotonicClock`")
    (description
     "This package provides Extension traits for `@code{SystemClock`} and `@code{MonotonicClock`}.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-cap-rand-3
  (package
    (name "rust-cap-rand")
    (version "3.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cap-rand" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h0vjjmdwpf0139y1silf84frkps1mix1280y21qd8lqyz68kjqa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ambient-authority" ,rust-ambient-authority-0.0.2)
                       ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/bytecodealliance/cap-std")
    (synopsis "Capability-based random number generators")
    (description
     "This package provides Capability-based random number generators.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-cap-net-ext-3
  (package
    (name "rust-cap-net-ext")
    (version "3.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cap-net-ext" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b3h14c31ibg65a21001jfh6w8gcgj4aq8ivj5p9hv662qw870wz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cap-primitives" ,rust-cap-primitives-3)
                       ("rust-cap-std" ,rust-cap-std-3)
                       ("rust-rustix" ,rust-rustix-1)
                       ("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://github.com/bytecodealliance/cap-std")
    (synopsis "Extension traits for `TcpListener`, `Pool`, etc")
    (description
     "This package provides Extension traits for `@code{TcpListener`}, `Pool`, etc.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-cap-async-std-3
  (package
    (name "rust-cap-async-std")
    (version "3.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cap-async-std" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08dwkndq25ci70ksvj1qd7ng8985xa86h23x59n39mqq4ry7wcnp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arf-strings" ,rust-arf-strings-0.7)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-camino" ,rust-camino-1)
                       ("rust-cap-primitives" ,rust-cap-primitives-3)
                       ("rust-io-extras" ,rust-io-extras-0.18)
                       ("rust-io-lifetimes" ,rust-io-lifetimes-2)
                       ("rust-rustix" ,rust-rustix-1))))
    (home-page "https://github.com/bytecodealliance/cap-std")
    (synopsis "Capability-based version of async-std")
    (description
     "This package provides Capability-based version of async-std.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-cap-fs-ext-3
  (package
    (name "rust-cap-fs-ext")
    (version "3.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cap-fs-ext" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g0pdhz3w09pdza8xamzrzicjpmwk73y7h8mdzxfhgqra62w2774"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arf-strings" ,rust-arf-strings-0.7)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-camino" ,rust-camino-1)
                       ("rust-cap-async-std" ,rust-cap-async-std-3)
                       ("rust-cap-primitives" ,rust-cap-primitives-3)
                       ("rust-cap-std" ,rust-cap-std-3)
                       ("rust-io-lifetimes" ,rust-io-lifetimes-2)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/bytecodealliance/cap-std")
    (synopsis "Extension traits for `Dir`, `File`, etc")
    (description
     "This package provides Extension traits for `Dir`, `File`, etc.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-wasmtime-wasi-29
  (package
    (name "rust-wasmtime-wasi")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-wasi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "190cgq4k8r6nwxhv1f91isf2p6dbjnb1yyma9p3xl6xbzjdyc6wd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cap-fs-ext" ,rust-cap-fs-ext-3)
                       ("rust-cap-net-ext" ,rust-cap-net-ext-3)
                       ("rust-cap-rand" ,rust-cap-rand-3)
                       ("rust-cap-std" ,rust-cap-std-3)
                       ("rust-cap-time-ext" ,rust-cap-time-ext-3)
                       ("rust-fs-set-times" ,rust-fs-set-times-0.20)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-io-extras" ,rust-io-extras-0.18)
                       ("rust-io-lifetimes" ,rust-io-lifetimes-2)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-system-interface" ,rust-system-interface-0.27)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-trait-variant" ,rust-trait-variant-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-wasmtime" ,rust-wasmtime-29)
                       ("rust-wiggle" ,rust-wiggle-29)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "WASI implementation in Rust")
    (description "This package provides WASI implementation in Rust.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-c-api-macros-29
  (package
    (name "rust-wasmtime-c-api-macros")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-c-api-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nxndnwfwx490yw5rwc1gn2xvd66c5z3sil29lw5sk8jw7mpjah2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Support macros for `wasmtime-c-api`")
    (description "This package provides Support macros for `wasmtime-c-api`.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-wmemcheck-29
  (package
    (name "rust-wasmtime-wmemcheck")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-wmemcheck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xr8c79cvb4hkwpy9pj8vfwmv8y1nb9qfxnqx2ahcg7fbrn8z617"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Memcheck implementation for Wasmtime")
    (description "This package provides Memcheck implementation for Wasmtime.")
    (license (list license:asl2.0))))

(define-public rust-winch-codegen-29
  (package
    (name "rust-winch-codegen")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winch-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1745a92ni167lczz20rc5vjld613m9x4id7l1aib0v7lqpr9x11g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cranelift-codegen" ,rust-cranelift-codegen-0.116)
                       ("rust-gimli" ,rust-gimli-0.31)
                       ("rust-regalloc2" ,rust-regalloc2-0.11)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.13)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasmparser" ,rust-wasmparser-0.221)
                       ("rust-wasmtime-cranelift" ,rust-wasmtime-cranelift-29)
                       ("rust-wasmtime-environ" ,rust-wasmtime-environ-29))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Winch code generation library")
    (description "This package provides Winch code generation library.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-winch-29
  (package
    (name "rust-wasmtime-winch")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-winch" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13yg8dx7nf37fp6syyj8i2cyaak8sgmbk4hhv3hxa0h5yawbzfpx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cranelift-codegen" ,rust-cranelift-codegen-0.116)
                       ("rust-gimli" ,rust-gimli-0.31)
                       ("rust-object" ,rust-object-0.36)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.13)
                       ("rust-wasmparser" ,rust-wasmparser-0.221)
                       ("rust-wasmtime-cranelift" ,rust-wasmtime-cranelift-29)
                       ("rust-wasmtime-environ" ,rust-wasmtime-environ-29)
                       ("rust-winch-codegen" ,rust-winch-codegen-29))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Integration between Wasmtime and Winch")
    (description
     "This package provides Integration between Wasmtime and Winch.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-slab-29
  (package
    (name "rust-wasmtime-slab")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-slab" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kswbqx53kcnsgd78nnvafh2jxydsnrqn5bvphaaq17sjqd85dgw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Uni-typed slab with a free list for use in Wasmtime")
    (description
     "This package provides Uni-typed slab with a free list for use in Wasmtime.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-jit-icache-coherence-29
  (package
    (name "rust-wasmtime-jit-icache-coherence")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-jit-icache-coherence" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1la7aw71kavh11d9k9nig7nbv2pd9xq72c99wp1fd4hnw198appc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Utilities for JIT icache maintenance")
    (description "This package provides Utilities for JIT icache maintenance.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-jit-debug-29
  (package
    (name "rust-wasmtime-jit-debug")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-jit-debug" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0spmc3laaava7a0bx8qryjnfq2kcqc3yfbf2r0shrq2yi9462yry"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-object" ,rust-object-0.36)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-wasmtime-versioned-export-macros" ,rust-wasmtime-versioned-export-macros-29))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "JIT debug interfaces support for Wasmtime")
    (description
     "This package provides JIT debug interfaces support for Wasmtime.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-fiber-29
  (package
    (name "rust-wasmtime-fiber")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-fiber" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05qis8ry2z3mqkw2hlpz3yz1g9ih0xjqa10rj751n24z27a91fnc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-wasmtime-asm-macros" ,rust-wasmtime-asm-macros-29)
                       ("rust-wasmtime-versioned-export-macros" ,rust-wasmtime-versioned-export-macros-29)
                       ("rust-wasmtime-versioned-export-macros" ,rust-wasmtime-versioned-export-macros-29)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Fiber support for Wasmtime")
    (description "This package provides Fiber support for Wasmtime.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-versioned-export-macros-29
  (package
    (name "rust-wasmtime-versioned-export-macros")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-versioned-export-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nvvm5xh6yclnf7511cnvzfi79l710lwhh6yc8229h3d47dqdzw6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Macros for defining versioned exports in Wasmtime")
    (description
     "This package provides Macros for defining versioned exports in Wasmtime.")
    (license (list license:asl2.0))))

(define-public rust-wasmprinter-0.221
  (package
    (name "rust-wasmprinter")
    (version "0.221.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmprinter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10xjs2bzvppwr4qdsgfqqmafjah9290bd0gz35w6r4pjjwmc8hvk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-termcolor" ,rust-termcolor-1)
                       ("rust-wasmparser" ,rust-wasmparser-0.221))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasmprinter")
    (synopsis
     "Rust converter from the WebAssembly binary format to the text format.")
    (description
     "This package provides Rust converter from the @code{WebAssembly} binary format to the text format.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-xdg-3
  (package
    (name "rust-xdg")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xdg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dc5jpkkylp7z54c4xwxzwxx1jb5cklwfjs5493k9y9d7wik7d1g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/whitequark/rust-xdg")
    (synopsis
     "library for storing and retrieving files according to XDG Base Directory specification")
    (description
     "This package provides a library for storing and retrieving files according to
XDG Base Directory specification.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-afl-0.15
  (package
    (name "rust-afl")
    (version "0.15.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "afl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bkmzmg4fbfxw8x3jy74y72mqjwclqdp4w69rvnzfm6cknj6is52"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-home" ,rust-home-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-xdg" ,rust-xdg-3))))
    (home-page "https://github.com/rust-fuzz/afl.rs")
    (synopsis "Fuzzing Rust code with american-fuzzy-lop")
    (description
     "This package provides Fuzzing Rust code with american-fuzzy-lop.")
    (license license:asl2.0)))

(define-public rust-cpp-demangle-0.4
  (package
    (name "rust-cpp-demangle")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cpp_demangle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z8c656jiwphnw1brkb0whm4kgh39h1msvgig2wc44yi58s8vrcn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-afl" ,rust-afl-0.15)
                       ("rust-cfg-if" ,rust-cfg-if-1))))
    (home-page "https://github.com/gimli-rs/cpp_demangle")
    (synopsis "crate for demangling C++ symbols")
    (description "This package provides a crate for demangling C++ symbols.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasmtime-environ-29
  (package
    (name "rust-wasmtime-environ")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-environ" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bclk4g44v8bcqf9fik9njsjdxqhk290iw54fsi7ld4pf2pw3bfd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cpp-demangle" ,rust-cpp-demangle-0.4)
                       ("rust-cranelift-bitset" ,rust-cranelift-bitset-0.116)
                       ("rust-cranelift-entity" ,rust-cranelift-entity-0.116)
                       ("rust-gimli" ,rust-gimli-0.31)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-object" ,rust-object-0.36)
                       ("rust-postcard" ,rust-postcard-1)
                       ("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.13)
                       ("rust-wasm-encoder" ,rust-wasm-encoder-0.221)
                       ("rust-wasmparser" ,rust-wasmparser-0.221)
                       ("rust-wasmprinter" ,rust-wasmprinter-0.221)
                       ("rust-wasmtime-component-util" ,rust-wasmtime-component-util-29))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis
     "Standalone environment support for WebAssembly code in Cranelift")
    (description
     "This package provides Standalone environment support for @code{WebAssembly} code in Cranelift.")
    (license (list license:asl2.0))))

(define-public rust-cranelift-native-0.116
  (package
    (name "rust-cranelift-native")
    (version "0.116.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-native" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ivna3w02yxa1hs2kkcv6xj8gs8g6mgcrwbpj6x4qb0z7wpyipmq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cranelift-codegen" ,rust-cranelift-codegen-0.116)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.13))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Support for targeting the host with Cranelift")
    (description
     "This package provides Support for targeting the host with Cranelift.")
    (license (list license:asl2.0))))

(define-public rust-cranelift-frontend-0.116
  (package
    (name "rust-cranelift-frontend")
    (version "0.116.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-frontend" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0msd90p3p5hs1bl687hf21k2h1zv60vrw32b2y9p6419czhl3b2g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cranelift-codegen" ,rust-cranelift-codegen-0.116)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.13))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Cranelift IR builder helper")
    (description "This package provides Cranelift IR builder helper.")
    (license (list license:asl2.0))))

(define-public rust-souper-ir-2
  (package
    (name "rust-souper-ir")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "souper-ir" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i60q84w5k3rd0j3zhsdc5xasrd4wrkamyrs01rik3lq6g71h355"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-id-arena" ,rust-id-arena-2))))
    (home-page "https://github.com/fitzgen/souper-ir")
    (synopsis "library for manipulating Souper IR")
    (description "This package provides a library for manipulating Souper IR.")
    (license (list license:expat license:asl2.0))))

(define-public rust-regalloc2-0.11
  (package
    (name "rust-regalloc2")
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regalloc2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16k30jbh1ag5vb4l6p43jik1dzq8pymjbiwblkj189hl32ryc1nw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-allocator-api2" ,rust-allocator-api2-0.2)
                       ("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-hashbrown" ,rust-hashbrown-0.15)
                       ("rust-libfuzzer-sys" ,rust-libfuzzer-sys-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustc-hash" ,rust-rustc-hash-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://github.com/bytecodealliance/regalloc2")
    (synopsis "Backtracking register allocator inspired from IonMonkey")
    (description
     "This package provides Backtracking register allocator inspired from @code{IonMonkey}.")
    (license (list license:asl2.0))))

(define-public rust-cranelift-isle-0.116
  (package
    (name "rust-cranelift-isle")
    (version "0.116.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-isle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kclhdj14l7av24k2h2hlrm4vkza5pyfzabj196h8w2hdrbhv8hw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-codespan-reporting" ,rust-codespan-reporting-0.11)
                       ("rust-log" ,rust-log-0.4))))
    (home-page
     "https://github.com/bytecodealliance/wasmtime/tree/main/cranelift/isle")
    (synopsis
     "ISLE: Instruction Selection and Lowering Expressions. A domain-specific language for instruction selection in Cranelift")
    (description
     "This package provides ISLE: Instruction Selection and Lowering Expressions.  A domain-specific
language for instruction selection in Cranelift.")
    (license (list license:asl2.0))))

(define-public rust-cranelift-control-0.116
  (package
    (name "rust-cranelift-control")
    (version "0.116.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-control" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vymgmnqqnmirvkyqraah03hgnjr2wbzqaj7cfali2mxs3335l96"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "White-box fuzz testing framework")
    (description "This package provides White-box fuzz testing framework.")
    (license (list license:asl2.0))))

(define-public rust-cranelift-codegen-shared-0.116
  (package
    (name "rust-cranelift-codegen-shared")
    (version "0.116.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-codegen-shared" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1src5xxbh4va2g0f1n5lgcfyrqhsk20589ccx1668bspjxa0y620"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis
     "For code shared between cranelift-codegen-meta and cranelift-codegen")
    (description
     "This package provides For code shared between cranelift-codegen-meta and cranelift-codegen.")
    (license (list license:asl2.0))))

(define-public rust-cranelift-codegen-meta-0.116
  (package
    (name "rust-cranelift-codegen-meta")
    (version "0.116.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-codegen-meta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f70xrardmbpvfij1djkavllzrlxy9wi6jkzh9bzq9qvqrqvq169"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cranelift-codegen-shared" ,rust-cranelift-codegen-shared-0.116)
                       ("rust-pulley-interpreter" ,rust-pulley-interpreter-29))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Metaprogram for cranelift-codegen code generator library")
    (description
     "This package provides Metaprogram for cranelift-codegen code generator library.")
    (license (list license:asl2.0))))

(define-public rust-cranelift-entity-0.116
  (package
    (name "rust-cranelift-entity")
    (version "0.116.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-entity" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08xk00xzncxycxm0z1g6daysq2g0qs503pbryvxp8m1732b0sbab"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cranelift-bitset" ,rust-cranelift-bitset-0.116)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Data structures using entity references as mapping keys")
    (description
     "This package provides Data structures using entity references as mapping keys.")
    (license (list license:asl2.0))))

(define-public rust-cranelift-bforest-0.116
  (package
    (name "rust-cranelift-bforest")
    (version "0.116.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-bforest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i13rpcpzi2b26nf1fajf2jcvmpzjgv8rmlav3m3djw6rsh08pg1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cranelift-entity" ,rust-cranelift-entity-0.116))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "forest of B+-trees")
    (description "This package provides a forest of B+-trees.")
    (license (list license:asl2.0))))

(define-public rust-capstone-sys-0.16
  (package
    (name "rust-capstone-sys")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "capstone-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qshi53z72yciyqskswyll6i9q40yjxf90347b3bgzqi2wkq6wgy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.59)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-regex" ,rust-regex-1))))
    (home-page
     "https://github.com/capstone-rust/capstone-rs/tree/master/capstone-sys")
    (synopsis "System bindings to the capstone disassembly library")
    (description
     "This package provides System bindings to the capstone disassembly library.")
    (license license:expat)))

(define-public rust-capstone-0.12
  (package
    (name "rust-capstone")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "capstone" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v2vfzpibdbbabi7nzqrbxn2i5p0a7m8hbhcdchjnnjqv4wa935h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-capstone-sys" ,rust-capstone-sys-0.16)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/capstone-rust/capstone-rs")
    (synopsis
     "High level bindings to capstone disassembly engine (https://capstone-engine.org/)")
    (description
     "This package provides High level bindings to capstone disassembly engine
(https://capstone-engine.org/).")
    (license license:expat)))

(define-public rust-cranelift-codegen-0.116
  (package
    (name "rust-cranelift-codegen")
    (version "0.116.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kihpw7gaj9wx4rw2x9lkn0x3nkcy93vn5pm3hvmh9dl9hn068ic"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-capstone" ,rust-capstone-0.12)
                       ("rust-cranelift-bforest" ,rust-cranelift-bforest-0.116)
                       ("rust-cranelift-bitset" ,rust-cranelift-bitset-0.116)
                       ("rust-cranelift-codegen-meta" ,rust-cranelift-codegen-meta-0.116)
                       ("rust-cranelift-codegen-shared" ,rust-cranelift-codegen-shared-0.116)
                       ("rust-cranelift-control" ,rust-cranelift-control-0.116)
                       ("rust-cranelift-entity" ,rust-cranelift-entity-0.116)
                       ("rust-cranelift-isle" ,rust-cranelift-isle-0.116)
                       ("rust-gimli" ,rust-gimli-0.31)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-postcard" ,rust-postcard-1)
                       ("rust-pulley-interpreter" ,rust-pulley-interpreter-29)
                       ("rust-regalloc2" ,rust-regalloc2-0.11)
                       ("rust-rustc-hash" ,rust-rustc-hash-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-souper-ir" ,rust-souper-ir-2)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.13))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Low-level code generator library")
    (description "This package provides Low-level code generator library.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-cranelift-29
  (package
    (name "rust-wasmtime-cranelift")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-cranelift" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11wyks7pdzshspw6ks5k5z45r28ngm6vrg0g559z2jsdcwiffsrn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cranelift-codegen" ,rust-cranelift-codegen-0.116)
                       ("rust-cranelift-control" ,rust-cranelift-control-0.116)
                       ("rust-cranelift-entity" ,rust-cranelift-entity-0.116)
                       ("rust-cranelift-frontend" ,rust-cranelift-frontend-0.116)
                       ("rust-cranelift-native" ,rust-cranelift-native-0.116)
                       ("rust-gimli" ,rust-gimli-0.31)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-object" ,rust-object-0.36)
                       ("rust-pulley-interpreter" ,rust-pulley-interpreter-29)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.13)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasmparser" ,rust-wasmparser-0.221)
                       ("rust-wasmtime-environ" ,rust-wasmtime-environ-29)
                       ("rust-wasmtime-versioned-export-macros" ,rust-wasmtime-versioned-export-macros-29))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Integration between Cranelift and Wasmtime")
    (description
     "This package provides Integration between Cranelift and Wasmtime.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-wit-bindgen-29
  (package
    (name "rust-wasmtime-wit-bindgen")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-wit-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rh6ds0kcslrwks8k7rz6hand5c4myjx7if1wdwxpr6i5nf32n43"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-heck" ,rust-heck-0.5)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-wit-parser" ,rust-wit-parser-0.221))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Internal `*.wit` support for the `wasmtime` crate's macros")
    (description
     "This package provides Internal `*.wit` support for the `wasmtime` crate's macros.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-component-util-29
  (package
    (name "rust-wasmtime-component-util")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-component-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vh5dzz2nn7clnbdy5igd07nm3igbgxy5krhdcv5maqjq6rwfzbh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis
     "Utility types and functions to support the component model in Wasmtime")
    (description
     "This package provides Utility types and functions to support the component model in Wasmtime.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-component-macro-29
  (package
    (name "rust-wasmtime-component-macro")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-component-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1byzawnz8myzmlwbc0g5y415sk9h5ac464pp0akd0ifr5rcyckfp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-wasmtime-component-util" ,rust-wasmtime-component-util-29)
                       ("rust-wasmtime-wit-bindgen" ,rust-wasmtime-wit-bindgen-29)
                       ("rust-wit-parser" ,rust-wit-parser-0.221))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Macros for deriving component interface types from Rust types")
    (description
     "This package provides Macros for deriving component interface types from Rust types.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-cache-29
  (package
    (name "rust-wasmtime-cache")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-cache" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z2gw8phnwchqdxh0ihxr3qikh6xrq6c92rm0zmdx018yv4624cb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-directories-next" ,rust-directories-next-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-postcard" ,rust-postcard-1)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-toml" ,rust-toml-0.8)
                       ("rust-windows-sys" ,rust-windows-sys-0.59)
                       ("rust-zstd" ,rust-zstd-0.13))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Support for automatic module caching with Wasmtime")
    (description
     "This package provides Support for automatic module caching with Wasmtime.")
    (license (list license:asl2.0))))

(define-public rust-wasmtime-asm-macros-29
  (package
    (name "rust-wasmtime-asm-macros")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-asm-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wk4rwn8085nllba4nz4z68ihl9ypm4zisjvyzlzn0aj286qn5qz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Macros for defining asm functions in Wasmtime")
    (description
     "This package provides Macros for defining asm functions in Wasmtime.")
    (license (list license:asl2.0))))

(define-public rust-wasmparser-0.235
  (package
    (name "rust-wasmparser")
    (version "0.235.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05q9craiackn3aq346y7kv5h7s1p548x3zszxpv66bgs3339c4hn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-hashbrown" ,rust-hashbrown-0.15)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasmparser")
    (synopsis
     "simple event-driven library for parsing WebAssembly binary files.")
    (description
     "This package provides a simple event-driven library for parsing
@code{WebAssembly} binary files.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-wasm-encoder-0.235
  (package
    (name "rust-wasm-encoder")
    (version "0.235.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-encoder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ajw7asr0q3y778r632f3csrlaw8g50qabghgwv23djw74y3kg5k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-leb128fmt" ,rust-leb128fmt-0.1)
                       ("rust-wasmparser" ,rust-wasmparser-0.235))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasm-encoder")
    (synopsis "low-level WebAssembly encoder.")
    (description
     "This package provides a low-level @code{WebAssembly} encoder.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-leb128fmt-0.1
  (package
    (name "rust-leb128fmt")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "leb128fmt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1chxm1484a0bly6anh6bd7a99sn355ymlagnwj3yajafnpldkv89"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/bluk/leb128fmt")
    (synopsis "library to encode and decode LEB128 compressed integers.")
    (description
     "This package provides a library to encode and decode LEB128 compressed integers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wast-235
  (package
    (name "rust-wast")
    (version "235.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wast" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04rfwxfr5y0ac9grps5cllqhw379pd7vwvrspchr1j96ys9l5nhy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-gimli" ,rust-gimli-0.31)
                       ("rust-leb128fmt" ,rust-leb128fmt-0.1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-unicode-width" ,rust-unicode-width-0.2)
                       ("rust-wasm-encoder" ,rust-wasm-encoder-0.235))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wast")
    (synopsis
     "Customizable Rust parsers for the WebAssembly Text formats WAT and WAST")
    (description
     "This package provides Customizable Rust parsers for the @code{WebAssembly} Text formats WAT and WAST.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-wat-1
  (package
    (name "rust-wat")
    (version "1.235.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ah6y8p54r1jfa6rxv1l3z8w6zijhn7vj85jdawkqy8mf4rf0xz7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wast" ,rust-wast-235))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wat")
    (synopsis "Rust parser for the WebAssembly Text format, WAT")
    (description
     "This package provides Rust parser for the @code{WebAssembly} Text format, WAT.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-id-arena-2
  (package
    (name "rust-id-arena")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "id-arena" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01ch8jhpgnih8sawqs44fqsqpc7bzwgy0xpi6j0f4j0i5mkvr8i5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rayon" ,rust-rayon-1))))
    (home-page "https://github.com/fitzgen/id-arena")
    (synopsis "simple, id-based arena.")
    (description "This package provides a simple, id-based arena.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wit-parser-0.221
  (package
    (name "rust-wit-parser")
    (version "0.221.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wit-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b5j2vpiww0crqavaijw4vv1y41darpd38q7id9llsymkrbi4qc9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-id-arena" ,rust-id-arena-2)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-unicode-xid" ,rust-unicode-xid-0.2)
                       ("rust-wasmparser" ,rust-wasmparser-0.221)
                       ("rust-wat" ,rust-wat-1))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wit-parser")
    (synopsis
     "Tooling for parsing `*.wit` files and working with their contents.")
    (description
     "This package provides Tooling for parsing `*.wit` files and working with their contents.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-logos-codegen-0.14
  (package
    (name "rust-logos-codegen")
    (version "0.14.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "logos-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gwnx7lk4y7xc4yk6pr0knrddard5z22rxaz9xrnc38cc1lh1y2r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-beef" ,rust-beef-0.5)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.8)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://logos.maciej.codes/")
    (synopsis "Create ridiculously fast Lexers")
    (description "This package provides Create ridiculously fast Lexers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-logos-derive-0.14
  (package
    (name "rust-logos-derive")
    (version "0.14.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "logos-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07bk3q4jry9f8blrnsiy872ivilzy62xaglnn2ni5p590qmp5yr4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-logos-codegen" ,rust-logos-codegen-0.14))))
    (home-page "https://logos.maciej.codes/")
    (synopsis "Create ridiculously fast Lexers")
    (description "This package provides Create ridiculously fast Lexers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-logos-0.14
  (package
    (name "rust-logos")
    (version "0.14.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "logos" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n349vin9mx326fkz68bsa4vc5sdn9n8qnfz7n1yqynbz1p3albj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-logos-derive" ,rust-logos-derive-0.14))))
    (home-page "https://logos.maciej.codes/")
    (synopsis "Create ridiculously fast Lexers")
    (description "This package provides Create ridiculously fast Lexers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-wave-0.221
  (package
    (name "rust-wasm-wave")
    (version "0.221.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-wave" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ysjy5gp0yflbmlly1scdfpd4m99m2gwc8pzppsyrkinpla86i1w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-logos" ,rust-logos-0.14)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wit-parser" ,rust-wit-parser-0.221))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasm-wave")
    (synopsis "WebAssembly Value Encoding")
    (description "This package provides @code{WebAssembly} Value Encoding.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-wasmparser-0.221
  (package
    (name "rust-wasmparser")
    (version "0.221.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11ficyz79dcypkxxg1c8vl8bm0avg8a80csnxq6vxhismcvglsyh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-hashbrown" ,rust-hashbrown-0.15)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasmparser")
    (synopsis
     "simple event-driven library for parsing WebAssembly binary files.")
    (description
     "This package provides a simple event-driven library for parsing
@code{WebAssembly} binary files.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-wasm-encoder-0.221
  (package
    (name "rust-wasm-encoder")
    (version "0.221.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-encoder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r9jv84kbjf6z18rgf3666vvcn5fybzn9daszsj81pi097z4916w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-leb128" ,rust-leb128-0.2)
                       ("rust-wasmparser" ,rust-wasmparser-0.221))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasm-encoder")
    (synopsis "low-level WebAssembly encoder.")
    (description
     "This package provides a low-level @code{WebAssembly} encoder.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-trait-variant-0.1
  (package
    (name "rust-trait-variant")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trait-variant" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19vpbnbcsxdiznwdw854pd0vya7rm7v7hnl3nh741621603pg5vh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/rust-lang/impl-trait-utils")
    (synopsis "Utilities for working with impl traits in Rust")
    (description
     "This package provides Utilities for working with impl traits in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasmtime-math-29
  (package
    (name "rust-wasmtime-math")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-math" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05yy3npnfk6h199bvibjm00yqf8g12pysp30ai6hzq15mb10w899"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libm" ,rust-libm-0.2))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Low-level math routines used in Wasmtime")
    (description
     "This package provides Low-level math routines used in Wasmtime.")
    (license (list license:asl2.0))))

(define-public rust-cranelift-bitset-0.116
  (package
    (name "rust-cranelift-bitset")
    (version "0.116.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-bitset" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0d4zvspdxpw8jl1q2zm8n1jrxg6kqmkphjr4rrcp49nflxlkjvkw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Various bitset stuff for use inside Cranelift")
    (description
     "This package provides Various bitset stuff for use inside Cranelift.")
    (license (list license:asl2.0))))

(define-public rust-pulley-interpreter-29
  (package
    (name "rust-pulley-interpreter")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pulley-interpreter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pg7rvnqkj47vz91zyqh0b1rvkw8m14jy64qhdqa4jfzfn2mznb2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-cranelift-bitset" ,rust-cranelift-bitset-0.116)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-sptr" ,rust-sptr-0.3)
                       ("rust-wasmtime-math" ,rust-wasmtime-math-29))))
    (home-page "https://github.com/bytecodealliance/wasmtime/tree/main/pulley")
    (synopsis
     "The Pulley interpreter, its bytecode definition, encoder, decoder, and etc..")
    (description
     "This package provides The Pulley interpreter, its bytecode definition, encoder, decoder, and etc...")
    (license (list license:asl2.0))))

(define-public rust-psm-0.1
  (package
    (name "rust-psm")
    (version "0.1.26")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "psm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07xfl8gpz8v9qz2zvnpcy9r3nppbhxlxkgqbxcdwsdl5xij4953f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/rust-lang/stacker/")
    (synopsis
     "Portable Stack Manipulation: stack manipulation and introspection routines")
    (description
     "This package provides Portable Stack Manipulation: stack manipulation and introspection routines.")
    (license (list license:expat license:asl2.0))))

(define-public rust-memfd-0.6
  (package
    (name "rust-memfd")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memfd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r5cm3wzyr1x7768h3hns77b494qbz0g05cb9wgpjvrcsm5gmkxj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustix" ,rust-rustix-0.38))))
    (home-page "https://github.com/lucab/memfd-rs")
    (synopsis "pure-Rust library to work with Linux memfd and sealing")
    (description
     "This package provides a pure-Rust library to work with Linux memfd and sealing.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ittapi-sys-0.4
  (package
    (name "rust-ittapi-sys")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ittapi-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z7lgc7gwlhcvkdk6bg9sf1ww4w0b41blp90hv4a4kq6ji9kixaj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/intel/ittapi/tree/master/rust/ittapi-sys")
    (synopsis "Rust bindings for ittapi")
    (description "This package provides Rust bindings for ittapi.")
    (license (list license:gpl2 license:bsd-3))))

(define-public rust-ittapi-0.4
  (package
    (name "rust-ittapi")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ittapi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cb41dapbximlma0vnar144m2j2km44g8g6zmv6ra4y42kk6z6bb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-ittapi-sys" ,rust-ittapi-sys-0.4)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/intel/ittapi/tree/master/rust/ittapi")
    (synopsis "High-level Rust bindings for ittapi")
    (description "This package provides High-level Rust bindings for ittapi.")
    (license (list license:gpl2 license:bsd-3))))

(define-public rust-fxprof-processed-profile-0.6
  (package
    (name "rust-fxprof-processed-profile")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fxprof-processed-profile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ggsn3im2bfcnxic0jzk00qgiacfrg2as6i4d8kj87kzxl52rl97"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-debugid" ,rust-debugid-0.8)
                       ("rust-fxhash" ,rust-fxhash-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/mstange/samply/")
    (synopsis
     "Create profiles in the Firefox Profiler's processed profile JSON format")
    (description
     "This package provides Create profiles in the Firefox Profiler's processed profile JSON format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasmtime-29
  (package
    (name "rust-wasmtime")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s8f05h19hnnhx2jmi7dr4m1kjbacpbxbih49hf6smbj0qjnm5qi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-addr2line" ,rust-addr2line-0.24)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-fxprof-processed-profile" ,rust-fxprof-processed-profile-0.6)
                       ("rust-gimli" ,rust-gimli-0.31)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-ittapi" ,rust-ittapi-0.4)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mach2" ,rust-mach2-0.4)
                       ("rust-memfd" ,rust-memfd-0.6)
                       ("rust-object" ,rust-object-0.36)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-postcard" ,rust-postcard-1)
                       ("rust-psm" ,rust-psm-0.1)
                       ("rust-pulley-interpreter" ,rust-pulley-interpreter-29)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-sptr" ,rust-sptr-0.3)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.13)
                       ("rust-trait-variant" ,rust-trait-variant-0.1)
                       ("rust-wasm-encoder" ,rust-wasm-encoder-0.221)
                       ("rust-wasm-wave" ,rust-wasm-wave-0.221)
                       ("rust-wasmparser" ,rust-wasmparser-0.221)
                       ("rust-wasmtime-asm-macros" ,rust-wasmtime-asm-macros-29)
                       ("rust-wasmtime-cache" ,rust-wasmtime-cache-29)
                       ("rust-wasmtime-component-macro" ,rust-wasmtime-component-macro-29)
                       ("rust-wasmtime-component-util" ,rust-wasmtime-component-util-29)
                       ("rust-wasmtime-cranelift" ,rust-wasmtime-cranelift-29)
                       ("rust-wasmtime-environ" ,rust-wasmtime-environ-29)
                       ("rust-wasmtime-fiber" ,rust-wasmtime-fiber-29)
                       ("rust-wasmtime-jit-debug" ,rust-wasmtime-jit-debug-29)
                       ("rust-wasmtime-jit-icache-coherence" ,rust-wasmtime-jit-icache-coherence-29)
                       ("rust-wasmtime-math" ,rust-wasmtime-math-29)
                       ("rust-wasmtime-slab" ,rust-wasmtime-slab-29)
                       ("rust-wasmtime-versioned-export-macros" ,rust-wasmtime-versioned-export-macros-29)
                       ("rust-wasmtime-versioned-export-macros" ,rust-wasmtime-versioned-export-macros-29)
                       ("rust-wasmtime-winch" ,rust-wasmtime-winch-29)
                       ("rust-wasmtime-wmemcheck" ,rust-wasmtime-wmemcheck-29)
                       ("rust-wat" ,rust-wat-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "High-level API to expose the Wasmtime runtime")
    (description
     "This package provides High-level API to expose the Wasmtime runtime.")
    (license (list license:asl2.0))))

(define-public rust-winx-0.36
  (package
    (name "rust-winx")
    (version "0.36.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winx" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bgls70sd0lxyhbklbs6ccchx0r2bbz0rcmgwxibhn0ryxvd6grz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/sunfishcode/winx")
    (synopsis "Windows API helper library")
    (description "This package provides Windows API helper library.")
    (license (list license:asl2.0))))

(define-public rust-rustix-linux-procfs-0.1
  (package
    (name "rust-rustix-linux-procfs")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustix-linux-procfs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mi0w4zgw263gaf2ss5a2qg38hcqvh979wjqqzrc85max7vlpj1g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustix" ,rust-rustix-1))))
    (home-page "https://github.com/sunfishcode/rustix-linux-procfs")
    (synopsis "Utilities for opening Linux procfs files and directories")
    (description
     "This package provides Utilities for opening Linux procfs files and directories.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-maybe-owned-0.3
  (package
    (name "rust-maybe-owned")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "maybe-owned" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d3sqiv59i06k73x6p7mf294zgdfb2qkky127ipfnjj9mr9wgb2g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rustonaut/maybe-owned")
    (synopsis
     "provides a `MaybeOwned` (and `MaybeOwnedMut`) type similar to std's `Cow` but it implements `From<T>` and `From<&'a T>` and does not require `ToOwned`")
    (description
     "This package provides provides a `@code{MaybeOwned`} (and `@code{MaybeOwnedMut`}) type similar to
std's `Cow` but it implements `From<T>` and `From<&'a T>` and does not require
`@code{ToOwned`}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ambient-authority-0.0.2
  (package
    (name "rust-ambient-authority")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ambient-authority" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fxsfyhy64jx7zrkb85h1vhr5nfqncja3pwpikid471d8w6yxm79"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/sunfishcode/ambient-authority")
    (synopsis "Ambient Authority")
    (description "This package provides Ambient Authority.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-cap-primitives-3
  (package
    (name "rust-cap-primitives")
    (version "3.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cap-primitives" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0apni9ybd60z3d2zzbwv3c57zghd0464r7gm4syghfags573j7ha"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ambient-authority" ,rust-ambient-authority-0.0.2)
                       ("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-fs-set-times" ,rust-fs-set-times-0.20)
                       ("rust-io-extras" ,rust-io-extras-0.18)
                       ("rust-io-lifetimes" ,rust-io-lifetimes-2)
                       ("rust-ipnet" ,rust-ipnet-2)
                       ("rust-maybe-owned" ,rust-maybe-owned-0.3)
                       ("rust-rustix" ,rust-rustix-1)
                       ("rust-rustix-linux-procfs" ,rust-rustix-linux-procfs-0.1)
                       ("rust-windows-sys" ,rust-windows-sys-0.59)
                       ("rust-winx" ,rust-winx-0.36))))
    (home-page "https://github.com/bytecodealliance/cap-std")
    (synopsis "Capability-based primitives")
    (description "This package provides Capability-based primitives.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-arf-strings-0.7
  (package
    (name "rust-arf-strings")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "arf-strings" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fdqcyss12hyk34wd6v2gsfrwxdclk7ddmyk9f2hb47345mv6f08"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustix" ,rust-rustix-1))))
    (home-page "https://github.com/bytecodealliance/arf-strings")
    (synopsis "Encoding and decoding for ARF strings")
    (description
     "This package provides Encoding and decoding for ARF strings.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-cap-std-3
  (package
    (name "rust-cap-std")
    (version "3.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cap-std" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12a1k1633flbznkfzjg3xmhkh5l4ss4j9hf3fvqmipc3lmf3bh07"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arf-strings" ,rust-arf-strings-0.7)
                       ("rust-camino" ,rust-camino-1)
                       ("rust-cap-primitives" ,rust-cap-primitives-3)
                       ("rust-io-extras" ,rust-io-extras-0.18)
                       ("rust-io-lifetimes" ,rust-io-lifetimes-2)
                       ("rust-rustix" ,rust-rustix-1))))
    (home-page "https://github.com/bytecodealliance/cap-std")
    (synopsis "Capability-based version of the Rust standard library")
    (description
     "This package provides Capability-based version of the Rust standard library.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-wasmtime-c-api-impl-29
  (package
    (name "rust-wasmtime-c-api-impl")
    (version "29.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-c-api-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "037dcqkjwl8vxmnfh6pqlzcpcdjgpb0r9fvvgiwyabcgc3rwwc7a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cap-std" ,rust-cap-std-3)
                       ("rust-env-logger" ,rust-env-logger-0.11)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-wasmtime" ,rust-wasmtime-29)
                       ("rust-wasmtime-c-api-macros" ,rust-wasmtime-c-api-macros-29)
                       ("rust-wasmtime-wasi" ,rust-wasmtime-wasi-29)
                       ("rust-wat" ,rust-wat-1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "C API to expose the Wasmtime runtime")
    (description "This package provides C API to expose the Wasmtime runtime.")
    (license (list license:asl2.0))))

(define-public rust-serde-json-1.0.1
  (package
    (name "rust-serde-json-1.0.1")
    (version "1.0.140")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wwkp4vc20r87081ihj3vpyz5qf7wqkqipq17v99nv6wjrp8n1i0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "JSON serialization file format")
    (description "This package provides a JSON serialization file format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cc-1
  (package
    (name "rust-cc")
    (version "1.2.27")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p5zfsl2mw3j46w58j2sxqkbfi49azilis5335pxlr2z3c3sm1yl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-jobserver" ,rust-jobserver-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-shlex" ,rust-shlex-1))))
    (home-page "https://github.com/rust-lang/cc-rs")
    (synopsis
     "build-time dependency for Cargo build scripts to assist in invoking the native
C compiler to compile native C code into a static archive to be linked into Rust
code.")
    (description
     "This package provides a build-time dependency for Cargo build scripts to assist
in invoking the native C compiler to compile native C code into a static archive
to be linked into Rust code.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tree-sitter-0.25
  (package
    (name "rust-tree-sitter")
    (version "0.25.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tree-sitter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w7rx91039kgms5m6ka2qknrfl3ac7634wb53yjzq2xz7ka1ikx7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.71)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.8)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-streaming-iterator" ,rust-streaming-iterator-0.1)
                       ("rust-tree-sitter-language" ,rust-tree-sitter-language-0.1)
                       ("rust-wasmtime-c-api-impl" ,rust-wasmtime-c-api-impl-29))))
    (home-page "https://tree-sitter.github.io/tree-sitter")
    (synopsis "Rust bindings to the Tree-sitter parsing library")
    (description
     "This package provides Rust bindings to the Tree-sitter parsing library.")
    (license license:expat)))

(define-public rust-tree-sitter-language-0.1
  (package
    (name "rust-tree-sitter-language")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tree-sitter-language" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f14p8i09yrfsf9708jmayy0km78n9pqzxmf31xzd0vk45q3j0f4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://tree-sitter.github.io/tree-sitter")
    (synopsis
     "The tree-sitter Language type, used by the library and by language implementations")
    (description
     "This package provides The tree-sitter Language type, used by the library and by language
implementations.")
    (license license:expat)))



(define-public rust-const-format-0.2.31
  (package
    (name "rust-const-format")
    (version "0.2.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "const_format" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j7zs1aar3daic7yy18sg34a518f5zzimn3q8fd1yww5lb3yz469"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--features=__test")
       #:cargo-inputs
       (("rust-const-format-proc-macros" ,rust-const-format-proc-macros-0.2.31)
        ("rust-konst" ,rust-konst-0.2))
       #:cargo-development-inputs (("rust-arrayvec" ,rust-arrayvec-0.5)
                                   ("rust-fastrand" ,rust-fastrand-1))))
    (home-page "https://github.com/rodrimati1992/const_format_crates/")
    (synopsis "Compile-time string formatting")
    (description "This package provides compile-time string formatting.")
    (license license:zlib)))

(define-public rust-const-format-proc-macros-0.2.31
  (package
    (name "rust-const-format-proc-macros")
    (version "0.2.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "const_format_proc_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xibiffpmwvlina6amybiz66g5zgs5r5gk9jrywlr1sa377bc9p0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-unicode-xid" ,rust-unicode-xid-0.2))
       #:cargo-development-inputs (("rust-fastrand" ,rust-fastrand-1))))
    (home-page "https://github.com/rodrimati1992/const_format_crates/")
    (synopsis "Implementation detail of the @code{const_format} crate")
    (description "Implementation detail of the @code{const_format} crate.")
    (license license:zlib)))


(define-public rust-sscanf-0.4.3
  (package
    (name "rust-sscanf-0.4.3")
    (version "0.4.3")
    (source (origin
	      (method url-fetch)
              (uri (crate-uri "sscanf" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
		"1w6lfy9sr1fh1ar3k68wjyscc9kpdi4ngygwixf0613aafdh1lfb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("const-format" ,rust-const-format-0.2.31)
	("regex" ,rust-regex-1)
	("sscanf-macro" ,rust-sscanf-macro-0.4.3)
	("lazy-static" ,rust-lazy-static-1))))
    (synopsis "A Rust crate with a sscanf (inverse of format!()) Macro based on Regex")
    (description
     "A Rust crate with a sscanf (inverse of format!()) Macro based on Regex")
    (home-page "https://github.com/mich101mich/sscanf")
    (license license:isc)))

(define-public rust-sscanf-macro-0.4.3
  (package
    (name "rust-sscanf-macro")
    (version "0.4.3")
    (source (origin
	      (method url-fetch)
              (uri (crate-uri "sscanf_macro" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
		"0dqsrabv6zmphzm0ssrq3h07gq67ccrp7kvn4kdbqjsp19iy1z6g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("unicode-width" ,rust-unicode-width-0.1.12))))
    (synopsis "A Rust crate with a sscanf (inverse of format!()) Macro based on Regex")
    (description
     "A Rust crate with a sscanf (inverse of format!()) Macro based on Regex")
    (home-page "https://github.com/mich101mich/sscanf")
    (license license:isc)))

(define-public rust-colorsys-0.6.7
  (package
    (name "rust-colorsys")
    (version "0.6.7")
    (source (origin
	      (method url-fetch)
              (uri (crate-uri "colorsys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
		"1g8vwcv89n2dzi9bmbzqlj9cl9a89jz49668grbcncv4cjx1l9jl"))))
    (build-system cargo-build-system)
    (synopsis "A module for color conversion and mutation written in Rust.")
    (description
     "A module for color conversion and mutation written in Rust.")
    (home-page "https://github.com/emgyrz/colorsys.rs")
    (license license:isc)))

(define-public rust-estimated-read-time-1
  (package
    (name "rust-estimated-read-time")
    (version "1.0.0")
    (source (origin
	      (method url-fetch)
              (uri (crate-uri "estimated_read_time" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
		"1mz8pkgk9v0cfzfjw659zl997gilangb78ccds8gic8h2hsgv734"))))
    (build-system cargo-build-system)
    (synopsis "Calculates the time taken to read any text.")
    (description
     "Calculates the time taken to read any text.")
    (home-page "https://github.com/karthik512/estimated_read_time")
    (license license:isc)))

(define-public rust-dbus-codegen-0.12
  (package
    (name "rust-dbus-codegen")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dbus-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04n8vpk9z0dc06cnh1i7707v7h5sg20c19vaa9ykrj0zv5signdw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("xml-rs" ,rust-xml-rs-0.8))))
    (home-page "https://crates.io/crates/dbus-codegen")
    (synopsis "Application-level tracing for Rust")
    (description "")
    (license license:expat)))

(define-public rust-unicode-width-0.1.12
  (package
    (name "rust-unicode-width")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode-width" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mk6mybsmi5py8hf8zy9vbgs4rw4gkdqdq3gzywd9kwf2prybxb8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("xml-rs" ,rust-xml-rs-0.8))))
    (home-page "https://crates.io/crates/dbus-codegen")
    (synopsis "Application-level tracing for Rust")
    (description "")
    (license license:expat)))

(define-public rust-tracing-0.1.40
  (package
    (name "rust-tracing")
    (version "0.1.40")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vv48dac9zgj9650pg2b4d0j3w6f3x9gbggf43scq5hrlysklln3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f		    ; unresolved import `tracing_mock`
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-tracing-attributes" ,rust-tracing-attributes-0.1)
        ("rust-tracing-core" ,rust-tracing-core-0.1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://tokio.rs")
    (synopsis "Application-level tracing for Rust")
    (description "@code{rust-tracing} is a framework for instrumenting Rust
programs to collect structured, event-based diagnostic information.")
    (license license:expat)))

(define-public rust-tracing-subscriber-0.3.18
  (package
    (name "rust-tracing-subscriber")
    (version "0.3.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing-subscriber" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12vs1bwk4kig1l2qqjbbn2nm5amwiqmkcmnznylzmnfvjy6083xd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f     ; use of undeclared crate or module `tracing_mock`
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-matchers" ,rust-matchers-0.1)
        ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.46)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sharded-slab" ,rust-sharded-slab-0.1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thread-local" ,rust-thread-local-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-core" ,rust-tracing-core-0.1)
        ("rust-tracing-log" ,rust-tracing-log-0.2)
        ("rust-tracing-serde" ,rust-tracing-serde-0.2)
        ("rust-valuable" ,rust-valuable-0.1)
        ("rust-valuable-serde" ,rust-valuable-serde-0.1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-regex" ,rust-regex-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-futures" ,rust-tracing-futures-0.2)
        ("rust-tracing-log" ,rust-tracing-log-0.2))))
    (home-page "https://tokio.rs")
    (synopsis "Implement and compose tracing subscribers")
    (description
     "This package provides utilities for implementing and composing tracing
subscribers.

Tracing is a framework for instrumenting Rust programs to collect scoped,
structured, and async-aware diagnostics.  The Subscriber trait represents the
functionality necessary to collect this trace data.  This crate contains tools
for composing subscribers out of smaller units of behaviour, and
batteries-included implementations of common subscriber functionality.

Tracing-subscriber is intended for use by both Subscriber authors and
application authors using tracing to instrument their applications.")
    (license license:expat)))

