;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (afistfullofash packages rust-crates)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages rust-sources)
  #:export (lookup-cargo-inputs))

;;;
;;; This file is managed by ‘guix import’.  Do NOT add definitions manually.
;;;

;;;
;;; Rust libraries fetched from crates.io and non-workspace development
;;; snapshots.
;;;

(define qqqq-separator 'begin-of-crates)

(define rust-adler32-1.2.0
  (crate-source "adler32" "1.2.0"
                "0d7jq7jsjyhsgbhnfq5fvrlh9j0i9g1fqrl2735ibv5f75yjgqda"))

(define rust-ahash-0.8.12
  (crate-source "ahash" "0.8.12"
                "0xbsp9rlm5ki017c0w6ay8kjwinwm8knjncci95mii30rmwz25as"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-ansi-term-0.12.1
  (crate-source "ansi_term" "0.12.1"
                "1ljmkbilxgmhavxvxqa7qvm6f3fjggi7q2l3a72q9x0cxjvrnanm"))

(define rust-as-raw-xcb-connection-1.0.1
  (crate-source "as-raw-xcb-connection" "1.0.1"
                "0sqgpz2ymv5yx76r5j2npjq2x5qvvqnw0vrs35cyv30p3pfp2m8p"))

(define rust-atty-0.2.14
  (crate-source "atty" "0.2.14"
                "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.9.4
  (crate-source "bitflags" "2.9.4"
                "157kkcv8s7vk6d17dar1pa5cqcz4c8pdrn16wm1ld7jnr86d2q92"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-bstr-1.12.0
  (crate-source "bstr" "1.12.0"
                "195i0gd7r7jg7a8spkmw08492n7rmiabcvz880xn2z8dkp8i6h93"))

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

(define rust-cairo-rs-0.19.4
  (crate-source "cairo-rs" "0.19.4"
                "0qp5rixgipdj9d8yd5458hzfxam1rgpzcxi90vq6q0v91r6jmb5j"))

(define rust-cairo-sys-rs-0.19.2
  (crate-source "cairo-sys-rs" "0.19.2"
                "0r0yp0lph77lm4blrn6fvdmz2i3r8ibkkjg6nmwbvvv4jq8v6fzx"))

(define rust-cc-1.2.39
  (crate-source "cc" "1.2.39"
                "0py3546wz3k5qi6pbfz80jvg0g3qgzr21c7a1p5wjvscjm4l6dg1"))

(define rust-cfg-expr-0.15.8
  (crate-source "cfg-expr" "0.15.8"
                "00lgf717pmf5qd2qsxxzs815v6baqg38d6m5i6wlh235p14asryh"))

(define rust-cfg-if-1.0.3
  (crate-source "cfg-if" "1.0.3"
                "1afg7146gbxjvkbjx7i5sdrpqp9q5akmk9004fr8rsm90jf2il9g"))

(define rust-chrono-0.4.42
  (crate-source "chrono" "0.4.42"
                "1lp8iz9js9jwxw0sj8yi59v54lgvwdvm49b9wch77f25sfym4l0l"))

(define rust-chrono-tz-0.9.0
  (crate-source "chrono-tz" "0.9.0"
                "1fvicqrlmdsjkrgxr7bxfd62i9w2qi2b6iv4w85av5syvqlqnsck"))

(define rust-chrono-tz-build-0.3.0
  (crate-source "chrono-tz-build" "0.3.0"
                "1c8ixwwwsn9kgs1dr5mz963p0fgw9j9p7fzb3w2c7y8xhkp8l20c"))

(define rust-clap-2.34.0
  (crate-source "clap" "2.34.0"
                "071q5d8jfwbazi6zhik9xwpacx5i6kb2vkzy060vhf0c3120aqd0"))

(define rust-colorsys-0.6.7
  ;; TODO: Check bundled sources.
  (crate-source "colorsys" "0.6.7"
                "1g8vwcv89n2dzi9bmbzqlj9cl9a89jz49668grbcncv4cjx1l9jl"))

(define rust-const-format-0.2.31
  (crate-source "const_format" "0.2.31"
                "0j7zs1aar3daic7yy18sg34a518f5zzimn3q8fd1yww5lb3yz469"))

(define rust-const-format-proc-macros-0.2.31
  (crate-source "const_format_proc_macros" "0.2.31"
                "1xibiffpmwvlina6amybiz66g5zgs5r5gk9jrywlr1sa377bc9p0"))

(define rust-convert-case-0.6.0
  (crate-source "convert_case" "0.6.0"
                "1jn1pq6fp3rri88zyw6jlhwwgf6qiyc08d6gjv0qypgkl862n67c"))

(define rust-core-foundation-sys-0.8.7
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-core2-0.4.0
  (crate-source "core2" "0.4.0"
                "01f5xv0kf3ds3xm7byg78hycbanb8zlpvsfv4j47y46n3bpsg6xl"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-crc32fast-1.5.0
  (crate-source "crc32fast" "1.5.0"
                "04d51liy8rbssra92p0qnwjw8i9rm9c4m3bwy19wjamz1k4w30cl"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-dary-heap-0.3.8
  (crate-source "dary_heap" "0.3.8"
                "010zfln7257vq9fsgcslkqs5gmcm1ahrri118bkhgh7igllf7lh6"))

(define rust-dbus-0.9.9
  (crate-source "dbus" "0.9.9"
                "1sfib87472q429k3j1hwhbjc7vcpjhz8hnnzd2ssfmdbx1an42qr"))

(define rust-dbus-codegen-0.11.0
  (crate-source "dbus-codegen" "0.11.0"
                "04n8vpk9z0dc06cnh1i7707v7h5sg20c19vaa9ykrj0zv5signdw"))

(define rust-dbus-crossroads-0.5.2
  (crate-source "dbus-crossroads" "0.5.2"
                "1q3dyywazr3hppm052fa8q2366q66ml789r42jjlnm47f51q6k1s"))

(define rust-deunicode-1.6.2
  (crate-source "deunicode" "1.6.2"
                "013biy7hhy59jcbry4dqn2pf4qhaw083ksn8xxiw373wjc37imdb"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-dirs-5.0.1
  (crate-source "dirs" "5.0.1"
                "0992xk5vx75b2x91nw9ssb51mpl8x73j9rxmpi96cryn0ffmmi24"))

(define rust-dirs-sys-0.4.1
  ;; TODO: Check bundled sources.
  (crate-source "dirs-sys" "0.4.1"
                "071jy0pvaad9lsa6mzawxrh7cmr7hsmsdxwzm7jzldfkrfjha3sj"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-estimated-read-time-1.0.0
  (crate-source "estimated_read_time" "1.0.0"
                "1mz8pkgk9v0cfzfjw659zl997gilangb78ccds8gic8h2hsgv734"))

(define rust-find-msvc-tools-0.1.2
  (crate-source "find-msvc-tools" "0.1.2"
                "0nbrhvk4m04hviiwbqp2jwcv9j2k70x0q2kcvfk51iygvaqp7v8w"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

(define rust-futures-executor-0.3.31
  (crate-source "futures-executor" "0.3.31"
                "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y"))

(define rust-futures-io-0.3.31
  (crate-source "futures-io" "0.3.31"
                "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y"))

(define rust-futures-macro-0.3.31
  (crate-source "futures-macro" "0.3.31"
                "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))

(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-gethostname-1.0.2
  (crate-source "gethostname" "1.0.2"
                "0mdfkmfr41xx1i0nlwgzncmnj2a5f18kn6ydp7j1qc1q83dpy9gw"))

(define rust-getrandom-0.2.16
  (crate-source "getrandom" "0.2.16"
                "14l5aaia20cc6cc08xdlhrzmfcylmrnprwnna20lqf746pqzjprk"))

(define rust-getrandom-0.3.3
  (crate-source "getrandom" "0.3.3"
                "1x6jl875zp6b2b6qp9ghc84b0l76bvng2lvm8zfcmwjl7rb5w516"))

(define rust-gio-0.19.8
  (crate-source "gio" "0.19.8"
                "1znz5ngfvv3gbndf6lzz3hs27hlb8ysls4axlfccrzvkscbz2jac"))

(define rust-gio-sys-0.19.8
  ;; TODO: Check bundled sources.
  (crate-source "gio-sys" "0.19.8"
                "1vylsskpipfwl7mvffp1s0227d0k5amyhd32dfnp3mhl8yx47mrc"))

(define rust-glib-0.19.9
  (crate-source "glib" "0.19.9"
                "0i2ak1scmzfmfxbm2dr146jl4y9mafxf1ald05jr8iimy5wh4r9r"))

(define rust-glib-macros-0.19.9
  (crate-source "glib-macros" "0.19.9"
                "1mzsh8jkg8vldvgvr9gsaidvn2myn5cbdn8a6m8rgbhlg8kv0aa4"))

(define rust-glib-sys-0.19.8
  ;; TODO: Check bundled sources.
  (crate-source "glib-sys" "0.19.8"
                "19f4q8x77vd7c1d9ikw492yskq5kpd7k04qb8xnh1c427a6w2baw"))

(define rust-globset-0.4.16
  (crate-source "globset" "0.4.16"
                "1xa9ivqs74imf1q288spxh49g6iw2mn3x9snibdgapazzj6h58al"))

(define rust-globwalk-0.9.1
  (crate-source "globwalk" "0.9.1"
                "0mz7bsa66p2rrgnz3l94ac4kbklh7mq8j30iizyxjy4qyvmn1xqb"))

(define rust-gobject-sys-0.19.8
  ;; TODO: Check bundled sources.
  (crate-source "gobject-sys" "0.19.8"
                "17lb7dfbpcg8zchwlfbc08kckwf0a7d9n5ly3pyic13f5ljpws9f"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.16.0
  (crate-source "hashbrown" "0.16.0"
                "13blh9j2yv77a6ni236ixiwdzbc1sh2bc4bdpaz7y859yv2bs6al"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.1.19
  (crate-source "hermit-abi" "0.1.19"
                "0cxcm8093nf5fyn114w8vxbrbcyvv91d4015rdnlgfll7cs6gd32"))

(define rust-humansize-2.1.3
  (crate-source "humansize" "2.1.3"
                "1msxd1akb3dydsa8qs461sds9krwnn31szvqgaq93p4x0ad1rdbc"))

(define rust-humantime-2.3.0
  (crate-source "humantime" "2.3.0"
                "092lpipp32ayz4kyyn4k3vz59j9blng36wprm5by0g2ykqr14nqk"))

(define rust-iana-time-zone-0.1.64
  (crate-source "iana-time-zone" "0.1.64"
                "1yz980fmhaq9bdkasz35z63az37ci6kzzfhya83kgdqba61pzr9k"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-ignore-0.4.23
  (crate-source "ignore" "0.4.23"
                "0jysggjfmlxbg60vhhiz4pb8jfb7cnq5swdsvxknbs7x18wgv2bd"))

(define rust-include-flate-0.3.1
  (crate-source "include-flate" "0.3.1"
                "167r4qx7yfs4vphrpgh98ixkmd94jy63a76sghg64ak8rav7q6z0"))

(define rust-include-flate-codegen-0.3.1
  (crate-source "include-flate-codegen" "0.3.1"
                "0l40qk0p1pi020v3y5ywh6jfzwgafyli9fkfds6ldgmffi9byjag"))

(define rust-include-flate-compress-0.3.1
  (crate-source "include-flate-compress" "0.3.1"
                "1g2dhaizqw9ixpyi751fgrj4yaji47crrdyvylqmkkbbf47a9rpa"))

(define rust-indexmap-2.11.4
  (crate-source "indexmap" "2.11.4"
                "1rc8bgcjzfcskz1zipjjm7s3m1jskzhnhr9jxmsafhdk1xv863sb"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-jobserver-0.1.34
  (crate-source "jobserver" "0.1.34"
                "0cwx0fllqzdycqn4d6nb277qx5qwnmjdxdl0lxkkwssx77j3vyws"))

(define rust-js-sys-0.3.81
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.81"
                "01ckbf16iwh7qj92fax9zh8vf2y9sk60cli6999cn7a1jxx96j7c"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-libc-0.2.176
  (crate-source "libc" "0.2.176"
                "0x7ivn80h7nz2l46vra7bxx36s6r8d0lkax14dx97skjsss2kyaq"))

(define rust-libdbus-sys-0.2.6
  ;; TODO: Check bundled sources.
  (crate-source "libdbus-sys" "0.2.6"
                "17xx4dy30fn81zhwsm4y2c84wr0apyiams8hy20lc3mmzrp8bgjw"))

(define rust-libflate-2.1.0
  (crate-source "libflate" "2.1.0"
                "07mj9z89vbhq837q58m4v2nblgsmrn6vrp8w1j8g0kpa2kfdzna5"))

(define rust-libflate-lz77-2.1.0
  (crate-source "libflate_lz77" "2.1.0"
                "0gc6h98jwigscasz8vw1vv65b3rismqcbndb8hf6yf4z6qxxgq76"))

(define rust-libm-0.2.15
  (crate-source "libm" "0.2.15"
                "1plpzf0p829viazdj57yw5dhmlr8ywf3apayxc2f2bq5a6mvryzr"))

(define rust-libredox-0.1.10
  (crate-source "libredox" "0.1.10"
                "1jswil4ai90s4rh91fg8580x8nikni1zl3wnch4h01nvidqpwvs1"))

(define rust-linux-raw-sys-0.11.0
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.11.0"
                "0fghx0nn8nvbz5yzgizfcwd6ap2pislp68j8c1bwyr6sacxkq7fz"))

(define rust-log-0.4.28
  (crate-source "log" "0.4.28"
                "0cklpzrpxafbaq1nyxarhnmcw9z3xcjrad3ch55mmr58xw2ha21l"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"))

(define rust-pango-0.19.8
  (crate-source "pango" "0.19.8"
                "1kffxkk7730csly86fkgja50k1184zj9lz49sv7qb0059233439z"))

(define rust-pango-sys-0.19.8
  ;; TODO: Check bundled sources.
  (crate-source "pango-sys" "0.19.8"
                "182bcd6255v5yvnskbhxnb6kwak240z7sn54si2b5h46l17xl0zz"))

(define rust-pangocairo-0.19.8
  (crate-source "pangocairo" "0.19.8"
                "1n8wrqy260zpfiifb2n10mbsv3kbrvxm1z7pv8b4w77c08yb9j74"))

(define rust-pangocairo-sys-0.19.8
  ;; TODO: Check bundled sources.
  (crate-source "pangocairo-sys" "0.19.8"
                "1myq3p8qrd63nlacd4sba66c17lfqgvzv8mpyn2rg1rqhi4h86ar"))

(define rust-parse-zoneinfo-0.3.1
  (crate-source "parse-zoneinfo" "0.3.1"
                "093cs8slbd6kyfi6h12isz0mnaayf5ha8szri1xrbqj4inqhaahz"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-pest-2.8.2
  (crate-source "pest" "2.8.2"
                "1a6g94pr4npsg0s6ljddwp4g127ks0nygzhxcpwfmyik6yis7q11"))

(define rust-pest-derive-2.8.2
  (crate-source "pest_derive" "2.8.2"
                "0qy6nv84q1m6m2rzw1qjfbxlcizz7h557rkk16yivjqafxpp0n5w"))

(define rust-pest-generator-2.8.2
  (crate-source "pest_generator" "2.8.2"
                "0bws5i6g3v1sldvy66p7qbzmz5mqbiflcqilaywgf1zy3n0kckvd"))

(define rust-pest-meta-2.8.2
  (crate-source "pest_meta" "2.8.2"
                "0844iv71ibf414yid0bvhi9i0zfi0jrmyh6mvjjx1jws102rp4a2"))

(define rust-phf-0.11.3
  (crate-source "phf" "0.11.3"
                "0y6hxp1d48rx2434wgi5g8j1pr8s5jja29ha2b65435fh057imhz"))

(define rust-phf-codegen-0.11.3
  (crate-source "phf_codegen" "0.11.3"
                "0si1n6zr93kzjs3wah04ikw8z6npsr39jw4dam8yi9czg2609y5f"))

(define rust-phf-generator-0.11.3
  (crate-source "phf_generator" "0.11.3"
                "0gc4np7s91ynrgw73s2i7iakhb4lzdv1gcyx7yhlc0n214a2701w"))

(define rust-phf-shared-0.11.3
  (crate-source "phf_shared" "0.11.3"
                "1rallyvh28jqd9i916gk5gk2igdmzlgvv5q0l3xbf3m6y8pbrsk7"))

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-proc-macro-crate-3.4.0
  (crate-source "proc-macro-crate" "3.4.0"
                "10v9qi51n4phn1lrj5r94kjq7yhci9jrkqnn6wpan05yjsgb3711"))

(define rust-proc-macro-error-1.0.4
  (crate-source "proc-macro-error" "1.0.4"
                "1373bhxaf0pagd8zkyd03kkx6bchzf6g0dkwrwzsnal9z47lj9fs"))

(define rust-proc-macro-error-attr-1.0.4
  (crate-source "proc-macro-error-attr" "1.0.4"
                "0sgq6m5jfmasmwwy8x4mjygx5l7kp8s4j60bv25ckv2j1qc41gm1"))

(define rust-proc-macro2-1.0.101
  (crate-source "proc-macro2" "1.0.101"
                "1pijhychkpl7rcyf1h7mfk6gjfii1ywf5n0snmnqs5g4hvyl7bl9"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-redox-users-0.4.6
  (crate-source "redox_users" "0.4.6"
                "0hya2cxx6hxmjfxzv9n8rjl5igpychav7zfi1f81pz6i4krry05s"))

(define rust-regex-1.11.3
  (crate-source "regex" "1.11.3"
                "0b58ya98c4i5cjjiwhpcnjr61cv9g143qhdwhsryggj09098hllb"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.4.11
  (crate-source "regex-automata" "0.4.11"
                "1bawj908pxixpggcnma3xazw53mwyz68lv9hn4yg63nlhv7bjgl3"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.8.6
  (crate-source "regex-syntax" "0.8.6"
                "00chjpglclfskmc919fj5aq308ffbrmcn7kzbkz92k231xdsmx6a"))

(define rust-rle-decode-fast-1.0.3
  (crate-source "rle-decode-fast" "1.0.3"
                "08kljzl29rpm12fiz0qj5pask49aiswdvcjigdcq73s224rgd0im"))

(define rust-rust-embed-8.7.2
  (crate-source "rust-embed" "8.7.2"
                "12hprnl569f1pg2sn960gfla913mk1mxdwpn2a6vl9iad2w0hn82"))

(define rust-rust-embed-impl-8.7.2
  (crate-source "rust-embed-impl" "8.7.2"
                "171lshvdh122ypbf23gmhvrqnhbk0q9g27gaq6g82w9b76jg2rb0"))

(define rust-rust-embed-utils-8.7.2
  (crate-source "rust-embed-utils" "8.7.2"
                "151m1966qk75y10msazdp0xj4fqw1khcry0z946bf84bcj0hrk7n"))

(define rust-rustix-1.1.2
  (crate-source "rustix" "1.1.2"
                "0gpz343xfzx16x82s1x336n0kr49j02cvhgxdvaq86jmqnigh5fd"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-serde-1.0.227
  (crate-source "serde" "1.0.227"
                "0ia2p85z8ypyjvl7x6a0pxy5fgjd6c3hrd9a76slxvgvqqzy9v40"))

(define rust-serde-core-1.0.227
  (crate-source "serde_core" "1.0.227"
                "1r9vnglazz5vfpi32by80c1nig1jvy9h2hcyl9pci8h7nrsn4mvs"))

(define rust-serde-derive-1.0.227
  (crate-source "serde_derive" "1.0.227"
                "016y5ryfv99z7a1khyrmiws5zq6lc07xyaiqkc7cy9487f999rji"))

(define rust-serde-json-1.0.145
  (crate-source "serde_json" "1.0.145"
                "1767y6kxjf7gwpbv8bkhgwc50nhg46mqwm9gy9n122f7v1k6yaj0"))

(define rust-serde-regex-1.1.0
  (crate-source "serde_regex" "1.1.0"
                "1pxsnxb8c198szghk1hvzvhva36w2q5zs70hqkmdf5d89qd6y4x8"))

(define rust-serde-spanned-0.6.9
  (crate-source "serde_spanned" "0.6.9"
                "18vmxq6qfrm110caszxrzibjhy2s54n1g5w1bshxq9kjmz7y0hdz"))

(define rust-sha2-0.10.9
  (crate-source "sha2" "0.10.9"
                "10xjj843v31ghsksd9sl9y12qfc48157j1xpb8v1ml39jy0psl57"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-slab-0.4.11
  (crate-source "slab" "0.4.11"
                "12bm4s88rblq02jjbi1dw31984w61y2ldn13ifk5gsqgy97f8aks"))

(define rust-slug-0.1.6
  (crate-source "slug" "0.1.6"
                "0977cyp88xrwbpmqwzafkvv8vm9i0gdb5zjskb6f6pg45vvq0al8"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-sscanf-0.4.3
  (crate-source "sscanf" "0.4.3"
                "1w6lfy9sr1fh1ar3k68wjyscc9kpdi4ngygwixf0613aafdh1lfb"))

(define rust-sscanf-macro-0.4.3
  (crate-source "sscanf_macro" "0.4.3"
                "0dqsrabv6zmphzm0ssrq3h07gq67ccrp7kvn4kdbqjsp19iy1z6g"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-strsim-0.8.0
  (crate-source "strsim" "0.8.0"
                "0sjsm7hrvjdifz661pjxq5w4hf190hx53fra8dfvamacvff139cf"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.106
  (crate-source "syn" "2.0.106"
                "19mddxp1ia00hfdzimygqmr1jqdvyl86k48427bkci4d08wc9rzd"))

(define rust-system-deps-6.2.2
  (crate-source "system-deps" "6.2.2"
                "0j93ryw031n3h8b0nfpj5xwh3ify636xmv8kxianvlyyipmkbrd3"))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-tera-1.20.0
  (crate-source "tera" "1.20.0"
                "1vnj9imw2h9szkd1izsrhwrc9jvazvdsp84x65wg2rg88ldqb7db"))

(define rust-textwrap-0.11.0
  (crate-source "textwrap" "0.11.0"
                "0q5hky03ik3y50s9sz25r438bc4nwhqc6dqwynv4wylc807n29nk"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.16
  (crate-source "thiserror" "2.0.16"
                "1h30bqyjn5s9ypm668yd9849371rzwk185klwgjg503k2hadcrrl"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.16
  (crate-source "thiserror-impl" "2.0.16"
                "0q3r1ipr1rhff6cgrcvc0njffw17rpcqz9hdc7p754cbqkhinpkc"))

(define rust-thread-local-1.1.9
  (crate-source "thread_local" "1.1.9"
                "1191jvl8d63agnq06pcnarivf63qzgpws5xa33hgc92gjjj4c0pn"))

(define rust-toml-0.8.23
  (crate-source "toml" "0.8.23"
                "0qnkrq4lm2sdhp3l6cb6f26i8zbnhqb7mhbmksd550wxdfcyn6yw"))

(define rust-toml-datetime-0.6.11
  (crate-source "toml_datetime" "0.6.11"
                "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))

(define rust-toml-datetime-0.7.2
  (crate-source "toml_datetime" "0.7.2"
                "1hgff8gdk9yx7dljkqfijmj0sc5ln4xhpj045divdhi7xifhiw9j"))

(define rust-toml-edit-0.22.27
  (crate-source "toml_edit" "0.22.27"
                "16l15xm40404asih8vyjvnka9g0xs9i4hfb6ry3ph9g419k8rzj1"))

(define rust-toml-edit-0.23.6
  (crate-source "toml_edit" "0.23.6"
                "0jqq4wz6is0497a42m0wh4j3x4vgp70wrlndd57zzzc61rygxvzk"))

(define rust-toml-parser-1.0.3
  (crate-source "toml_parser" "1.0.3"
                "09x6i0b57lwc7yn6w1kbd2ypm4vpcrgd2vdax7h745g77g1r7y2c"))

(define rust-toml-write-0.1.2
  (crate-source "toml_write" "0.1.2"
                "008qlhqlqvljp1gpp9rn5cqs74gwvdgbvs92wnpq8y3jlz4zi6ax"))

(define rust-tracing-0.1.40
  (crate-source "tracing" "0.1.40"
                "1vv48dac9zgj9650pg2b4d0j3w6f3x9gbggf43scq5hrlysklln3"))

(define rust-tracing-attributes-0.1.30
  (crate-source "tracing-attributes" "0.1.30"
                "00v9bhfgfg3v101nmmy7s3vdwadb7ngc8c1iw6wai9vj9sv3lf41"))

(define rust-tracing-core-0.1.34
  (crate-source "tracing-core" "0.1.34"
                "0y3nc4mpnr79rzkrcylv5f5bnjjp19lsxwis9l4kzs97ya0jbldr"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.18
  (crate-source "tracing-subscriber" "0.3.18"
                "12vs1bwk4kig1l2qqjbbn2nm5amwiqmkcmnznylzmnfvjy6083xd"))

(define rust-typenum-1.18.0
  (crate-source "typenum" "1.18.0"
                "0gwgz8n91pv40gabrr1lzji0b0hsmg0817njpy397bq7rvizzk0x"))

(define rust-ucd-trie-0.1.7
  (crate-source "ucd-trie" "0.1.7"
                "0wc9p07sqwz320848i52nvyjvpsxkx3kv5bfbmm6s35809fdk5i8"))

(define rust-unic-char-property-0.9.0
  (crate-source "unic-char-property" "0.9.0"
                "08g21dn3wwix3ycfl0vrbahn0835nv2q3swm8wms0vwvgm07mid8"))

(define rust-unic-char-range-0.9.0
  (crate-source "unic-char-range" "0.9.0"
                "1g0z7iwvjhqspi6194zsff8vy6i3921hpqcrp3v1813hbwnh5603"))

(define rust-unic-common-0.9.0
  (crate-source "unic-common" "0.9.0"
                "1g1mm954m0zr497dl4kx3vr09yaly290zs33bbl4wrbaba1gzmw0"))

(define rust-unic-segment-0.9.0
  (crate-source "unic-segment" "0.9.0"
                "08wgz2q6vrdvmbd23kf9pbg8cyzm5q8hq9spc4blzy2ppqk5vvg4"))

(define rust-unic-ucd-segment-0.9.0
  (crate-source "unic-ucd-segment" "0.9.0"
                "0027lczcg0r401g6fnzm2bq9fxhgxvri1nlryhhv8192lqic2y90"))

(define rust-unic-ucd-version-0.9.0
  (crate-source "unic-ucd-version" "0.9.0"
                "1i5hnzpfnxkp4ijfk8kvhpvj84bij575ybqx1b6hyigy6wi2zgcn"))

(define rust-unicode-ident-1.0.19
  (crate-source "unicode-ident" "1.0.19"
                "17bx1j1zf6b9j3kpyf74mraary7ava3984km0n8kh499h5a58fpn"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-width-0.1.12
  (crate-source "unicode-width" "0.1.12"
                "1mk6mybsmi5py8hf8zy9vbgs4rw4gkdqdq3gzywd9kwf2prybxb8"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-vec-map-0.8.2
  (crate-source "vec_map" "0.8.2"
                "1481w9g1dw9rxp3l6snkdqihzyrd2f8vispzqmwjwsdyhw8xzggi"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-compare-0.2.0
  (crate-source "version-compare" "0.2.0"
                "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasi-0.14.7+wasi-0.2.4
  (crate-source "wasi" "0.14.7+wasi-0.2.4"
                "133fq3mq7h65mzrsphcm7bbbx1gsz7srrbwh01624zin43g7hd48"))

(define rust-wasip2-1.0.1+wasi-0.2.4
  (crate-source "wasip2" "1.0.1+wasi-0.2.4"
                "1rsqmpspwy0zja82xx7kbkbg9fv34a4a2if3sbd76dy64a244qh5"))

(define rust-wasm-bindgen-0.2.104
  (crate-source "wasm-bindgen" "0.2.104"
                "0b8f4l6pqm0bz0lj5xgwmchb6977n71vmh7srd0axwg93b011nn1"))

(define rust-wasm-bindgen-backend-0.2.104
  (crate-source "wasm-bindgen-backend" "0.2.104"
                "069vnhhn2j4w2gwd8rch6g8d3iwkrgi45fas6i3qm7glcrd9l737"))

(define rust-wasm-bindgen-macro-0.2.104
  (crate-source "wasm-bindgen-macro" "0.2.104"
                "06d1m5bg272h6jabq0snm7c50fifjz6r20f5hqlmz7y5wivh99kw"))

(define rust-wasm-bindgen-macro-support-0.2.104
  (crate-source "wasm-bindgen-macro-support" "0.2.104"
                "1mr18kx7ima1pmsqlkk982q4a0vf3r8s1x6901jb59sd1prd41wz"))

(define rust-wasm-bindgen-shared-0.2.104
  (crate-source "wasm-bindgen-shared" "0.2.104"
                "1la1xj9v3gmawnlyi7lc3mb3xi447r6frb98hi2fb9m1nb47vmms"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-core-0.62.1
  (crate-source "windows-core" "0.62.1"
                "1aa94x61q0x39xnlzxjmahwck9i5p51xgzrz7m6hi1dj2rafwi38"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-implement-0.60.1
  (crate-source "windows-implement" "0.60.1"
                "1q2lfwdqrkfzsrlshvvyr2cj7ckq4rqxj0ispzlnvyvl5bj0gczd"))

(define rust-windows-interface-0.59.2
  (crate-source "windows-interface" "0.59.2"
                "19a6if8dfnazjgjw4hm0kayk9vrjclyj3iqivcaaqr39pkfx3ay0"))

(define rust-windows-link-0.2.0
  (crate-source "windows-link" "0.2.0"
                "0r9w2z96d5phmm185aq92z54jp9h2nqisa4wgc71idxbc436rr25"))

(define rust-windows-result-0.4.0
  (crate-source "windows-result" "0.4.0"
                "0zqn8kmmf7y9yw9g7q6pbcg9dbry9m03fqi0b92q767q0v1xr13h"))

(define rust-windows-strings-0.5.0
  (crate-source "windows-strings" "0.5.0"
                "1nld65azvms87rdm2bdm8gskwdmsswh4pxbc8babxc2klmawc63j"))

(define rust-windows-sys-0.48.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.61.1
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.61.1"
                "03vg2rxm0lyiyq64b5sm95lkg2x95sjdb0zb0y4q8g2avm0rw43g"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-winnow-0.7.13
  (crate-source "winnow" "0.7.13"
                "1krrjc1wj2vx0r57m9nwnlc1zrhga3fq41d8w9hysvvqb5mj7811"))

(define rust-wit-bindgen-0.46.0
  (crate-source "wit-bindgen" "0.46.0"
                "0ngysw50gp2wrrfxbwgp6dhw1g6sckknsn3wm7l00vaf7n48aypi"))

(define rust-x11rb-0.13.2
  (crate-source "x11rb" "0.13.2"
                "053lvnaw9ycbl791mgwly2hw27q6vqgzrb1y5kz1as52wmdsm4wr"))

(define rust-x11rb-protocol-0.13.2
  (crate-source "x11rb-protocol" "0.13.2"
                "1g81cznbyn522b0fbis0i44wh3adad2vhsz5pzf99waf3sbc4vza"))

(define rust-xml-rs-0.8.27
  (crate-source "xml-rs" "0.8.27"
                "1irplg223x6w3lvj0yig6czbiwci06495wc9xg3660kh6cvl1n3g"))

(define rust-zerocopy-0.8.27
  (crate-source "zerocopy" "0.8.27"
                "0b1870gf2zzlckca69v2k4mqwmf8yh2li37qldnzvvd3by58g508"))

(define rust-zerocopy-derive-0.8.27
  (crate-source "zerocopy-derive" "0.8.27"
                "0c9qrylm2p55dvaplxsl24ma48add9qk4y0d6kjbkllaqvcvill8"))

(define rust-zstd-0.13.3
  (crate-source "zstd" "0.13.3"
                "12n0h4w9l526li7jl972rxpyf012jw3nwmji2qbjghv9ll8y67p9"))

(define rust-zstd-safe-7.2.4
  (crate-source "zstd-safe" "7.2.4"
                "179vxmkzhpz6cq6mfzvgwc99bpgllkr6lwxq7ylh5dmby3aw8jcg"))

(define rust-zstd-sys-2.0.16+zstd.1.5.7
  ;; TODO: Check bundled sources.
  (crate-source "zstd-sys" "2.0.16+zstd.1.5.7"
                "0j1pd2iaqpvaxlgqmmijj68wma7xwdv9grrr63j873yw5ay9xqci"))

(define ssss-separator 'end-of-crates)


;;;
;;; Cargo inputs.
;;;

(define-cargo-inputs lookup-cargo-inputs
                     (runst =>
                            (list rust-adler32-1.2.0
                                  rust-ahash-0.8.12
                                  rust-aho-corasick-1.1.3
                                  rust-allocator-api2-0.2.21
                                  rust-android-system-properties-0.1.5
                                  rust-ansi-term-0.12.1
                                  rust-as-raw-xcb-connection-1.0.1
                                  rust-atty-0.2.14
                                  rust-autocfg-1.5.0
                                  rust-bitflags-1.3.2
                                  rust-bitflags-2.9.4
                                  rust-block-buffer-0.10.4
                                  rust-bstr-1.12.0
                                  rust-bumpalo-3.19.0
                                  rust-cairo-rs-0.19.4
                                  rust-cairo-sys-rs-0.19.2
                                  rust-cc-1.2.39
                                  rust-cfg-expr-0.15.8
                                  rust-cfg-if-1.0.3
                                  rust-chrono-0.4.42
                                  rust-chrono-tz-0.9.0
                                  rust-chrono-tz-build-0.3.0
                                  rust-clap-2.34.0
                                  rust-colorsys-0.6.7
                                  rust-const-format-0.2.31
                                  rust-const-format-proc-macros-0.2.31
                                  rust-convert-case-0.6.0
                                  rust-core-foundation-sys-0.8.7
                                  rust-core2-0.4.0
                                  rust-cpufeatures-0.2.17
                                  rust-crc32fast-1.5.0
                                  rust-crossbeam-deque-0.8.6
                                  rust-crossbeam-epoch-0.9.18
                                  rust-crossbeam-utils-0.8.21
                                  rust-crypto-common-0.1.6
                                  rust-dary-heap-0.3.8
                                  rust-dbus-0.9.9
                                  rust-dbus-codegen-0.11.0
                                  rust-dbus-crossroads-0.5.2
                                  rust-deunicode-1.6.2
                                  rust-digest-0.10.7
                                  rust-dirs-5.0.1
                                  rust-dirs-sys-0.4.1
                                  rust-equivalent-1.0.2
                                  rust-errno-0.3.14
                                  rust-estimated-read-time-1.0.0
                                  rust-find-msvc-tools-0.1.2
                                  rust-futures-channel-0.3.31
                                  rust-futures-core-0.3.31
                                  rust-futures-executor-0.3.31
                                  rust-futures-io-0.3.31
                                  rust-futures-macro-0.3.31
                                  rust-futures-task-0.3.31
                                  rust-futures-util-0.3.31
                                  rust-generic-array-0.14.7
                                  rust-gethostname-1.0.2
                                  rust-getrandom-0.2.16
                                  rust-getrandom-0.3.3
                                  rust-gio-0.19.8
                                  rust-gio-sys-0.19.8
                                  rust-glib-0.19.9
                                  rust-glib-macros-0.19.9
                                  rust-glib-sys-0.19.8
                                  rust-globset-0.4.16
                                  rust-globwalk-0.9.1
                                  rust-gobject-sys-0.19.8
                                  rust-hashbrown-0.14.5
                                  rust-hashbrown-0.16.0
                                  rust-heck-0.5.0
                                  rust-hermit-abi-0.1.19
                                  rust-humansize-2.1.3
                                  rust-humantime-2.3.0
                                  rust-iana-time-zone-0.1.64
                                  rust-iana-time-zone-haiku-0.1.2
                                  rust-ignore-0.4.23
                                  rust-include-flate-0.3.1
                                  rust-include-flate-codegen-0.3.1
                                  rust-include-flate-compress-0.3.1
                                  rust-indexmap-2.11.4
                                  rust-itoa-1.0.15
                                  rust-jobserver-0.1.34
                                  rust-js-sys-0.3.81
                                  rust-lazy-static-1.5.0
                                  rust-libc-0.2.176
                                  rust-libdbus-sys-0.2.6
                                  rust-libflate-2.1.0
                                  rust-libflate-lz77-2.1.0
                                  rust-libm-0.2.15
                                  rust-libredox-0.1.10
                                  rust-linux-raw-sys-0.11.0
                                  rust-log-0.4.28
                                  rust-matchers-0.1.0
                                  rust-memchr-2.7.6
                                  rust-nu-ansi-term-0.46.0
                                  rust-num-traits-0.2.19
                                  rust-once-cell-1.21.3
                                  rust-option-ext-0.2.0
                                  rust-overload-0.1.1
                                  rust-pango-0.19.8
                                  rust-pango-sys-0.19.8
                                  rust-pangocairo-0.19.8
                                  rust-pangocairo-sys-0.19.8
                                  rust-parse-zoneinfo-0.3.1
                                  rust-percent-encoding-2.3.2
                                  rust-pest-2.8.2
                                  rust-pest-derive-2.8.2
                                  rust-pest-generator-2.8.2
                                  rust-pest-meta-2.8.2
                                  rust-phf-0.11.3
                                  rust-phf-codegen-0.11.3
                                  rust-phf-generator-0.11.3
                                  rust-phf-shared-0.11.3
                                  rust-pin-project-lite-0.2.16
                                  rust-pin-utils-0.1.0
                                  rust-pkg-config-0.3.32
                                  rust-ppv-lite86-0.2.21
                                  rust-proc-macro-crate-3.4.0
                                  rust-proc-macro-error-1.0.4
                                  rust-proc-macro-error-attr-1.0.4
                                  rust-proc-macro2-1.0.101
                                  rust-quote-1.0.40
                                  rust-r-efi-5.3.0
                                  rust-rand-0.8.5
                                  rust-rand-chacha-0.3.1
                                  rust-rand-core-0.6.4
                                  rust-redox-users-0.4.6
                                  rust-regex-1.11.3
                                  rust-regex-automata-0.1.10
                                  rust-regex-automata-0.4.11
                                  rust-regex-syntax-0.6.29
                                  rust-regex-syntax-0.8.6
                                  rust-rle-decode-fast-1.0.3
                                  rust-rust-embed-8.7.2
                                  rust-rust-embed-impl-8.7.2
                                  rust-rust-embed-utils-8.7.2
                                  rust-rustix-1.1.2
                                  rust-rustversion-1.0.22
                                  rust-ryu-1.0.20
                                  rust-same-file-1.0.6
                                  rust-serde-1.0.227
                                  rust-serde-core-1.0.227
                                  rust-serde-derive-1.0.227
                                  rust-serde-json-1.0.145
                                  rust-serde-regex-1.1.0
                                  rust-serde-spanned-0.6.9
                                  rust-sha2-0.10.9
                                  rust-sharded-slab-0.1.7
                                  rust-shlex-1.3.0
                                  rust-siphasher-1.0.1
                                  rust-slab-0.4.11
                                  rust-slug-0.1.6
                                  rust-smallvec-1.15.1
                                  rust-sscanf-0.4.3
                                  rust-sscanf-macro-0.4.3
                                  rust-strsim-0.8.0
                                  rust-strsim-0.11.1
                                  rust-syn-1.0.109
                                  rust-syn-2.0.106
                                  rust-system-deps-6.2.2
                                  rust-target-lexicon-0.12.16
                                  rust-tera-1.20.0
                                  rust-textwrap-0.11.0
                                  rust-thiserror-1.0.69
                                  rust-thiserror-2.0.16
                                  rust-thiserror-impl-1.0.69
                                  rust-thiserror-impl-2.0.16
                                  rust-thread-local-1.1.9
                                  rust-toml-0.8.23
                                  rust-toml-datetime-0.6.11
                                  rust-toml-datetime-0.7.2
                                  rust-toml-edit-0.22.27
                                  rust-toml-edit-0.23.6
                                  rust-toml-parser-1.0.3
                                  rust-toml-write-0.1.2
                                  rust-tracing-0.1.40
                                  rust-tracing-attributes-0.1.30
                                  rust-tracing-core-0.1.34
                                  rust-tracing-log-0.2.0
                                  rust-tracing-subscriber-0.3.18
                                  rust-typenum-1.18.0
                                  rust-ucd-trie-0.1.7
                                  rust-unic-char-property-0.9.0
                                  rust-unic-char-range-0.9.0
                                  rust-unic-common-0.9.0
                                  rust-unic-segment-0.9.0
                                  rust-unic-ucd-segment-0.9.0
                                  rust-unic-ucd-version-0.9.0
                                  rust-unicode-ident-1.0.19
                                  rust-unicode-segmentation-1.12.0
                                  rust-unicode-width-0.1.12
                                  rust-unicode-xid-0.2.6
                                  rust-valuable-0.1.1
                                  rust-vec-map-0.8.2
                                  rust-version-compare-0.2.0
                                  rust-version-check-0.9.5
                                  rust-walkdir-2.5.0
                                  rust-wasi-0.11.1+wasi-snapshot-preview1
                                  rust-wasi-0.14.7+wasi-0.2.4
                                  rust-wasip2-1.0.1+wasi-0.2.4
                                  rust-wasm-bindgen-0.2.104
                                  rust-wasm-bindgen-backend-0.2.104
                                  rust-wasm-bindgen-macro-0.2.104
                                  rust-wasm-bindgen-macro-support-0.2.104
                                  rust-wasm-bindgen-shared-0.2.104
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-util-0.1.11
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-core-0.62.1
                                  rust-windows-implement-0.60.1
                                  rust-windows-interface-0.59.2
                                  rust-windows-link-0.2.0
                                  rust-windows-result-0.4.0
                                  rust-windows-strings-0.5.0
                                  rust-windows-sys-0.48.0
                                  rust-windows-sys-0.52.0
                                  rust-windows-sys-0.59.0
                                  rust-windows-sys-0.61.1
                                  rust-windows-targets-0.48.5
                                  rust-windows-targets-0.52.6
                                  rust-windows-aarch64-gnullvm-0.48.5
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-msvc-0.48.5
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-i686-gnu-0.48.5
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-msvc-0.48.5
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-x86-64-gnu-0.48.5
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnullvm-0.48.5
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-msvc-0.48.5
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-winnow-0.7.13
                                  rust-wit-bindgen-0.46.0
                                  rust-x11rb-0.13.2
                                  rust-x11rb-protocol-0.13.2
                                  rust-xml-rs-0.8.27
                                  rust-zerocopy-0.8.27
                                  rust-zerocopy-derive-0.8.27
                                  rust-zstd-0.13.3
                                  rust-zstd-safe-7.2.4
                                  rust-zstd-sys-2.0.16+zstd.1.5.7)))
