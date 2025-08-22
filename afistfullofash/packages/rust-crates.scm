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

(define rust-ahash-0.8.11
  (crate-source "ahash" "0.8.11"
                "04chdfkls5xmhp1d48gnjsmglbqibizs3bpbj6rsj604m10si7g8"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-allocator-api2-0.2.18
  (crate-source "allocator-api2" "0.2.18"
                "0kr6lfnxvnj164j1x38g97qjlhb7akppqzvgfs0697140ixbav2w"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-android-tzdata-0.1.1
  (crate-source "android-tzdata" "0.1.1"
                "1w7ynjxrfs97xg3qlcdns4kgfpwcdv824g611fq32cag4cdr96g9"))

(define rust-anstream-0.6.14
  (crate-source "anstream" "0.6.14"
                "0nx1vnfs2lil1sl14l49i6jvp6zpjczn85wxx4xw1ycafvx7b321"))

(define rust-anstyle-1.0.7
  (crate-source "anstyle" "1.0.7"
                "06qxmrba0xbhv07jpdvrdrhw1hjlb9icj88bqvlnissz9bqgr383"))

(define rust-anstyle-parse-0.2.4
  (crate-source "anstyle-parse" "0.2.4"
                "1m121pa4plpcb4g7xali2kv9njmgb3713q3fxf60b4jd0fli2fn0"))

(define rust-anstyle-query-1.0.3
  (crate-source "anstyle-query" "1.0.3"
                "1x9pyl231rry5g45dvkdb2sfnl2dx2f4qd9a5v3ml8kr9ryr0k56"))

(define rust-anstyle-wincon-3.0.3
  (crate-source "anstyle-wincon" "3.0.3"
                "06gv2vbj4hvwb8fxqjmvabp5kx2w01cjgh86pd98y1mpzr4q98v1"))

(define rust-as-raw-xcb-connection-1.0.1
  (crate-source "as-raw-xcb-connection" "1.0.1"
                "0sqgpz2ymv5yx76r5j2npjq2x5qvvqnw0vrs35cyv30p3pfp2m8p"))

(define rust-autocfg-1.2.0
  (crate-source "autocfg" "1.2.0"
                "102c77is3pii4rsqfsc5vrbk6qabjy0yqc0gwqzmjjb9fp3spzgi"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.6.0
  (crate-source "bitflags" "2.6.0"
                "1pkidwzn3hnxlsl8zizh0bncgbjnw7c41cx7bby26ncbzmiznj5h"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-bstr-1.9.1
  (crate-source "bstr" "1.9.1"
                "01ipr5rncw3kf4dyc1p2g00njn1df2b0xpviwhb8830iv77wbvq5"))

(define rust-bumpalo-3.15.4
  (crate-source "bumpalo" "3.15.4"
                "1ahfhgw2lzlgv5j0h07z8mkdnk4kvl2grf8dkb32dm4zsjfrpxkz"))

(define rust-cairo-rs-0.20.10
  (crate-source "cairo-rs" "0.20.10"
                "0m9z0hackmqni1zfkjwp2ha46v3yvbyk3fw27zvb6byhgji653mm"))

(define rust-cairo-sys-rs-0.20.0
  (crate-source "cairo-sys-rs" "0.20.0"
                "13x85l52nl2izmdb48zzpkhhh1a4dsgqlp8gys4n1f5r2kwr10j2"))

(define rust-cc-1.0.90
  (crate-source "cc" "1.0.90"
                "1xg1bqnq50dpf6g1hl90caxgz4afnf74pxa426gh7wxch9561mlc"))

(define rust-cfg-expr-0.15.7
  (crate-source "cfg-expr" "0.15.7"
                "07cny7rg58mlzmzw2b1x5b6ib1za9647gklksnlzv9m9cj5qcl7s"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-chrono-0.4.35
  (crate-source "chrono" "0.4.35"
                "16k3caxzip1ql827pz5rj7aqfqy7yhpxyxzb5wqkj2mwvh1mkbwf"))

(define rust-chrono-tz-0.9.0
  (crate-source "chrono-tz" "0.9.0"
                "1fvicqrlmdsjkrgxr7bxfd62i9w2qi2b6iv4w85av5syvqlqnsck"))

(define rust-chrono-tz-build-0.3.0
  (crate-source "chrono-tz-build" "0.3.0"
                "1c8ixwwwsn9kgs1dr5mz963p0fgw9j9p7fzb3w2c7y8xhkp8l20c"))

(define rust-clap-4.5.4
  (crate-source "clap" "4.5.4"
                "1828wm9qws5gh2xnimnvmp2vria6d6hsxnqmhnm84dwjcxm0dg4h"))

(define rust-clap-builder-4.5.2
  (crate-source "clap_builder" "4.5.2"
                "1d7p4hph4fyhaphkf0v5zv0kq4lz25a9jq2f901yrq3afqp9w4mf"))

(define rust-clap-lex-0.7.0
  (crate-source "clap_lex" "0.7.0"
                "1kh1sckgq71kay2rrr149pl9gbsrvyccsq6xm5xpnq0cxnyqzk4q"))

(define rust-colorchoice-1.0.1
  (crate-source "colorchoice" "1.0.1"
                "08h4jsrd2j5k6lp1b9v5p1f1g7cmyzm4djsvb3ydywdb4hmqashb"))

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

(define rust-core-foundation-sys-0.8.6
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.6"
                "13w6sdf06r0hn7bx2b45zxsg1mm2phz34jikm6xc5qrbr6djpsh6"))

(define rust-core2-0.4.0
  (crate-source "core2" "0.4.0"
                "01f5xv0kf3ds3xm7byg78hycbanb8zlpvsfv4j47y46n3bpsg6xl"))

(define rust-cpufeatures-0.2.12
  (crate-source "cpufeatures" "0.2.12"
                "012m7rrak4girqlii3jnqwrr73gv1i980q4wra5yyyhvzwk5xzjk"))

(define rust-crc32fast-1.4.0
  (crate-source "crc32fast" "1.4.0"
                "1ahy259ypc955l5ak24hdlgllb6vm6y2pvwr6qrlyisbg255m1dk"))

(define rust-crossbeam-deque-0.8.5
  (crate-source "crossbeam-deque" "0.8.5"
                "03bp38ljx4wj6vvy4fbhx41q8f585zyqix6pncz1mkz93z08qgv1"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-utils-0.8.19
  (crate-source "crossbeam-utils" "0.8.19"
                "0iakrb1b8fjqrag7wphl94d10irhbh2fw1g444xslsywqyn3p3i4"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-dary-heap-0.3.6
  (crate-source "dary_heap" "0.3.6"
                "1jm04p72s7xij3cr71h59dw07s63nah5b10sh8akcr2129zx2qkp"))

(define rust-dbus-0.9.7
  (crate-source "dbus" "0.9.7"
                "06vdv4aarjs4w6byg9nqajr67c8qvlhk3153ic2i65pvp63ikchv"))

(define rust-dbus-codegen-0.12.0
  (crate-source "dbus-codegen" "0.12.0"
                "1nx1fbfrdy7sl0iyl4ybsdk7siz5qn36m65dnw5ypli0w1w8qyyg"))

(define rust-dbus-crossroads-0.5.2
  (crate-source "dbus-crossroads" "0.5.2"
                "1q3dyywazr3hppm052fa8q2366q66ml789r42jjlnm47f51q6k1s"))

(define rust-deunicode-1.4.3
  (crate-source "deunicode" "1.4.3"
                "156zkxmlfm1qg84x8jaqbj3mpc86lzwqiv0xr2w9di2ncw959s5n"))

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

(define rust-equivalent-1.0.1
  (crate-source "equivalent" "1.0.1"
                "1malmx5f4lkfvqasz319lq6gb3ddg19yzf9s8cykfsgzdmyq0hsl"))

(define rust-errno-0.3.8
  (crate-source "errno" "0.3.8"
                "0ia28ylfsp36i27g1qih875cyyy4by2grf80ki8vhgh6vinf8n52"))

(define rust-estimated-read-time-1.0.0
  (crate-source "estimated_read_time" "1.0.0"
                "1mz8pkgk9v0cfzfjw659zl997gilangb78ccds8gic8h2hsgv734"))

(define rust-futures-channel-0.3.30
  (crate-source "futures-channel" "0.3.30"
                "0y6b7xxqdjm9hlcjpakcg41qfl7lihf6gavk8fyqijsxhvbzgj7a"))

(define rust-futures-core-0.3.30
  (crate-source "futures-core" "0.3.30"
                "07aslayrn3lbggj54kci0ishmd1pr367fp7iks7adia1p05miinz"))

(define rust-futures-executor-0.3.30
  (crate-source "futures-executor" "0.3.30"
                "07dh08gs9vfll2h36kq32q9xd86xm6lyl9xikmmwlkqnmrrgqxm5"))

(define rust-futures-io-0.3.30
  (crate-source "futures-io" "0.3.30"
                "1hgh25isvsr4ybibywhr4dpys8mjnscw4wfxxwca70cn1gi26im4"))

(define rust-futures-macro-0.3.30
  (crate-source "futures-macro" "0.3.30"
                "1b49qh9d402y8nka4q6wvvj0c88qq91wbr192mdn5h54nzs0qxc7"))

(define rust-futures-task-0.3.30
  (crate-source "futures-task" "0.3.30"
                "013h1724454hj8qczp8vvs10qfiqrxr937qsrv6rhii68ahlzn1q"))

(define rust-futures-util-0.3.30
  (crate-source "futures-util" "0.3.30"
                "0j0xqhcir1zf2dcbpd421kgw6wvsk0rpxflylcysn1rlp3g02r1x"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-gethostname-0.4.3
  (crate-source "gethostname" "0.4.3"
                "063qqhznyckwx9n4z4xrmdv10s0fi6kbr17r6bi1yjifki2y0xh1"))

(define rust-getrandom-0.2.12
  (crate-source "getrandom" "0.2.12"
                "1d8jb9bv38nkwlqqdjcav6gxckgwc9g30pm3qq506rvncpm9400r"))

(define rust-gio-0.20.0
  (crate-source "gio" "0.20.0"
                "1qn6aagg82j6z1s7g36v8vwrqp7nqcg55xybhckw7za9hyk3v3ir"))

(define rust-gio-sys-0.20.5
  ;; TODO: Check bundled sources.
  (crate-source "gio-sys" "0.20.5"
                "0yvpmr9f74a8rf1i0q10i708id0n5mg1amf3d51swijrmm64czr1"))

(define rust-glib-0.20.5
  (crate-source "glib" "0.20.5"
                "133nc6vym2z87pmrs2b8q72m7f85r0ciybmm5p8bj5gbw2q3311m"))

(define rust-glib-macros-0.20.5
  (crate-source "glib-macros" "0.20.5"
                "0mn1054ypcw906qpgvkqdz1wfjjb2kgfsmh4lwfykhyggai1rlp7"))

(define rust-glib-sys-0.20.5
  ;; TODO: Check bundled sources.
  (crate-source "glib-sys" "0.20.5"
                "12d399a45rvpv1kb0jcwcqinn1xsxq96z5ymz2kd9v5p7a312nca"))

(define rust-globset-0.4.14
  (crate-source "globset" "0.4.14"
                "1qab0c1drpybgm4nc92lf8b46x0ap44c9y4k23rndgc5bfdkpnjp"))

(define rust-globwalk-0.9.1
  (crate-source "globwalk" "0.9.1"
                "0mz7bsa66p2rrgnz3l94ac4kbklh7mq8j30iizyxjy4qyvmn1xqb"))

(define rust-gobject-sys-0.20.0
  ;; TODO: Check bundled sources.
  (crate-source "gobject-sys" "0.20.0"
                "1f0337v7la5raq2fsdwkjjdz811rhr307sgpdnsibzzzymj8i466"))

(define rust-hashbrown-0.14.3
  (crate-source "hashbrown" "0.14.3"
                "012nywlg0lj9kwanh69my5x67vjlfmzfi9a0rq4qvis2j8fil3r9"))

(define rust-hashbrown-0.15.2
  (crate-source "hashbrown" "0.15.2"
                "12dj0yfn59p3kh3679ac0w1fagvzf4z2zp87a13gbbqbzw0185dz"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-humansize-2.1.3
  (crate-source "humansize" "2.1.3"
                "1msxd1akb3dydsa8qs461sds9krwnn31szvqgaq93p4x0ad1rdbc"))

(define rust-humantime-2.2.0
  (crate-source "humantime" "2.2.0"
                "17rz8jhh1mcv4b03wnknhv1shwq2v9vhkhlfg884pprsig62l4cv"))

(define rust-iana-time-zone-0.1.60
  (crate-source "iana-time-zone" "0.1.60"
                "0hdid5xz3jznm04lysjm3vi93h3c523w0hcc3xba47jl3ddbpzz7"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-ignore-0.4.22
  (crate-source "ignore" "0.4.22"
                "1wcaqpi6djqgi1brghrdyw4d5qgnwzhqrqyn4mar4vp677gi0s5l"))

(define rust-include-flate-0.3.0
  (crate-source "include-flate" "0.3.0"
                "0ghiy1f2ipzw71nggrv6chard023nyjmvq2dyg0qcm39a1kw2jfz"))

(define rust-include-flate-codegen-0.2.0
  (crate-source "include-flate-codegen" "0.2.0"
                "1mz7zyalpn1f53zy12xnbjis93jfh23wy7n6hlxp5gk1c9n28nwc"))

(define rust-indexmap-2.7.1
  (crate-source "indexmap" "2.7.1"
                "0lmnm1zbr5gq3wic3d8a76gpvampridzwckfl97ckd5m08mrk74c"))

(define rust-is-terminal-polyfill-1.70.0
  (crate-source "is_terminal_polyfill" "1.70.0"
                "0018q5cf3rifbnzfc1w1z1xcx9c6i7xlywp2n0fw4limq1vqaizq"))

(define rust-itoa-1.0.11
  (crate-source "itoa" "1.0.11"
                "0nv9cqjwzr3q58qz84dcz63ggc54yhf1yqar1m858m1kfd4g3wa9"))

(define rust-js-sys-0.3.69
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.69"
                "0v99rz97asnzapb0jsc3jjhvxpfxr7h7qd97yqyrf9i7viimbh99"))

(define rust-lazy-static-1.4.0
  (crate-source "lazy_static" "1.4.0"
                "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))

(define rust-libc-0.2.153
  (crate-source "libc" "0.2.153"
                "1gg7m1ils5dms5miq9fyllrcp0jxnbpgkx71chd2i0lafa8qy6cw"))

(define rust-libdbus-sys-0.2.5
  ;; TODO: Check bundled sources.
  (crate-source "libdbus-sys" "0.2.5"
                "0wjw93q6ckrn8qdrxzdi02f0ma9g7nnlpgkrkcll1mjhnw95a206"))

(define rust-libflate-2.1.0
  (crate-source "libflate" "2.1.0"
                "07mj9z89vbhq837q58m4v2nblgsmrn6vrp8w1j8g0kpa2kfdzna5"))

(define rust-libflate-lz77-2.1.0
  (crate-source "libflate_lz77" "2.1.0"
                "0gc6h98jwigscasz8vw1vv65b3rismqcbndb8hf6yf4z6qxxgq76"))

(define rust-libm-0.2.8
  (crate-source "libm" "0.2.8"
                "0n4hk1rs8pzw8hdfmwn96c4568s93kfxqgcqswr7sajd2diaihjf"))

(define rust-libredox-0.0.1
  (crate-source "libredox" "0.0.1"
                "1s2fh4ikpp9xl0lsl01pi0n8pw1q9s3ld452vd8qh1v63v537j45"))

(define rust-linux-raw-sys-0.4.13
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.13"
                "172k2c6422gsc914ig8rh99mb9yc7siw6ikc3d9xw1k7vx0s3k81"))

(define rust-log-0.4.21
  (crate-source "log" "0.4.21"
                "074hldq1q8rlzq2s2qa8f25hj4s3gpw71w64vdwzjd01a4g8rvch"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-num-traits-0.2.18
  (crate-source "num-traits" "0.2.18"
                "0yjib8p2p9kzmaz48xwhs69w5dh1wipph9jgnillzd2x33jz03fs"))

(define rust-once-cell-1.19.0
  (crate-source "once_cell" "1.19.0"
                "14kvw7px5z96dk4dwdm1r9cqhhy2cyj1l5n5b29mynbb8yr15nrz"))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"))

(define rust-pango-0.20.10
  (crate-source "pango" "0.20.10"
                "0f1902vsnhb6jprra2c0mhsr6k28d00pync2jc6qz17jc70kg3fq"))

(define rust-pango-sys-0.20.0
  ;; TODO: Check bundled sources.
  (crate-source "pango-sys" "0.20.0"
                "141kj535jllhlgjqg3p0z962z371rq16k2ki3xkfrr6f21ywaz5h"))

(define rust-pangocairo-0.20.10
  (crate-source "pangocairo" "0.20.10"
                "0kyqga9i164d1kzq2lma68wln2iplh1zjx485nn696fva720v2aq"))

(define rust-pangocairo-sys-0.20.0
  ;; TODO: Check bundled sources.
  (crate-source "pangocairo-sys" "0.20.0"
                "0lsqj70qn8b514qs56n5kv4i16vr9jk4d14gb6qrlw3ffpmaa8xw"))

(define rust-parse-zoneinfo-0.3.0
  (crate-source "parse-zoneinfo" "0.3.0"
                "0h8g6jy4kckn2gk8sd5adaws180n1ip65xhzw5jxlq4w8ibg41f7"))

(define rust-percent-encoding-2.3.1
  (crate-source "percent-encoding" "2.3.1"
                "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))

(define rust-pest-2.7.8
  (crate-source "pest" "2.7.8"
                "1a77a6v1dna7ba6hp69q65r8mrinzbrwg8afg01qx35p1wyh5y2n"))

(define rust-pest-derive-2.7.8
  (crate-source "pest_derive" "2.7.8"
                "09hhvpbnfkng57zfh0mrz1rsmadnrlrvqf2pmjv6ml9z75r4zlmh"))

(define rust-pest-generator-2.7.8
  (crate-source "pest_generator" "2.7.8"
                "105b6p56smpgbf6zgbxv101jh5yccsys9msq04plj2kxdhm7xhgx"))

(define rust-pest-meta-2.7.8
  (crate-source "pest_meta" "2.7.8"
                "14z2b94sqmbi2302yp1lp6zzy4b7bxfq6via6ms4c3q53iixfk4k"))

(define rust-phf-0.11.2
  (crate-source "phf" "0.11.2"
                "1p03rsw66l7naqhpgr1a34r9yzi1gv9jh16g3fsk6wrwyfwdiqmd"))

(define rust-phf-codegen-0.11.2
  (crate-source "phf_codegen" "0.11.2"
                "0nia6h4qfwaypvfch3pnq1nd2qj64dif4a6kai3b7rjrsf49dlz8"))

(define rust-phf-generator-0.11.2
  (crate-source "phf_generator" "0.11.2"
                "1c14pjyxbcpwkdgw109f7581cc5fa3fnkzdq1ikvx7mdq9jcrr28"))

(define rust-phf-shared-0.11.2
  (crate-source "phf_shared" "0.11.2"
                "0azphb0a330ypqx3qvyffal5saqnks0xvl8rj73jlk3qxxgbkz4h"))

(define rust-pin-project-lite-0.2.13
  (crate-source "pin-project-lite" "0.2.13"
                "0n0bwr5qxlf0mhn2xkl36sy55118s9qmvx2yl5f3ixkb007lbywa"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-pkg-config-0.3.30
  (crate-source "pkg-config" "0.3.30"
                "1v07557dj1sa0aly9c90wsygc0i8xv5vnmyv0g94lpkvj8qb4cfj"))

(define rust-ppv-lite86-0.2.17
  (crate-source "ppv-lite86" "0.2.17"
                "1pp6g52aw970adv3x2310n7glqnji96z0a9wiamzw89ibf0ayh2v"))

(define rust-proc-macro-crate-3.1.0
  (crate-source "proc-macro-crate" "3.1.0"
                "110jcl9vnj92ihbhjqmkp19m8rzxc14a7i60knlmv99qlwfcadvd"))

(define rust-proc-macro2-1.0.86
  (crate-source "proc-macro2" "1.0.86"
                "0xrv22p8lqlfdf1w0pj4si8n2ws4aw0kilmziwf0vpv5ys6rwway"))

(define rust-quote-1.0.35
  (crate-source "quote" "1.0.35"
                "1vv8r2ncaz4pqdr78x7f138ka595sp2ncr1sa2plm4zxbsmwj7i9"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-redox-syscall-0.4.1
  (crate-source "redox_syscall" "0.4.1"
                "1aiifyz5dnybfvkk4cdab9p2kmphag1yad6iknc7aszlxxldf8j7"))

(define rust-redox-users-0.4.4
  (crate-source "redox_users" "0.4.4"
                "1d1c7dhbb62sh8jrq9dhvqcyxqsh3wg8qknsi94iwq3r0wh7k151"))

(define rust-regex-1.11.1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.4.8
  (crate-source "regex-automata" "0.4.8"
                "18wd530ndrmygi6xnz3sp345qi0hy2kdbsa89182nwbl6br5i1rn"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

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

(define rust-rustix-0.38.32
  (crate-source "rustix" "0.38.32"
                "12fvzwnsb13svnqzsf01maz44dib8kmgp2w8cxp7f8azwrhliq35"))

(define rust-ryu-1.0.17
  (crate-source "ryu" "1.0.17"
                "188vrsh3zlnl5xl7lw0rp2sc0knpx8yaqpwvr648b6h12v4rfrp8"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-serde-1.0.219
  (crate-source "serde" "1.0.219"
                "1dl6nyxnsi82a197sd752128a4avm6mxnscywas1jq30srp2q3jz"))

(define rust-serde-derive-1.0.219
  (crate-source "serde_derive" "1.0.219"
                "001azhjmj7ya52pmfiw4ppxm16nd44y15j2pf5gkcwrcgz7pc0jv"))

(define rust-serde-json-1.0.140
  (crate-source "serde_json" "1.0.140"
                "0wwkp4vc20r87081ihj3vpyz5qf7wqkqipq17v99nv6wjrp8n1i0"))

(define rust-serde-regex-1.1.0
  (crate-source "serde_regex" "1.1.0"
                "1pxsnxb8c198szghk1hvzvhva36w2q5zs70hqkmdf5d89qd6y4x8"))

(define rust-serde-spanned-0.6.9
  (crate-source "serde_spanned" "0.6.9"
                "18vmxq6qfrm110caszxrzibjhy2s54n1g5w1bshxq9kjmz7y0hdz"))

(define rust-sha2-0.10.8
  (crate-source "sha2" "0.10.8"
                "1j1x78zk9il95w9iv46dh9wm73r6xrgj32y6lzzw7bxws9dbfgbr"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-siphasher-0.3.11
  (crate-source "siphasher" "0.3.11"
                "03axamhmwsrmh0psdw3gf7c0zc4fyl5yjxfifz9qfka6yhkqid9q"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-slug-0.1.5
  (crate-source "slug" "0.1.5"
                "1i68hkvpbf04ga5kcssyads2wdy0kyikbqgq0l069nn8r774mn9v"))

(define rust-smallvec-1.13.2
  (crate-source "smallvec" "1.13.2"
                "0rsw5samawl3wsw6glrsb127rx6sh89a8wyikicw6dkdcjd1lpiw"))

(define rust-sscanf-0.4.3
  (crate-source "sscanf" "0.4.3"
                "1w6lfy9sr1fh1ar3k68wjyscc9kpdi4ngygwixf0613aafdh1lfb"))

(define rust-sscanf-macro-0.4.3
  (crate-source "sscanf_macro" "0.4.3"
                "0dqsrabv6zmphzm0ssrq3h07gq67ccrp7kvn4kdbqjsp19iy1z6g"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-syn-2.0.87
  (crate-source "syn" "2.0.87"
                "0bd3mfcswvn4jkrp7ich5kk58kmpph8412yxd36nsfnh8vilrai5"))

(define rust-system-deps-7.0.1
  (crate-source "system-deps" "7.0.1"
                "08krr6lik6a1yz21287zaavq5qzscb90n5354hj6qjikk8yz30bc"))

(define rust-target-lexicon-0.12.14
  (crate-source "target-lexicon" "0.12.14"
                "0bzzr5cq1n56nmjp5fkf2h1g9a27lmkbld3qqfvwy6x2j4w41z71"))

(define rust-tera-1.20.0
  (crate-source "tera" "1.20.0"
                "1vnj9imw2h9szkd1izsrhwrc9jvazvdsp84x65wg2rg88ldqb7db"))

(define rust-thiserror-1.0.68
  (crate-source "thiserror" "1.0.68"
                "14m8zrzcgwczrnpd1qpsz3qrspfcw69m4s1rccc9gfqbh3f9kp82"))

(define rust-thiserror-2.0.12
  (crate-source "thiserror" "2.0.12"
                "024791nsc0np63g2pq30cjf9acj38z3jwx9apvvi8qsqmqnqlysn"))

(define rust-thiserror-impl-1.0.68
  (crate-source "thiserror-impl" "1.0.68"
                "0zjckd8jp6q0db1l8vc85bbcxqzv3sianpx4v29jfkgnlv4ixim7"))

(define rust-thiserror-impl-2.0.12
  (crate-source "thiserror-impl" "2.0.12"
                "07bsn7shydaidvyyrm7jz29vp78vrxr9cr9044rfmn078lmz8z3z"))

(define rust-thread-local-1.1.8
  (crate-source "thread_local" "1.1.8"
                "173i5lyjh011gsimk21np9jn8al18rxsrkjli20a7b8ks2xgk7lb"))

(define rust-toml-0.8.23
  (crate-source "toml" "0.8.23"
                "0qnkrq4lm2sdhp3l6cb6f26i8zbnhqb7mhbmksd550wxdfcyn6yw"))

(define rust-toml-datetime-0.6.11
  (crate-source "toml_datetime" "0.6.11"
                "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))

(define rust-toml-edit-0.21.1
  (crate-source "toml_edit" "0.21.1"
                "1qch02syrd9c8krcimfl72gyjz11fmjssh03hrg41dbqgzyk91ba"))

(define rust-toml-edit-0.22.27
  (crate-source "toml_edit" "0.22.27"
                "16l15xm40404asih8vyjvnka9g0xs9i4hfb6ry3ph9g419k8rzj1"))

(define rust-toml-write-0.1.2
  (crate-source "toml_write" "0.1.2"
                "008qlhqlqvljp1gpp9rn5cqs74gwvdgbvs92wnpq8y3jlz4zi6ax"))

(define rust-tracing-0.1.41
  (crate-source "tracing" "0.1.41"
                "1l5xrzyjfyayrwhvhldfnwdyligi1mpqm8mzbi2m1d6y6p2hlkkq"))

(define rust-tracing-attributes-0.1.28
  (crate-source "tracing-attributes" "0.1.28"
                "0v92l9cxs42rdm4m5hsa8z7ln1xsiw1zc2iil8c6k7lzq0jf2nir"))

(define rust-tracing-core-0.1.33
  (crate-source "tracing-core" "0.1.33"
                "170gc7cxyjx824r9kr17zc9gvzx89ypqfdzq259pr56gg5bwjwp6"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.19
  (crate-source "tracing-subscriber" "0.3.19"
                "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678"))

(define rust-typenum-1.17.0
  (crate-source "typenum" "1.17.0"
                "09dqxv69m9lj9zvv6xw5vxaqx15ps0vxyy5myg33i0kbqvq0pzs2"))

(define rust-ucd-trie-0.1.6
  (crate-source "ucd-trie" "0.1.6"
                "1ff4yfksirqs37ybin9aw71aa5gva00hw7jdxbw8w668zy964r7d"))

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

(define rust-unicode-ident-1.0.12
  (crate-source "unicode-ident" "1.0.12"
                "0jzf1znfpb2gx8nr8mvmyqs1crnv79l57nxnbiszc7xf7ynbjm1k"))

(define rust-unicode-segmentation-1.11.0
  (crate-source "unicode-segmentation" "1.11.0"
                "00kjpwp1g8fqm45drmwivlacn3y9jx73bvs09n6s3x73nqi7vj6l"))

(define rust-unicode-width-0.1.11
  (crate-source "unicode-width" "0.1.11"
                "11ds4ydhg8g7l06rlmh712q41qsrd0j0h00n1jm74kww3kqk65z5"))

(define rust-unicode-xid-0.2.4
  (crate-source "unicode-xid" "0.2.4"
                "131dfzf7d8fsr1ivch34x42c2d1ik5ig3g78brxncnn0r1sdyqpr"))

(define rust-utf8parse-0.2.1
  (crate-source "utf8parse" "0.2.1"
                "02ip1a0az0qmc2786vxk2nqwsgcwf17d3a38fkf0q7hrmwh9c6vi"))

(define rust-valuable-0.1.0
  (crate-source "valuable" "0.1.0"
                "0v9gp3nkjbl30z0fd56d8mx7w1csk86wwjhfjhr400wh9mfpw2w3"))

(define rust-version-check-0.9.4
  (crate-source "version_check" "0.9.4"
                "0gs8grwdlgh0xq660d7wr80x14vxbizmd8dbp29p2pdncx8lp1s9"))

(define rust-version-compare-0.2.0
  (crate-source "version-compare" "0.2.0"
                "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasm-bindgen-0.2.92
  (crate-source "wasm-bindgen" "0.2.92"
                "1a4mcw13nsk3fr8fxjzf9kk1wj88xkfsmnm0pjraw01ryqfm7qjb"))

(define rust-wasm-bindgen-backend-0.2.92
  (crate-source "wasm-bindgen-backend" "0.2.92"
                "1nj7wxbi49f0rw9d44rjzms26xlw6r76b2mrggx8jfbdjrxphkb1"))

(define rust-wasm-bindgen-macro-0.2.92
  (crate-source "wasm-bindgen-macro" "0.2.92"
                "09npa1srjjabd6nfph5yc03jb26sycjlxhy0c2a1pdrpx4yq5y51"))

(define rust-wasm-bindgen-macro-support-0.2.92
  (crate-source "wasm-bindgen-macro-support" "0.2.92"
                "1dqv2xs8zcyw4kjgzj84bknp2h76phmsb3n7j6hn396h4ssifkz9"))

(define rust-wasm-bindgen-shared-0.2.92
  (crate-source "wasm-bindgen-shared" "0.2.92"
                "15kyavsrna2cvy30kg03va257fraf9x00ny554vxngvpyaa0q6dg"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.6
  (crate-source "winapi-util" "0.1.6"
                "15i5lm39wd44004i9d5qspry2cynkrpvwzghr6s2c3dsk28nz7pj"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.4
  (crate-source "windows_aarch64_gnullvm" "0.52.4"
                "1jfam5qfngg8v1syxklnvy8la94b5igm7klkrk8z5ik5qgs6rx5w"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.4
  (crate-source "windows_aarch64_msvc" "0.52.4"
                "0xdn6db0rk8idn7dxsyflixq2dbj9x60kzdzal5rkxmwsffjb7ys"))

(define rust-windows-core-0.52.0
  (crate-source "windows-core" "0.52.0"
                "1nc3qv7sy24x0nlnb32f7alzpd6f72l4p24vl65vydbyil669ark"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.4
  (crate-source "windows_i686_gnu" "0.52.4"
                "1lq1g35sbj55ms86by4c080jcqrlfjy9bw5r4mgrkq4riwkdhx5l"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.4
  (crate-source "windows_i686_msvc" "0.52.4"
                "00lfzw88dkf3fdcf2hpfhp74i9pwbp7rwnj1nhy79vavksifj58m"))

(define rust-windows-sys-0.48.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.4
  (crate-source "windows-targets" "0.52.4"
                "06sdd7fin3dj9cmlg6n1dw0n1l10jhn9b8ckz1cqf0drb9z7plvx"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.4
  (crate-source "windows_x86_64_gnu" "0.52.4"
                "00qs6x33bf9lai2q68faxl56cszbv7mf7zqlslmc1778j0ahkvjy"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.4
  (crate-source "windows_x86_64_gnullvm" "0.52.4"
                "0xr13xxakp14hs4v4hg2ynjcv7wrzr3hg7zk5agglj8v8pr7kjkp"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.4
  (crate-source "windows_x86_64_msvc" "0.52.4"
                "1n0yc7xiv9iki1j3xl8nxlwwkr7dzsnwwvycvgxxv81d5bjm5drj"))

(define rust-winnow-0.5.40
  (crate-source "winnow" "0.5.40"
                "0xk8maai7gyxda673mmw3pj1hdizy5fpi7287vaywykkk19sk4zm"))

(define rust-winnow-0.7.10
  (crate-source "winnow" "0.7.10"
                "1v69byry8fyarzl83wij6f1h3zxnw69assp9kdfb10cdfk42hsf0"))

(define rust-x11rb-0.13.1
  (crate-source "x11rb" "0.13.1"
                "04jyfm0xmc538v09pzsyr2w801yadsgvyl2p0p76hzzffg5gz4ax"))

(define rust-x11rb-protocol-0.13.1
  (crate-source "x11rb-protocol" "0.13.1"
                "0gfbxf2k7kbk577j3rjhfx7hm70kmwln6da7xyc4l2za0d2pq47c"))

(define rust-xml-rs-0.8.19
  (crate-source "xml-rs" "0.8.19"
                "0nnpvk3fv32hgh7vs9gbg2swmzxx5yz73f4b7rak7q39q2x9rjqg"))

(define rust-zerocopy-0.7.35
  (crate-source "zerocopy" "0.7.35"
                "1w36q7b9il2flg0qskapgi9ymgg7p985vniqd09vi0mwib8lz6qv"))

(define rust-zerocopy-derive-0.7.35
  (crate-source "zerocopy-derive" "0.7.35"
                "0gnf2ap2y92nwdalzz3x7142f2b83sni66l39vxp2ijd6j080kzs"))

(define ssss-separator 'end-of-crates)


;;;
;;; Cargo inputs.
;;;

(define-cargo-inputs lookup-cargo-inputs
                     (runst =>
                            (list rust-adler32-1.2.0
                                  rust-ahash-0.8.11
                                  rust-aho-corasick-1.1.3
                                  rust-allocator-api2-0.2.18
                                  rust-android-tzdata-0.1.1
                                  rust-android-system-properties-0.1.5
                                  rust-anstream-0.6.14
                                  rust-anstyle-1.0.7
                                  rust-anstyle-parse-0.2.4
                                  rust-anstyle-query-1.0.3
                                  rust-anstyle-wincon-3.0.3
                                  rust-as-raw-xcb-connection-1.0.1
                                  rust-autocfg-1.2.0
                                  rust-bitflags-1.3.2
                                  rust-bitflags-2.6.0
                                  rust-block-buffer-0.10.4
                                  rust-bstr-1.9.1
                                  rust-bumpalo-3.15.4
                                  rust-cairo-rs-0.20.10
                                  rust-cairo-sys-rs-0.20.0
                                  rust-cc-1.0.90
                                  rust-cfg-expr-0.15.7
                                  rust-cfg-if-1.0.0
                                  rust-chrono-0.4.35
                                  rust-chrono-tz-0.9.0
                                  rust-chrono-tz-build-0.3.0
                                  rust-clap-4.5.4
                                  rust-clap-builder-4.5.2
                                  rust-clap-lex-0.7.0
                                  rust-colorchoice-1.0.1
                                  rust-colorsys-0.6.7
                                  rust-const-format-0.2.31
                                  rust-const-format-proc-macros-0.2.31
                                  rust-convert-case-0.6.0
                                  rust-core-foundation-sys-0.8.6
                                  rust-core2-0.4.0
                                  rust-cpufeatures-0.2.12
                                  rust-crc32fast-1.4.0
                                  rust-crossbeam-deque-0.8.5
                                  rust-crossbeam-epoch-0.9.18
                                  rust-crossbeam-utils-0.8.19
                                  rust-crypto-common-0.1.6
                                  rust-dary-heap-0.3.6
                                  rust-dbus-0.9.7
                                  rust-dbus-codegen-0.12.0
                                  rust-dbus-crossroads-0.5.2
                                  rust-deunicode-1.4.3
                                  rust-digest-0.10.7
                                  rust-dirs-5.0.1
                                  rust-dirs-sys-0.4.1
                                  rust-equivalent-1.0.1
                                  rust-errno-0.3.8
                                  rust-estimated-read-time-1.0.0
                                  rust-futures-channel-0.3.30
                                  rust-futures-core-0.3.30
                                  rust-futures-executor-0.3.30
                                  rust-futures-io-0.3.30
                                  rust-futures-macro-0.3.30
                                  rust-futures-task-0.3.30
                                  rust-futures-util-0.3.30
                                  rust-generic-array-0.14.7
                                  rust-gethostname-0.4.3
                                  rust-getrandom-0.2.12
                                  rust-gio-0.20.0
                                  rust-gio-sys-0.20.5
                                  rust-glib-0.20.5
                                  rust-glib-macros-0.20.5
                                  rust-glib-sys-0.20.5
                                  rust-globset-0.4.14
                                  rust-globwalk-0.9.1
                                  rust-gobject-sys-0.20.0
                                  rust-hashbrown-0.14.3
                                  rust-hashbrown-0.15.2
                                  rust-heck-0.5.0
                                  rust-humansize-2.1.3
                                  rust-humantime-2.2.0
                                  rust-iana-time-zone-0.1.60
                                  rust-iana-time-zone-haiku-0.1.2
                                  rust-ignore-0.4.22
                                  rust-include-flate-0.3.0
                                  rust-include-flate-codegen-0.2.0
                                  rust-indexmap-2.7.1
                                  rust-is-terminal-polyfill-1.70.0
                                  rust-itoa-1.0.11
                                  rust-js-sys-0.3.69
                                  rust-lazy-static-1.4.0
                                  rust-libc-0.2.153
                                  rust-libdbus-sys-0.2.5
                                  rust-libflate-2.1.0
                                  rust-libflate-lz77-2.1.0
                                  rust-libm-0.2.8
                                  rust-libredox-0.0.1
                                  rust-linux-raw-sys-0.4.13
                                  rust-log-0.4.21
                                  rust-matchers-0.1.0
                                  rust-memchr-2.7.4
                                  rust-nu-ansi-term-0.46.0
                                  rust-num-traits-0.2.18
                                  rust-once-cell-1.19.0
                                  rust-option-ext-0.2.0
                                  rust-overload-0.1.1
                                  rust-pango-0.20.10
                                  rust-pango-sys-0.20.0
                                  rust-pangocairo-0.20.10
                                  rust-pangocairo-sys-0.20.0
                                  rust-parse-zoneinfo-0.3.0
                                  rust-percent-encoding-2.3.1
                                  rust-pest-2.7.8
                                  rust-pest-derive-2.7.8
                                  rust-pest-generator-2.7.8
                                  rust-pest-meta-2.7.8
                                  rust-phf-0.11.2
                                  rust-phf-codegen-0.11.2
                                  rust-phf-generator-0.11.2
                                  rust-phf-shared-0.11.2
                                  rust-pin-project-lite-0.2.13
                                  rust-pin-utils-0.1.0
                                  rust-pkg-config-0.3.30
                                  rust-ppv-lite86-0.2.17
                                  rust-proc-macro-crate-3.1.0
                                  rust-proc-macro2-1.0.86
                                  rust-quote-1.0.35
                                  rust-rand-0.8.5
                                  rust-rand-chacha-0.3.1
                                  rust-rand-core-0.6.4
                                  rust-redox-syscall-0.4.1
                                  rust-redox-users-0.4.4
                                  rust-regex-1.11.1
                                  rust-regex-automata-0.1.10
                                  rust-regex-automata-0.4.8
                                  rust-regex-syntax-0.6.29
                                  rust-regex-syntax-0.8.5
                                  rust-rle-decode-fast-1.0.3
                                  rust-rust-embed-8.7.2
                                  rust-rust-embed-impl-8.7.2
                                  rust-rust-embed-utils-8.7.2
                                  rust-rustix-0.38.32
                                  rust-ryu-1.0.17
                                  rust-same-file-1.0.6
                                  rust-serde-1.0.219
                                  rust-serde-derive-1.0.219
                                  rust-serde-json-1.0.140
                                  rust-serde-regex-1.1.0
                                  rust-serde-spanned-0.6.9
                                  rust-sha2-0.10.8
                                  rust-sharded-slab-0.1.7
                                  rust-siphasher-0.3.11
                                  rust-slab-0.4.9
                                  rust-slug-0.1.5
                                  rust-smallvec-1.13.2
                                  rust-sscanf-0.4.3
                                  rust-sscanf-macro-0.4.3
                                  rust-strsim-0.11.1
                                  rust-syn-2.0.87
                                  rust-system-deps-7.0.1
                                  rust-target-lexicon-0.12.14
                                  rust-tera-1.20.0
                                  rust-thiserror-1.0.68
                                  rust-thiserror-2.0.12
                                  rust-thiserror-impl-1.0.68
                                  rust-thiserror-impl-2.0.12
                                  rust-thread-local-1.1.8
                                  rust-toml-0.8.23
                                  rust-toml-datetime-0.6.11
                                  rust-toml-edit-0.21.1
                                  rust-toml-edit-0.22.27
                                  rust-toml-write-0.1.2
                                  rust-tracing-0.1.41
                                  rust-tracing-attributes-0.1.28
                                  rust-tracing-core-0.1.33
                                  rust-tracing-log-0.2.0
                                  rust-tracing-subscriber-0.3.19
                                  rust-typenum-1.17.0
                                  rust-ucd-trie-0.1.6
                                  rust-unic-char-property-0.9.0
                                  rust-unic-char-range-0.9.0
                                  rust-unic-common-0.9.0
                                  rust-unic-segment-0.9.0
                                  rust-unic-ucd-segment-0.9.0
                                  rust-unic-ucd-version-0.9.0
                                  rust-unicode-ident-1.0.12
                                  rust-unicode-segmentation-1.11.0
                                  rust-unicode-width-0.1.11
                                  rust-unicode-xid-0.2.4
                                  rust-utf8parse-0.2.1
                                  rust-valuable-0.1.0
                                  rust-version-compare-0.2.0
                                  rust-version-check-0.9.4
                                  rust-walkdir-2.5.0
                                  rust-wasi-0.11.0+wasi-snapshot-preview1
                                  rust-wasm-bindgen-0.2.92
                                  rust-wasm-bindgen-backend-0.2.92
                                  rust-wasm-bindgen-macro-0.2.92
                                  rust-wasm-bindgen-macro-support-0.2.92
                                  rust-wasm-bindgen-shared-0.2.92
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-util-0.1.6
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-core-0.52.0
                                  rust-windows-sys-0.48.0
                                  rust-windows-sys-0.52.0
                                  rust-windows-targets-0.48.5
                                  rust-windows-targets-0.52.4
                                  rust-windows-aarch64-gnullvm-0.48.5
                                  rust-windows-aarch64-gnullvm-0.52.4
                                  rust-windows-aarch64-msvc-0.48.5
                                  rust-windows-aarch64-msvc-0.52.4
                                  rust-windows-i686-gnu-0.48.5
                                  rust-windows-i686-gnu-0.52.4
                                  rust-windows-i686-msvc-0.48.5
                                  rust-windows-i686-msvc-0.52.4
                                  rust-windows-x86-64-gnu-0.48.5
                                  rust-windows-x86-64-gnu-0.52.4
                                  rust-windows-x86-64-gnullvm-0.48.5
                                  rust-windows-x86-64-gnullvm-0.52.4
                                  rust-windows-x86-64-msvc-0.48.5
                                  rust-windows-x86-64-msvc-0.52.4
                                  rust-winnow-0.5.40
                                  rust-winnow-0.7.10
                                  rust-x11rb-0.13.1
                                  rust-x11rb-protocol-0.13.1
                                  rust-xml-rs-0.8.19
                                  rust-zerocopy-0.7.35
                                  rust-zerocopy-derive-0.7.35)))
