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
  #:use-module (afistfullofash packages rust-sources)
  
  #:export (lookup-cargo-inputs))

;;;
;;; This file is managed by ‘guix import’.  Do NOT add definitions manually.
;;;

;;;
;;; Rust libraries fetched from crates.io and non-workspace development
;;; snapshots.
;;;

(define qqqq-separator 'begin-of-crates)

(define rust-addr2line-0.25.1
  (crate-source "addr2line" "0.25.1"
                "0jwb96gv17vdr29hbzi0ha5q6jkpgjyn7rjlg5nis65k41rk0p8v"))

(define rust-adler2-2.0.0
  (crate-source "adler2" "2.0.0"
                "09r6drylvgy8vv8k20lnbvwq8gp09h7smfn6h1rxsy15pgh629si"))

(define rust-adler32-1.2.0
  (crate-source "adler32" "1.2.0"
                "0d7jq7jsjyhsgbhnfq5fvrlh9j0i9g1fqrl2735ibv5f75yjgqda"))

(define rust-aes-0.8.4
  (crate-source "aes" "0.8.4"
                "1853796anlwp4kqim0s6wm1srl4ib621nm0cl2h3c8klsjkgfsdi"))

(define rust-ahash-0.8.12
  (crate-source "ahash" "0.8.12"
                "0xbsp9rlm5ki017c0w6ay8kjwinwm8knjncci95mii30rmwz25as"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-aho-corasick-1.1.4
  (crate-source "aho-corasick" "1.1.4"
                "00a32wb2h07im3skkikc495jvncf62jl6s96vwc7bhi70h9imlyx"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-android-tzdata-0.1.1
  (crate-source "android-tzdata" "0.1.1"
                "1w7ynjxrfs97xg3qlcdns4kgfpwcdv824g611fq32cag4cdr96g9"))

(define rust-annotate-snippets-0.11.5
  (crate-source "annotate-snippets" "0.11.5"
                "1i1bmr5vy957l8fvivj9x1xs24np0k56rdgwj0bxqk45b2p8w3ki"))

(define rust-ansi-term-0.12.1
  (crate-source "ansi_term" "0.12.1"
                "1ljmkbilxgmhavxvxqa7qvm6f3fjggi7q2l3a72q9x0cxjvrnanm"))

(define rust-anstream-0.6.14
  (crate-source "anstream" "0.6.14"
                "0nx1vnfs2lil1sl14l49i6jvp6zpjczn85wxx4xw1ycafvx7b321"))

(define rust-anstream-0.6.21
  (crate-source "anstream" "0.6.21"
                "0jjgixms4qjj58dzr846h2s29p8w7ynwr9b9x6246m1pwy0v5ma3"))

(define rust-anstyle-1.0.13
  (crate-source "anstyle" "1.0.13"
                "0y2ynjqajpny6q0amvfzzgw0gfw3l47z85km4gvx87vg02lcr4ji"))

(define rust-anstyle-1.0.8
  (crate-source "anstyle" "1.0.8"
                "1cfmkza63xpn1kkz844mgjwm9miaiz4jkyczmwxzivcsypk1vv0v"))

(define rust-anstyle-parse-0.2.4
  (crate-source "anstyle-parse" "0.2.4"
                "1m121pa4plpcb4g7xali2kv9njmgb3713q3fxf60b4jd0fli2fn0"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.0.3
  (crate-source "anstyle-query" "1.0.3"
                "1x9pyl231rry5g45dvkdb2sfnl2dx2f4qd9a5v3ml8kr9ryr0k56"))

(define rust-anstyle-query-1.1.5
  (crate-source "anstyle-query" "1.1.5"
                "1p6shfpnbghs6jsa0vnqd8bb8gd7pjd0jr7w0j8jikakzmr8zi20"))

(define rust-anstyle-wincon-3.0.11
  (crate-source "anstyle-wincon" "3.0.11"
                "0zblannm70sk3xny337mz7c6d8q8i24vhbqi42ld8v7q1wjnl7i9"))

(define rust-anstyle-wincon-3.0.3
  (crate-source "anstyle-wincon" "3.0.3"
                "06gv2vbj4hvwb8fxqjmvabp5kx2w01cjgh86pd98y1mpzr4q98v1"))

(define rust-anyhow-1.0.102
  (crate-source "anyhow" "1.0.102"
                "0b447dra1v12z474c6z4jmicdmc5yxz5bakympdnij44ckw2s83z"))

(define rust-anyhow-1.0.89
  (crate-source "anyhow" "1.0.89"
                "1xh1vg89n56h6nqikcmgbpmkixjds33492klrp9m96xrbmhgizc6"))

(define rust-arraydeque-0.5.1
  (crate-source "arraydeque" "0.5.1"
                "0dn2xdfg3rkiqsh8a6achnmvf5nf11xk33xgjzpksliab4yjx43x"))

(define rust-as-raw-xcb-connection-1.0.1
  (crate-source "as-raw-xcb-connection" "1.0.1"
                "0sqgpz2ymv5yx76r5j2npjq2x5qvvqnw0vrs35cyv30p3pfp2m8p"))

(define rust-assert-cmd-2.0.16
  (crate-source "assert_cmd" "2.0.16"
                "0gdj0710k3lnvyjmpv8a4dgwrk9ib85l2wfw4n2xwy3qyavka66w"))

(define rust-atty-0.2.14
  (crate-source "atty" "0.2.14"
                "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))

(define rust-autocfg-1.3.0
  (crate-source "autocfg" "1.3.0"
                "1c3njkfzpil03k92q0mij5y1pkhhfr4j3bf0h53bgl2vs85lsjqc"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-aws-lc-fips-sys-0.13.12
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "aws-lc-fips-sys" "0.13.12"
                "143xqasif8fj1vvphsv1g0866rmnm4zl9dvza2wbvvyxmm1cvn2y"))

(define rust-aws-lc-rs-1.16.0
  (crate-source "aws-lc-rs" "1.16.0"
                "1acsazi40b19inwq96c04a2d9jsnfb1jnc4q4q86f5xvwd8b79yr"))

(define rust-aws-lc-sys-0.27.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "aws-lc-sys" "0.27.1"
                "07sba4qa23gr0zzanlf9a0vlsi4fk5shl4k2svbwwwb1fy3ni4kp"))

(define rust-aws-lc-sys-0.37.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "aws-lc-sys" "0.37.1"
                "0j85hjk6jh1cfgqnj9fw61zmm3hiqara5dqx264149lh80hzx4mh"))

(define rust-axoasset-2.0.1
  (crate-source "axoasset" "2.0.1"
                "0gnq6l387ss43jsg2a3fx2wp9s6mkplfkkdvqw25wqwvfg1bkq8v"))

(define rust-axocli-0.3.0
  (crate-source "axocli" "0.3.0"
                "1qmhzdj5aaqrxkh53q4zw70lwg4j21nkq1l224q2sdmrm9gc89yq"))

(define rust-axoprocess-0.2.1
  (crate-source "axoprocess" "0.2.1"
                "06syx48krg47sjjxll2lb45r09hpx7b3riiphlvr2bn0lsc4fjwa"))

(define rust-axoproject-0.31.0
  (crate-source "axoproject" "0.31.0"
                "1k6ypv9q9qb7paiz3z3pnx5qbgqy6i95qfcgbrsgyj185rhgyvk0"))

(define rust-axotag-0.3.0
  (crate-source "axotag" "0.3.0"
                "0svyd9f2nmsg3fdgb7brnna6zrcq1djvadl403lp5k64zchk34nw"))

(define rust-axoupdater-0.10.0
  (crate-source "axoupdater" "0.10.0"
                "1kqqn9f3dd4gdndghki7qqwzii7irm0p76qk4x554ydbic8nzdha"))

(define rust-backtrace-0.3.76
  (crate-source "backtrace" "0.3.76"
                "1mibx75x4jf6wz7qjifynld3hpw3vq6sy3d3c9y5s88sg59ihlxv"))

(define rust-backtrace-ext-0.2.1
  (crate-source "backtrace-ext" "0.2.1"
                "0l4xacjnx4jrn9k14xbs2swks018mviq03sp7c1gn62apviywysk"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-bindgen-0.69.5
  (crate-source "bindgen" "0.69.5"
                "1240snlcfj663k04bjsg629g4wx6f83flgbjh5rzpgyagk3864r7"))

(define rust-bindgen-0.72.1
  (crate-source "bindgen" "0.72.1"
                "15bq73y3wd3x3vxh3z3g72hy08zs8rxg1f0i1xsrrd6g16spcdwr"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.11.0
  (crate-source "bitflags" "2.11.0"
                "1bwjibwry5nfwsfm9kjg2dqx5n5nja9xymwbfl6svnn8jsz6ff44"))

(define rust-bitflags-2.5.0
  (crate-source "bitflags" "2.5.0"
                "1h91vdx1il069vdiiissj8ymzj130rbiic0dbs77yxjgjim9sjyg"))

(define rust-bitflags-2.9.4
  (crate-source "bitflags" "2.9.4"
                "157kkcv8s7vk6d17dar1pa5cqcz4c8pdrn16wm1ld7jnr86d2q92"))

(define rust-blake2-0.10.6
  (crate-source "blake2" "0.10.6"
                "1zlf7w7gql12v61d9jcbbswa3dw8qxsjglylsiljp9f9b3a2ll26"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-bssl-sys-0.1.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "bssl-sys" "0.1.0"
                "0p5v3ad1paf12db4hmwq4j8dvcrppsscf57dwvr880q67hwi4b9i"))

(define rust-bstr-1.12.0
  (crate-source "bstr" "1.12.0"
                "195i0gd7r7jg7a8spkmw08492n7rmiabcvz880xn2z8dkp8i6h93"))

(define rust-bstr-1.12.1
  (crate-source "bstr" "1.12.1"
                "1arc1v7h5l86vd6z76z3xykjzldqd5icldn7j9d3p7z6x0d4w133"))

(define rust-bstr-1.9.1
  (crate-source "bstr" "1.9.1"
                "01ipr5rncw3kf4dyc1p2g00njn1df2b0xpviwhb8830iv77wbvq5"))

(define rust-bumpalo-3.16.0
  (crate-source "bumpalo" "3.16.0"
                "0b015qb4knwanbdlp1x48pkb4pm57b8gidbhhhxr900q2wb6fabr"))

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

(define rust-bumpalo-3.20.2
  (crate-source "bumpalo" "3.20.2"
                "1jrgxlff76k9glam0akhwpil2fr1w32gbjdf5hpipc7ld2c7h82x"))

(define rust-bytecount-0.6.8
  (crate-source "bytecount" "0.6.8"
                "1klqfjwn41fwmcqw4z03v6i4imgrf7lmf3b5s9v74hxir8hrps2w"))

(define rust-bytemuck-1.16.0
  (crate-source "bytemuck" "1.16.0"
                "19dwdvjri09mhgrngy0737965pchm25ix2yma8sgwpjxrcalr0vq"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-byteorder-lite-0.1.0
  (crate-source "byteorder-lite" "0.1.0"
                "15alafmz4b9az56z6x7glcbcb6a8bfgyd109qc3bvx07zx4fj7wg"))

(define rust-bytes-1.11.1
  (crate-source "bytes" "1.11.1"
                "0czwlhbq8z29wq0ia87yass2mzy1y0jcasjb8ghriiybnwrqfx0y"))

(define rust-bzip2-0.6.0
  (crate-source "bzip2" "0.6.0"
                "1zsc7y9581ffcm527ha6fij1yh9s4zci2i1hg97qw11l4kadra5y"))

(define rust-cairo-rs-0.19.4
  (crate-source "cairo-rs" "0.19.4"
                "0qp5rixgipdj9d8yd5458hzfxam1rgpzcxi90vq6q0v91r6jmb5j"))

(define rust-cairo-sys-rs-0.19.2
  (crate-source "cairo-sys-rs" "0.19.2"
                "0r0yp0lph77lm4blrn6fvdmz2i3r8ibkkjg6nmwbvvv4jq8v6fzx"))

(define rust-camino-1.2.2
  (crate-source "camino" "1.2.2"
                "0j0ayqfbbl8bxg0795ssk1hzkjix3dvl2kk63hdgzf9cd5nscag6"))

(define rust-cargo-dist-schema-0.31.0
  (crate-source "cargo-dist-schema" "0.31.0"
                "0x42vmaz2fansya0q7j1x7z1k13mmm874hdvnbg0bmpg6ik3vf5i"))

(define rust-cargo-metadata-0.18.1
  (crate-source "cargo_metadata" "0.18.1"
                "0drh0zndl4qgndy6kg6783cydbvhxgv0hcg7d9hhqx0zwi3nb21d"))

(define rust-cargo-metadata-0.23.1
  (crate-source "cargo_metadata" "0.23.1"
                "1sddycfscjy47av3ykzykqgz8zjds0i00gcxs76vw4x1n0bpv67g"))

(define rust-cargo-platform-0.1.8
  (crate-source "cargo-platform" "0.1.8"
                "1z5b7ivbj508wkqdg2vb0hw4vi1k1pyhcn6h1h1b8svcb8vg1c94"))

(define rust-cargo-platform-0.3.1
  (crate-source "cargo-platform" "0.3.1"
                "1r2fdbcb9gvyzy38vciz7hz1fsv75iw1nd9g838syw5j8idc8bhj"))

(define rust-cargo-wix-0.3.9
  (crate-source "cargo-wix" "0.3.9"
                "1bfv3jdflkr6ahq4wkdysjxakrh5m1c3ncprbv1qi59ha5gzn8vh"))

(define rust-catppuccin-2.4.0
  (crate-source "catppuccin" "2.4.0"
                "1x1vccgzc690g8cxd84dm6xw971ypxdhfkcsgc2qzy6m47m9h5xa"))

(define rust-cc-1.0.98
  (crate-source "cc" "1.0.98"
                "0gzhij74hblfkzwwyysdc8crfd6fr0m226vzmijmwwhdakkp1hj1"))

(define rust-cc-1.2.39
  (crate-source "cc" "1.2.39"
                "0py3546wz3k5qi6pbfz80jvg0g3qgzr21c7a1p5wjvscjm4l6dg1"))

(define rust-cc-1.2.56
  (crate-source "cc" "1.2.56"
                "1chvh9g2izhqad7vzy4cc7xpdljdvqpsr6x6hv1hmyqv3mlkbgxf"))

(define rust-cesu8-1.1.0
  (crate-source "cesu8" "1.1.0"
                "0g6q58wa7khxrxcxgnqyi9s1z2cjywwwd3hzr5c55wskhx6s0hvd"))

(define rust-cexpr-0.6.0
  (crate-source "cexpr" "0.6.0"
                "0rl77bwhs5p979ih4r0202cn5jrfsrbgrksp40lkfz5vk1x3ib3g"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-expr-0.15.8
  (crate-source "cfg-expr" "0.15.8"
                "00lgf717pmf5qd2qsxxzs815v6baqg38d6m5i6wlh235p14asryh"))

(define rust-cfg-expr-0.20.6
  (crate-source "cfg-expr" "0.20.6"
                "0smbxbd39s2kpmz6r9yg4xmh0wx5d1in6amf49rpr0m6l6szbkkq"))

(define rust-cfg-if-0.1.10
  (crate-source "cfg-if" "0.1.10"
                "08h80ihs74jcyp24cd75wwabygbbdgl05k6p5dmq8akbr78vv1a7"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-cfg-if-1.0.3
  (crate-source "cfg-if" "1.0.3"
                "1afg7146gbxjvkbjx7i5sdrpqp9q5akmk9004fr8rsm90jf2il9g"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-chrono-0.4.38
  (crate-source "chrono" "0.4.38"
                "009l8vc5p8750vn02z30mblg4pv2qhkbfizhfwmzc6vpy5nr67x2"))

(define rust-chrono-0.4.42
  (crate-source "chrono" "0.4.42"
                "1lp8iz9js9jwxw0sj8yi59v54lgvwdvm49b9wch77f25sfym4l0l"))

(define rust-chrono-0.4.44
  (crate-source "chrono" "0.4.44"
                "1c64mk9a235271j5g3v4zrzqqmd43vp9vki7vqfllpqf5rd0fwy6"))

(define rust-chrono-tz-0.9.0
  (crate-source "chrono-tz" "0.9.0"
                "1fvicqrlmdsjkrgxr7bxfd62i9w2qi2b6iv4w85av5syvqlqnsck"))

(define rust-chrono-tz-build-0.3.0
  (crate-source "chrono-tz-build" "0.3.0"
                "1c8ixwwwsn9kgs1dr5mz963p0fgw9j9p7fzb3w2c7y8xhkp8l20c"))

(define rust-cipher-0.4.4
  (crate-source "cipher" "0.4.4"
                "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp"))

(define rust-clang-sys-1.8.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "clang-sys" "1.8.1"
                "1x1r9yqss76z8xwpdanw313ss6fniwc1r7dzb5ycjn0ph53kj0hb"))

(define rust-clap-2.34.0
  (crate-source "clap" "2.34.0"
                "071q5d8jfwbazi6zhik9xwpacx5i6kb2vkzy060vhf0c3120aqd0"))

(define rust-clap-4.5.20
  (crate-source "clap" "4.5.20"
                "1s37v23gcxkjy4800qgnkxkpliz68vslpr5sgn1xar56hmnkfzxr"))

(define rust-clap-4.5.60
  (crate-source "clap" "4.5.60"
                "02h3nzznssjgp815nnbzk0r62y2iw03kdli75c233kirld6z75r7"))

(define rust-clap-builder-4.5.20
  (crate-source "clap_builder" "4.5.20"
                "0m6w10l2f65h3ch0d53lql6p26xxrh20ffipra9ysjsfsjmq1g0r"))

(define rust-clap-builder-4.5.60
  (crate-source "clap_builder" "4.5.60"
                "0xk8mdizvmmn6w5ij5cwhy5pbgyac4w9pfvl6nqmjl7a5hql38i4"))

(define rust-clap-cargo-0.18.3
  (crate-source "clap-cargo" "0.18.3"
                "01l070c3a88spywm8drhfzwn3ljp142fqsi1p15pan42bj9m2rck"))

(define rust-clap-complete-4.5.66
  (crate-source "clap_complete" "4.5.66"
                "0c8h6x3x1ddldfmhii12hrd92v1av8d18rckdzjs8qciwfvs6my7"))

(define rust-clap-derive-4.5.18
  (crate-source "clap_derive" "4.5.18"
                "1ardb26bvcpg72q9myr7yir3a8c83gx7vxk1cccabsd9n73s1ija"))

(define rust-clap-derive-4.5.55
  (crate-source "clap_derive" "4.5.55"
                "1r949xis3jmhzh387smd70vc8a3b9734ck3g5ahg59a63bd969x9"))

(define rust-clap-lex-0.7.0
  (crate-source "clap_lex" "0.7.0"
                "1kh1sckgq71kay2rrr149pl9gbsrvyccsq6xm5xpnq0cxnyqzk4q"))

(define rust-clap-lex-1.0.0
  (crate-source "clap_lex" "1.0.0"
                "0c8888qi1l9sayqlv666h8s0yxn2qc6jr88v1zagk43mpjjjx0is"))

(define rust-clap-stdin-0.5.1
  (crate-source "clap-stdin" "0.5.1"
                "0gw0hpi8d99yqs71c40jjfw4q7l94a25jnyssgkw3grkcs4zf7a7"))

(define rust-cmake-0.1.57
  (crate-source "cmake" "0.1.57"
                "0zgg10qgykig4nxyf7whrqfg7fkk0xfxhiavikmrndvbrm23qi3m"))

(define rust-color-backtrace-0.7.2
  (crate-source "color-backtrace" "0.7.2"
                "1gdsxya683msjlrjhj5bwzyyjb85ij73mns356h7p1rfsvajk0rh"))

(define rust-colorchoice-1.0.1
  (crate-source "colorchoice" "1.0.1"
                "08h4jsrd2j5k6lp1b9v5p1f1g7cmyzm4djsvb3ydywdb4hmqashb"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-colorsys-0.6.7
  ;; TODO: Check bundled sources.
  (crate-source "colorsys" "0.6.7"
                "1g8vwcv89n2dzi9bmbzqlj9cl9a89jz49668grbcncv4cjx1l9jl"))

(define rust-combine-4.6.7
  (crate-source "combine" "4.6.7"
                "1z8rh8wp59gf8k23ar010phgs0wgf5i8cx4fg01gwcnzfn5k0nms"))

(define rust-comfy-table-7.2.2
  (crate-source "comfy-table" "7.2.2"
                "0ixdw77rly84i5z1mxyw6v8lp1isaawnmgxv5d64n88zrxp5v34m"))

(define rust-console-0.15.10
  (crate-source "console" "0.15.10"
                "06q4ag46machxp5w381x1v9l2g7d801q6sawvxcpidarh36nwg7a"))

(define rust-console-0.15.11
  (crate-source "console" "0.15.11"
                "1n5gmsjk6isbnw6qss043377kln20lfwlmdk3vswpwpr21dwnk05"))

(define rust-console-0.16.2
  (crate-source "console" "0.16.2"
                "1i5y6h3myz38jl9p3gglx5vh9c69kxxajsv3jx0pw8i6i555mr03"))

(define rust-const-format-0.2.31
  (crate-source "const_format" "0.2.31"
                "0j7zs1aar3daic7yy18sg34a518f5zzimn3q8fd1yww5lb3yz469"))

(define rust-const-format-proc-macros-0.2.31
  (crate-source "const_format_proc_macros" "0.2.31"
                "1xibiffpmwvlina6amybiz66g5zgs5r5gk9jrywlr1sa377bc9p0"))

(define rust-constant-time-eq-0.4.2
  (crate-source "constant_time_eq" "0.4.2"
                "16zamq60dq80k3rqlzh9j9cpjhishmh924lnwbplgrnmkkvfylix"))

(define rust-convert-case-0.6.0
  (crate-source "convert_case" "0.6.0"
                "1jn1pq6fp3rri88zyw6jlhwwgf6qiyc08d6gjv0qypgkl862n67c"))

(define rust-core-foundation-0.10.1
  (crate-source "core-foundation" "0.10.1"
                "1xjns6dqf36rni2x9f47b65grxwdm20kwdg9lhmzdrrkwadcv9mj"))

(define rust-core-foundation-sys-0.8.6
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.6"
                "13w6sdf06r0hn7bx2b45zxsg1mm2phz34jikm6xc5qrbr6djpsh6"))

(define rust-core-foundation-sys-0.8.7
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-core2-0.4.0
  (crate-source "core2" "0.4.0"
                "01f5xv0kf3ds3xm7byg78hycbanb8zlpvsfv4j47y46n3bpsg6xl"))

(define rust-cpufeatures-0.2.12
  (crate-source "cpufeatures" "0.2.12"
                "012m7rrak4girqlii3jnqwrr73gv1i980q4wra5yyyhvzwk5xzjk"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-crc32fast-1.5.0
  (crate-source "crc32fast" "1.5.0"
                "04d51liy8rbssra92p0qnwjw8i9rm9c4m3bwy19wjamz1k4w30cl"))

(define rust-crossbeam-deque-0.8.5
  (crate-source "crossbeam-deque" "0.8.5"
                "03bp38ljx4wj6vvy4fbhx41q8f585zyqix6pncz1mkz93z08qgv1"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-utils-0.8.20
  (crate-source "crossbeam-utils" "0.8.20"
                "100fksq5mm1n7zj242cclkw6yf7a4a8ix3lvpfkhxvdhbda9kv12"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crossterm-0.29.0
  (crate-source "crossterm" "0.29.0"
                "0yzqxxd90k7d2ac26xq1awsznsaq0qika2nv1ik3p0vzqvjg5ffq"))

(define rust-crossterm-winapi-0.9.1
  (crate-source "crossterm_winapi" "0.9.1"
                "0axbfb2ykbwbpf1hmxwpawwfs8wvmkcka5m561l7yp36ldi7rpdc"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-css-colors-1.0.1
  (crate-source "css-colors" "1.0.1"
                "0dljfdw4p54drjy9a5m6h5qnvz8lkdllxfkln0vk9wh8azybphi2"))

(define rust-ctest-0.4.11
  (crate-source "ctest" "0.4.11"
                "1pldb2yfc2qz899bk2gak9p21kkpwvwf7vpvmxah76hz116wj61g"))

(define rust-current-platform-0.2.0
  (crate-source "current_platform" "0.2.0"
                "1g504i1l733bn1hyzzfvwmc8qq84dxxpscs9rcb21cj4zsy5hj57"))

(define rust-darling-0.14.4
  (crate-source "darling" "0.14.4"
                "0l1qrn805bsxa0iy7x8bmdwr8c10hlw0yiqs8ckv7lbz86rhqxbv"))

(define rust-darling-0.21.3
  (crate-source "darling" "0.21.3"
                "1h281ah78pz05450r71h3gwm2n24hy8yngbz58g426l4j1q37pww"))

(define rust-darling-core-0.14.4
  (crate-source "darling_core" "0.14.4"
                "1w4b2ndxmkwghwq84yphk8x15jnpivm08w596g12ry5pwsk1r70h"))

(define rust-darling-core-0.21.3
  (crate-source "darling_core" "0.21.3"
                "193ya45qgac0a4siwghk0bl8im8h89p3cald7kw8ag3yrmg1jiqj"))

(define rust-darling-macro-0.14.4
  (crate-source "darling_macro" "0.14.4"
                "13mlyd5w275c815k0ijf6g4c446hs8b3m2h4an5isqgpr7dv9am4"))

(define rust-darling-macro-0.21.3
  (crate-source "darling_macro" "0.21.3"
                "10ac85n4lnx3rmf5rw8lijl2c0sbl6ghcpgfmzh0s26ihbghi0yk"))

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

(define rust-debug-ignore-1.0.5
  (crate-source "debug-ignore" "1.0.5"
                "08gwdny6124ggy4hyli92hdyiqc5j2z9lqhbw81k0mgljcfyvrzz"))

(define rust-deranged-0.5.6
  (crate-source "deranged" "0.5.6"
                "1i48p5l878bw4qzi1wz43lrq3jvplhpdzfxvjg0x3qn2janwagfc"))

(define rust-deranged-0.5.8
  (crate-source "deranged" "0.5.8"
                "0711df3w16vx80k55ivkwzwswziinj4dz05xci3rvmn15g615n3w"))

(define rust-derive-builder-0.11.2
  (crate-source "derive_builder" "0.11.2"
                "18q4jx8zvg2pkx2d1nmlklx8m48bwrghyg8rdg1irdwkw5xxyynh"))

(define rust-derive-builder-core-0.11.2
  (crate-source "derive_builder_core" "0.11.2"
                "1i5gmf5lglbg7agj1khc6k9swf1clfs5fg6w0icw1w91m77x948z"))

(define rust-derive-builder-macro-0.11.2
  (crate-source "derive_builder_macro" "0.11.2"
                "0s6xfgsybd9wbk39hbgqjcn7d1l36a33q6v7d0x5y17d5fvi80wg"))

(define rust-detect-newline-style-0.1.2
  (crate-source "detect-newline-style" "0.1.2"
                "0j9pcjk2ab21f36fqybz69whd1c4xy60hy7qd5v59aqm6rfg490i"))

(define rust-deunicode-1.6.0
  (crate-source "deunicode" "1.6.0"
                "006gnml4jy3m03yqma8qvx7kl9i2bw667za9f7yc6k9ckv64959k"))

(define rust-deunicode-1.6.2
  (crate-source "deunicode" "1.6.2"
                "013biy7hhy59jcbry4dqn2pf4qhaw083ksn8xxiw373wjc37imdb"))

(define rust-dialoguer-0.12.0
  (crate-source "dialoguer" "0.12.0"
                "15mdq2cp838yiq9fs1jkhvskixvlqz5p8f8dipkn88xz06sh9w95"))

(define rust-diesel-2.3.6
  (crate-source "diesel" "2.3.6"
                "0ka9b5i9m5q8sm7cxz6dd398ykd8pwxmx6jzrzmvcvsa33yc5dnr"))

(define rust-diesel-derives-2.3.7
  (crate-source "diesel_derives" "2.3.7"
                "0kj0ds9qkzkxv7gi8hdgf5m5x1kaq8289r1nq1qbcsy0zbq8nqa7"))

(define rust-diesel-migrations-2.3.1
  (crate-source "diesel_migrations" "2.3.1"
                "174g5fl3vfjb3rrv8fk8z4m1k0h81sqcfmgcz4si23szciax4pvl"))

(define rust-diesel-table-macro-syntax-0.3.0
  (crate-source "diesel_table_macro_syntax" "0.3.0"
                "135ypb3k6wg63kqf1bddc45w1hj3qy6n95sn253i2r28dc3l897y"))

(define rust-difflib-0.4.0
  (crate-source "difflib" "0.4.0"
                "1s7byq4d7jgf2hcp2lcqxi2piqwl8xqlharfbi8kf90n8csy7131"))

(define rust-diffy-0.4.2
  (crate-source "diffy" "0.4.2"
                "14fjsz0gnd06fy96l1mksp7m78fv645sp19r504d1gcl072vhidm"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-dirs-2.0.2
  (crate-source "dirs" "2.0.2"
                "1qymhyq7w7wlf1dirq6gsnabdyzg6yi2yyxkx6c4ldlkbjdaibhk"))

(define rust-dirs-5.0.1
  (crate-source "dirs" "5.0.1"
                "0992xk5vx75b2x91nw9ssb51mpl8x73j9rxmpi96cryn0ffmmi24"))

(define rust-dirs-sys-0.3.7
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "dirs-sys" "0.3.7"
                "19md1cnkazham8a6kh22v12d8hh3raqahfk6yb043vrjr68is78v"))

(define rust-dirs-sys-0.4.1
  ;; TODO: Check bundled sources.
  (crate-source "dirs-sys" "0.4.1"
                "071jy0pvaad9lsa6mzawxrh7cmr7hsmsdxwzm7jzldfkrfjha3sj"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-doc-comment-0.3.3
  (crate-source "doc-comment" "0.3.3"
                "043sprsf3wl926zmck1bm7gw0jq50mb76lkpk49vasfr6ax1p97y"))

(define rust-document-features-0.2.11
  (crate-source "document-features" "0.2.11"
                "0pdhpbz687fk2rkgz45yy3gvbhlxliwb7g1lj3jbx1f1qr89n94m"))

(define rust-dotenvy-0.15.7
  (crate-source "dotenvy" "0.15.7"
                "16s3n973n5aqym02692i1npb079n5mb0fwql42ikmwn8wnrrbbqs"))

(define rust-downcast-rs-2.0.2
  (crate-source "downcast-rs" "2.0.2"
                "1g0crs9qgz0sd9cwdgmm0zvjin2v549v46xfnc859rk903v40whi"))

(define rust-dsl-auto-type-0.2.0
  (crate-source "dsl_auto_type" "0.2.0"
                "0zkmlx1a3p9mfsfns39nspvci2gv70vivwipfyr6vw5ywhrjc4nx"))

(define rust-dunce-1.0.5
  (crate-source "dunce" "1.0.5"
                "04y8wwv3vvcqaqmqzssi6k0ii9gs6fpz96j5w9nky2ccsl23axwj"))

(define rust-dyn-clone-1.0.17
  (crate-source "dyn-clone" "1.0.17"
                "09cig7dgg6jnqa10p4233nd8wllbjf4ffsw7wj0m4lwa5w3z0vhd"))

(define rust-either-1.12.0
  (crate-source "either" "1.12.0"
                "12xmhlrv5gfsraimh6xaxcmb0qh6cc7w7ap4sw40ky9wfm095jix"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-encoding-rs-0.8.34
  (crate-source "encoding_rs" "0.8.34"
                "0nagpi1rjqdpvakymwmnlxzq908ncg868lml5b70n08bm82fjpdl"))

(define rust-encoding-rs-io-0.1.7
  (crate-source "encoding_rs_io" "0.1.7"
                "10ra4l688cdadd8h1lsbahld1zbywnnqv68366mbhamn3xjwbhqw"))

(define rust-env-logger-0.10.2
  (crate-source "env_logger" "0.10.2"
                "1005v71kay9kbz1d5907l0y7vh9qn2fqsp2yfgb8bjvin6m0bm2c"))

(define rust-equivalent-1.0.1
  (crate-source "equivalent" "1.0.1"
                "1malmx5f4lkfvqasz319lq6gb3ddg19yzf9s8cykfsgzdmyq0hsl"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-errno-0.3.13
  (crate-source "errno" "0.3.13"
                "1bd5g3srn66zr3bspac0150bvpg1s7zi6zwhwhlayivciz12m3kp"))

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-errno-0.3.9
  (crate-source "errno" "0.3.9"
                "1fi0m0493maq1jygcf1bya9cymz2pc1mqxj26bdv7yjd37v5qk2k"))

(define rust-estimated-read-time-1.0.0
  (crate-source "estimated_read_time" "1.0.0"
                "1mz8pkgk9v0cfzfjw659zl997gilangb78ccds8gic8h2hsgv734"))

(define rust-fastrand-2.1.0
  (crate-source "fastrand" "2.1.0"
                "06p5d0rxq7by260m4ym9ial0bwgi0v42lrvhl6nm2g7h0h2m3h4z"))

(define rust-fastrand-2.1.1
  (crate-source "fastrand" "2.1.1"
                "19nyzdq3ha4g173364y2wijmd6jlyms8qx40daqkxsnl458jmh78"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fd-lock-4.0.4
  (crate-source "fd-lock" "4.0.4"
                "0y5a22zaqns06slndm64gjdx983i6b4l4ks895rxznnn4bv2zs8c"))

(define rust-filetime-0.2.23
  (crate-source "filetime" "0.2.23"
                "1za0sbq7fqidk8aaq9v7m9ms0sv8mmi49g6p5cphpan819q4gr0y"))

(define rust-find-msvc-tools-0.1.2
  (crate-source "find-msvc-tools" "0.1.2"
                "0nbrhvk4m04hviiwbqp2jwcv9j2k70x0q2kcvfk51iygvaqp7v8w"))

(define rust-find-msvc-tools-0.1.9
  (crate-source "find-msvc-tools" "0.1.9"
                "10nmi0qdskq6l7zwxw5g56xny7hb624iki1c39d907qmfh3vrbjv"))

(define rust-fixedbitset-0.5.7
  (crate-source "fixedbitset" "0.5.7"
                "16fd3v9d2cms2vddf9xhlm56sz4j0zgrk3d2h6v1l7hx760lwrqx"))

(define rust-flate2-1.1.9
  (crate-source "flate2" "1.1.9"
                "0g2pb7cxnzcbzrj8bw4v6gpqqp21aycmf6d84rzb6j748qkvlgw4"))

(define rust-float-cmp-0.9.0
  (crate-source "float-cmp" "0.9.0"
                "1i799ksbq7fj9rm9m82g1yqgm6xi3jnrmylddmqknmksajylpplq"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-foldhash-0.2.0
  (crate-source "foldhash" "0.2.0"
                "1nvgylb099s11xpfm1kn2wcsql080nqmnhj1l25bp3r2b35j9kkp"))

(define rust-foreign-types-0.3.2
  (crate-source "foreign-types" "0.3.2"
                "1cgk0vyd7r45cj769jym4a6s7vwshvd0z4bqrb92q1fwibmkkwzn"))

(define rust-foreign-types-shared-0.1.1
  (crate-source "foreign-types-shared" "0.1.1"
                "0jxgzd04ra4imjv8jgkmdq59kj8fsz6w4zxsbmlai34h26225c00"))

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-fs-extra-1.3.0
  (crate-source "fs_extra" "1.3.0"
                "075i25z70j2mz9r7i9p9r521y8xdj81q7skslyb7zhqnnw33fw22"))

(define rust-futures-channel-0.3.30
  (crate-source "futures-channel" "0.3.30"
                "0y6b7xxqdjm9hlcjpakcg41qfl7lihf6gavk8fyqijsxhvbzgj7a"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.30
  (crate-source "futures-core" "0.3.30"
                "07aslayrn3lbggj54kci0ishmd1pr367fp7iks7adia1p05miinz"))

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

(define rust-futures-task-0.3.30
  (crate-source "futures-task" "0.3.30"
                "013h1724454hj8qczp8vvs10qfiqrxr937qsrv6rhii68ahlzn1q"))

(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))

(define rust-futures-util-0.3.30
  (crate-source "futures-util" "0.3.30"
                "0j0xqhcir1zf2dcbpd421kgw6wvsk0rpxflylcysn1rlp3g02r1x"))

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-garando-errors-0.1.0
  (crate-source "garando_errors" "0.1.0"
                "0si5cmcy9fm9pdbxwaqjbvbyiw4gj5i2hkgyks024ngdmk25wj8q"))

(define rust-garando-pos-0.1.0
  (crate-source "garando_pos" "0.1.0"
                "1bqa49pxd70mfqkri6swz4vr4dvgxb9aixdvxzd8d96wfpy8d4qc"))

(define rust-garando-syntax-0.1.1
  (crate-source "garando_syntax" "0.1.1"
                "0ayk80px1kwkqc6an0nf2j3k7n1d4pnb3g0v4n6cfbyic4w3i2hx"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-gethostname-1.0.2
  (crate-source "gethostname" "1.0.2"
                "0mdfkmfr41xx1i0nlwgzncmnj2a5f18kn6ydp7j1qc1q83dpy9gw"))

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-getrandom-0.2.16
  (crate-source "getrandom" "0.2.16"
                "14l5aaia20cc6cc08xdlhrzmfcylmrnprwnna20lqf746pqzjprk"))

(define rust-getrandom-0.2.17
  (crate-source "getrandom" "0.2.17"
                "1l2ac6jfj9xhpjjgmcx6s1x89bbnw9x6j9258yy6xjkzpq0bqapz"))

(define rust-getrandom-0.3.3
  (crate-source "getrandom" "0.3.3"
                "1x6jl875zp6b2b6qp9ghc84b0l76bvng2lvm8zfcmwjl7rb5w516"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-getrandom-0.4.1
  (crate-source "getrandom" "0.4.1"
                "1v7fm84f2jh6x7w3bd2ncl3sw29wnb0rhg7xya1pd30i02cg77hk"))

(define rust-getrandom-0.4.2
  (crate-source "getrandom" "0.4.2"
                "0mb5833hf9pvn9dhvxjgfg5dx0m77g8wavvjdpvpnkp9fil1xr8d"))

(define rust-gimli-0.32.3
  (crate-source "gimli" "0.32.3"
                "1iqk5xznimn5bfa8jy4h7pa1dv3c624hzgd2dkz8mpgkiswvjag6"))

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

(define rust-glob-0.3.3
  (crate-source "glob" "0.3.3"
                "106jpd3syfzjfj2k70mwm0v436qbx96wig98m4q8x071yrq35hhc"))

(define rust-globset-0.4.14
  (crate-source "globset" "0.4.14"
                "1qab0c1drpybgm4nc92lf8b46x0ap44c9y4k23rndgc5bfdkpnjp"))

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

(define rust-goblin-0.10.5
  (crate-source "goblin" "0.10.5"
                "1nawdq2lmd85a7zlaf9m8zkqqaglk8c9xlvqx90lqbdinfpnlflq"))

(define rust-guppy-0.17.25
  (crate-source "guppy" "0.17.25"
                "05g10jwn0907ij3rjvjlskywmjgrri5z04nxhpal4sh7f9srf0m5"))

(define rust-guppy-workspace-hack-0.1.0
  (crate-source "guppy-workspace-hack" "0.1.0"
                "106pg6ifjq92rz5xbbv0aw4xchl1fkikpjry72p0nxczv620cqlj"))

(define rust-hashbrown-0.12.3
  (crate-source "hashbrown" "0.12.3"
                "1268ka4750pyg2pbgsr43f0289l5zah4arir2k4igx5a8c6fg7la"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.0
  (crate-source "hashbrown" "0.15.0"
                "1yx4xq091s7i6mw6bn77k8cp4jrpcac149xr32rg8szqsj27y20y"))

(define rust-hashbrown-0.15.2
  (crate-source "hashbrown" "0.15.2"
                "12dj0yfn59p3kh3679ac0w1fagvzf4z2zp87a13gbbqbzw0185dz"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.16.0
  (crate-source "hashbrown" "0.16.0"
                "13blh9j2yv77a6ni236ixiwdzbc1sh2bc4bdpaz7y859yv2bs6al"))

(define rust-hashbrown-0.16.1
  (crate-source "hashbrown" "0.16.1"
                "004i3njw38ji3bzdp9z178ba9x3k0c1pgy8x69pj7yfppv4iq7c4"))

(define rust-hashlink-0.10.0
  (crate-source "hashlink" "0.10.0"
                "1h8lzvnl9qxi3zyagivzz2p1hp6shgddfmccyf6jv7s1cdicz0kk"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.1.19
  (crate-source "hermit-abi" "0.1.19"
                "0cxcm8093nf5fyn114w8vxbrbcyvv91d4015rdnlgfll7cs6gd32"))

(define rust-hermit-abi-0.3.9
  (crate-source "hermit-abi" "0.3.9"
                "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-hmac-0.12.1
  (crate-source "hmac" "0.12.1"
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))

(define rust-home-0.5.12
  (crate-source "home" "0.5.12"
                "13bjyzgx6q9srnfvl43dvmhn93qc8mh5w7cylk2g13sj3i3pyqnc"))

(define rust-homedir-0.3.4
  (crate-source "homedir" "0.3.4"
                "18kb7myfvzzixv02k066477k11zzbaj2yddarjbrcx65r1dvvnsv"))

(define rust-http-1.1.0
  (crate-source "http" "1.1.0"
                "0n426lmcxas6h75c2cp25m933pswlrfjz10v91vc62vib2sdvf91"))

(define rust-http-body-1.0.0
  (crate-source "http-body" "1.0.0"
                "0hyn8n3iadrbwq8y0p1rl1275s4nm49bllw5wji29g4aa3dqbb0w"))

(define rust-http-body-util-0.1.3
  (crate-source "http-body-util" "0.1.3"
                "0jm6jv4gxsnlsi1kzdyffjrj8cfr3zninnxpw73mvkxy4qzdj8dh"))

(define rust-httparse-1.10.1
  (crate-source "httparse" "1.10.1"
                "11ycd554bw2dkgw0q61xsa7a4jn1wb1xbfacmf3dbwsikvkkvgvd"))

(define rust-humansize-2.1.3
  (crate-source "humansize" "2.1.3"
                "1msxd1akb3dydsa8qs461sds9krwnn31szvqgaq93p4x0ad1rdbc"))

(define rust-humantime-2.1.0
  (crate-source "humantime" "2.1.0"
                "1r55pfkkf5v0ji1x6izrjwdq9v6sc7bv99xj6srywcar37xmnfls"))

(define rust-humantime-2.3.0
  (crate-source "humantime" "2.3.0"
                "092lpipp32ayz4kyyn4k3vz59j9blng36wprm5by0g2ykqr14nqk"))

(define rust-hyper-1.6.0
  (crate-source "hyper" "1.6.0"
                "103ggny2k31z0iq2gzwk2vbx601wx6xkpjpxn40hr3p3b0b5fayc"))

(define rust-hyper-rustls-0.27.2
  (crate-source "hyper-rustls" "0.27.2"
                "0ma1wyfnqnkz7zyr7wpply3xfvlijd0rqqhb6ajs28c9jhnbxr2y"))

(define rust-hyper-util-0.1.17
  (crate-source "hyper-util" "0.1.17"
                "1a5fcnz0alrg4lx9xf6ja66ihaab58jnm5msnky804wg39cras9w"))

(define rust-iana-time-zone-0.1.60
  (crate-source "iana-time-zone" "0.1.60"
                "0hdid5xz3jznm04lysjm3vi93h3c523w0hcc3xba47jl3ddbpzz7"))

(define rust-iana-time-zone-0.1.64
  (crate-source "iana-time-zone" "0.1.64"
                "1yz980fmhaq9bdkasz35z63az37ci6kzzfhya83kgdqba61pzr9k"))

(define rust-iana-time-zone-0.1.65
  (crate-source "iana-time-zone" "0.1.65"
                "0w64khw5p8s4nzwcf36bwnsmqzf61vpwk9ca1920x82bk6nwj6z3"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-icu-collections-1.5.0
  (crate-source "icu_collections" "1.5.0"
                "09j5kskirl59mvqc8kabhy7005yyy7dp88jw9f6f3gkf419a8byv"))

(define rust-icu-collections-2.1.1
  (crate-source "icu_collections" "2.1.1"
                "0hsblchsdl64q21qwrs4hvc2672jrf466zivbj1bwyv606bn8ssc"))

(define rust-icu-locale-core-2.1.1
  (crate-source "icu_locale_core" "2.1.1"
                "1djvdc2f5ylmp1ymzv4gcnmq1s4hqfim9nxlcm173lsd01hpifpd"))

(define rust-icu-locid-1.5.0
  (crate-source "icu_locid" "1.5.0"
                "0dznvd1c5b02iilqm044q4hvar0sqibq1z46prqwjzwif61vpb0k"))

(define rust-icu-locid-transform-1.5.0
  (crate-source "icu_locid_transform" "1.5.0"
                "0kmmi1kmj9yph6mdgkc7v3wz6995v7ly3n80vbg0zr78bp1iml81"))

(define rust-icu-locid-transform-data-1.5.0
  (crate-source "icu_locid_transform_data" "1.5.0"
                "0vkgjixm0wzp2n3v5mw4j89ly05bg3lx96jpdggbwlpqi0rzzj7x"))

(define rust-icu-normalizer-1.5.0
  (crate-source "icu_normalizer" "1.5.0"
                "0kx8qryp8ma8fw1vijbgbnf7zz9f2j4d14rw36fmjs7cl86kxkhr"))

(define rust-icu-normalizer-2.1.1
  (crate-source "icu_normalizer" "2.1.1"
                "16dmn5596la2qm0r3vih0bzjfi0vx9a20yqjha6r1y3vnql8hv2z"))

(define rust-icu-normalizer-data-1.5.0
  (crate-source "icu_normalizer_data" "1.5.0"
                "05lmk0zf0q7nzjnj5kbmsigj3qgr0rwicnn5pqi9n7krmbvzpjpq"))

(define rust-icu-normalizer-data-2.1.1
  (crate-source "icu_normalizer_data" "2.1.1"
                "02jnzizg6q75m41l6c13xc7nkc5q8yr1b728dcgfhpzw076wrvbs"))

(define rust-icu-properties-1.5.1
  (crate-source "icu_properties" "1.5.1"
                "1xgf584rx10xc1p7zjr78k0n4zn3g23rrg6v2ln31ingcq3h5mlk"))

(define rust-icu-properties-2.1.2
  (crate-source "icu_properties" "2.1.2"
                "1v3lbmhhi7i6jgw51ikjb1p50qh5rb67grlkdnkc63l7zq1gq2q2"))

(define rust-icu-properties-data-1.5.0
  (crate-source "icu_properties_data" "1.5.0"
                "0scms7pd5a7yxx9hfl167f5qdf44as6r3bd8myhlngnxqgxyza37"))

(define rust-icu-properties-data-2.1.2
  (crate-source "icu_properties_data" "2.1.2"
                "1bvpkh939rgzrjfdb7hz47v4wijngk0snmcgrnpwc9fpz162jv31"))

(define rust-icu-provider-1.5.0
  (crate-source "icu_provider" "1.5.0"
                "1nb8vvgw8dv2inqklvk05fs0qxzkw8xrg2n9vgid6y7gm3423m3f"))

(define rust-icu-provider-2.1.1
  (crate-source "icu_provider" "2.1.1"
                "0576b7dizgyhpfa74kacv86y4g1p7v5ffd6c56kf1q82rvq2r5l5"))

(define rust-icu-provider-macros-1.5.0
  (crate-source "icu_provider_macros" "1.5.0"
                "1mjs0w7fcm2lcqmbakhninzrjwqs485lkps4hz0cv3k36y9rxj0y"))

(define rust-id-arena-2.3.0
  (crate-source "id-arena" "2.3.0"
                "0m6rs0jcaj4mg33gkv98d71w3hridghp5c4yr928hplpkgbnfc1x"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

(define rust-idna-adapter-1.2.0
  (crate-source "idna_adapter" "1.2.0"
                "0wggnkiivaj5lw0g0384ql2d7zk4ppkn3b1ry4n0ncjpr7qivjns"))

(define rust-idna-adapter-1.2.1
  (crate-source "idna_adapter" "1.2.1"
                "0i0339pxig6mv786nkqcxnwqa87v4m94b2653f6k3aj0jmhfkjis"))

(define rust-ignore-0.4.22
  (crate-source "ignore" "0.4.22"
                "1wcaqpi6djqgi1brghrdyw4d5qgnwzhqrqyn4mar4vp677gi0s5l"))

(define rust-ignore-0.4.23
  (crate-source "ignore" "0.4.23"
                "0jysggjfmlxbg60vhhiz4pb8jfb7cnq5swdsvxknbs7x18wgv2bd"))

(define rust-image-0.25.4
  (crate-source "image" "0.25.4"
                "1bmchl9nknhd7jkbizzjxg28vzsm5x9ks2b4rq13nx8xld24s55w"))

(define rust-include-dir-0.7.4
  (crate-source "include_dir" "0.7.4"
                "1pfh3g45z88kwq93skng0n6g3r7zkhq9ldqs9y8rvr7i11s12gcj"))

(define rust-include-dir-macros-0.7.4
  (crate-source "include_dir_macros" "0.7.4"
                "0x8smnf6knd86g69p19z5lpfsaqp8w0nx14kdpkz1m8bxnkqbavw"))

(define rust-include-flate-0.3.1
  (crate-source "include-flate" "0.3.1"
                "167r4qx7yfs4vphrpgh98ixkmd94jy63a76sghg64ak8rav7q6z0"))

(define rust-include-flate-codegen-0.3.1
  (crate-source "include-flate-codegen" "0.3.1"
                "0l40qk0p1pi020v3y5ywh6jfzwgafyli9fkfds6ldgmffi9byjag"))

(define rust-include-flate-compress-0.3.1
  (crate-source "include-flate-compress" "0.3.1"
                "1g2dhaizqw9ixpyi751fgrj4yaji47crrdyvylqmkkbbf47a9rpa"))

(define rust-indexmap-1.9.3
  (crate-source "indexmap" "1.9.3"
                "16dxmy7yvk51wvnih3a3im6fp5lmx0wx76i03n06wyak6cwhw1xx"))

(define rust-indexmap-2.11.4
  (crate-source "indexmap" "2.11.4"
                "1rc8bgcjzfcskz1zipjjm7s3m1jskzhnhr9jxmsafhdk1xv863sb"))

(define rust-indexmap-2.13.0
  (crate-source "indexmap" "2.13.0"
                "05qh5c4h2hrnyypphxpwflk45syqbzvqsvvyxg43mp576w2ff53p"))

(define rust-indexmap-2.6.0
  (crate-source "indexmap" "2.6.0"
                "1nmrwn8lbs19gkvhxaawffzbvrpyrb5y3drcrr645x957kz0fybh"))

(define rust-indoc-2.0.7
  (crate-source "indoc" "2.0.7"
                "01np60qdq6lvgh8ww2caajn9j4dibx9n58rvzf7cya1jz69mrkvr"))

(define rust-inout-0.1.3
  (crate-source "inout" "0.1.3"
                "1xf9gf09nc7y1a261xlfqsf66yn6mb81ahlzzyyd1934sr9hbhd0"))

(define rust-insta-1.45.1
  (crate-source "insta" "1.45.1"
                "0p3syq3rpyp90c713wdr7cazgfxgcxym67qmcn5ap10c6lj3nglq"))

(define rust-insta-1.46.3
  (crate-source "insta" "1.46.3"
                "1r0mc4sjayarbl5cbizk4wa0hwwakcwj836f6k5ww73zgk4bhbg8"))

(define rust-ipnet-2.9.0
  (crate-source "ipnet" "2.9.0"
                "1hzrcysgwf0knf83ahb3535hrkw63mil88iqc6kjaryfblrqylcg"))

(define rust-iri-string-0.7.10
  (crate-source "iri-string" "0.7.10"
                "06kk3a5jz576p7vrpf7zz9jv3lrgcyp7pczcblcxdnryg3q3h4y9"))

(define rust-is-ci-1.2.0
  (crate-source "is_ci" "1.2.0"
                "0ifwvxmrsj4r29agfzr71bjq6y1bihkx38fbzafq5vl0jn1wjmbn"))

(define rust-is-terminal-0.4.12
  (crate-source "is-terminal" "0.4.12"
                "12vk6g0f94zlxl6mdh5gc4jdjb469n9k9s7y3vb0iml05gpzagzj"))

(define rust-is-terminal-polyfill-1.70.0
  (crate-source "is_terminal_polyfill" "1.70.0"
                "0018q5cf3rifbnzfc1w1z1xcx9c6i7xlywp2n0fw4limq1vqaizq"))

(define rust-is-terminal-polyfill-1.70.2
  (crate-source "is_terminal_polyfill" "1.70.2"
                "15anlc47sbz0jfs9q8fhwf0h3vs2w4imc030shdnq54sny5i7jx6"))

(define rust-itertools-0.11.0
  (crate-source "itertools" "0.11.0"
                "0mzyqcc59azx9g5cg6fs8k529gvh4463smmka6jvzs3cd2jp7hdi"))

(define rust-itertools-0.12.1
  (crate-source "itertools" "0.12.1"
                "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))

(define rust-itertools-0.13.0
  (crate-source "itertools" "0.13.0"
                "11hiy3qzl643zcigknclh446qb9zlg4dpdzfkjaa9q9fqpgyfgj1"))

(define rust-itertools-0.14.0
  (crate-source "itertools" "0.14.0"
                "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))

(define rust-itoa-1.0.11
  (crate-source "itoa" "1.0.11"
                "0nv9cqjwzr3q58qz84dcz63ggc54yhf1yqar1m858m1kfd4g3wa9"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-itoa-1.0.17
  (crate-source "itoa" "1.0.17"
                "1lh93xydrdn1g9x547bd05g0d3hra7pd1k4jfd2z1pl1h5hwdv4j"))

(define rust-jni-0.21.1
  (crate-source "jni" "0.21.1"
                "15wczfkr2r45slsljby12ymf2hij8wi5b104ghck9byjnwmsm1qs"))

(define rust-jni-sys-0.3.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "jni-sys" "0.3.0"
                "0c01zb9ygvwg9wdx2fii2d39myzprnpqqhy7yizxvjqp5p04pbwf"))

(define rust-jobserver-0.1.31
  (crate-source "jobserver" "0.1.31"
                "0vnyfxr5gm03j3lpnd1zswnyvqa40kbssy08pz2m35salfm9kc6j"))

(define rust-jobserver-0.1.34
  (crate-source "jobserver" "0.1.34"
                "0cwx0fllqzdycqn4d6nb277qx5qwnmjdxdl0lxkkwssx77j3vyws"))

(define rust-js-sys-0.3.69
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.69"
                "0v99rz97asnzapb0jsc3jjhvxpfxr7h7qd97yqyrf9i7viimbh99"))

(define rust-js-sys-0.3.81
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.81"
                "01ckbf16iwh7qj92fax9zh8vf2y9sk60cli6999cn7a1jxx96j7c"))

(define rust-js-sys-0.3.86
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.86"
                "1r9007pbs322i7wy1rkn0fd97m289sq106a1db4c0hkwr7qkjqfk"))

(define rust-js-sys-0.3.91
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.91"
                "171rzgq33wc1nxkgnvhlqqwwnrifs13mg3jjpjj5nf1z0yvib5xl"))

(define rust-keccak-0.1.6
  (crate-source "keccak" "0.1.6"
                "0lynp77kk3xw5kbdnmpc4wzx3qqn9cyfvg5prfb3sfnfik4ww9nb"))

(define rust-lazy-static-1.4.0
  (crate-source "lazy_static" "1.4.0"
                "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-lazycell-1.3.0
  (crate-source "lazycell" "1.3.0"
                "0m8gw7dn30i0zjjpjdyf6pc16c34nl71lpv461mix50x3p70h3c3"))

(define rust-leb128fmt-0.1.0
  (crate-source "leb128fmt" "0.1.0"
                "1chxm1484a0bly6anh6bd7a99sn355ymlagnwj3yajafnpldkv89"))

(define rust-lexopt-0.3.0
  (crate-source "lexopt" "0.3.0"
                "00dlvik2ygw8z101vf3bfndcvxhp92v25sbzz6bdiwvxgxhlpzxs"))

(define rust-libbz2-rs-sys-0.2.2
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libbz2-rs-sys" "0.2.2"
                "1xz88pa6xc372kjnr9gv4qaz5myjna9d7db5a2a7sk142md58jic"))

(define rust-libc-0.2.159
  (crate-source "libc" "0.2.159"
                "1i9xpia0hn1y8dws7all8rqng6h3lc8ymlgslnljcvm376jrf7an"))

(define rust-libc-0.2.174
  (crate-source "libc" "0.2.174"
                "0xl7pqvw7g2874dy3kjady2fjr4rhj5lxsnxkkhr5689jcr6jw8i"))

(define rust-libc-0.2.176
  (crate-source "libc" "0.2.176"
                "0x7ivn80h7nz2l46vra7bxx36s6r8d0lkax14dx97skjsss2kyaq"))

(define rust-libc-0.2.183
  (crate-source "libc" "0.2.183"
                "17c9gyia7rrzf9gsssvk3vq9ca2jp6rh32fsw6ciarpn5djlddmm"))

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

(define rust-libloading-0.8.9
  (crate-source "libloading" "0.8.9"
                "0mfwxwjwi2cf0plxcd685yxzavlslz7xirss3b9cbrzyk4hv1i6p"))

(define rust-libm-0.2.15
  (crate-source "libm" "0.2.15"
                "1plpzf0p829viazdj57yw5dhmlr8ywf3apayxc2f2bq5a6mvryzr"))

(define rust-libm-0.2.8
  (crate-source "libm" "0.2.8"
                "0n4hk1rs8pzw8hdfmwn96c4568s93kfxqgcqswr7sajd2diaihjf"))

(define rust-libredox-0.1.10
  (crate-source "libredox" "0.1.10"
                "1jswil4ai90s4rh91fg8580x8nikni1zl3wnch4h01nvidqpwvs1"))

(define rust-libredox-0.1.14
  (crate-source "libredox" "0.1.14"
                "02p3pxlqf54znf1jhiyyjs0i4caf8ckrd5l8ygs4i6ba3nfy6i0p"))

(define rust-libsqlite3-sys-0.35.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libsqlite3-sys" "0.35.0"
                "0gy1m6j1l94fxsirzp4h4rkrksf78rz7jy3px57qd1rcd8m1hg0k"))

(define rust-link-cplusplus-1.0.12
  (crate-source "link-cplusplus" "1.0.12"
                "10lcgfp9pnxpihp21s86xnq57vpr97m2k419d8rvkl57m8qcfy3z"))

(define rust-linux-raw-sys-0.11.0
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.11.0"
                "0fghx0nn8nvbz5yzgizfcwd6ap2pislp68j8c1bwyr6sacxkq7fz"))

(define rust-linux-raw-sys-0.12.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.12.1"
                "0lwasljrqxjjfk9l2j8lyib1babh2qjlnhylqzl01nihw14nk9ij"))

(define rust-linux-raw-sys-0.4.14
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.14"
                "12gsjgbhhjwywpqcrizv80vrp7p7grsz5laqq773i33wphjsxcvq"))

(define rust-linux-raw-sys-0.4.15
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.15"
                "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j"))

(define rust-linux-raw-sys-0.9.4
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.9.4"
                "04kyjdrq79lz9ibrf7czk6cv9d3jl597pb9738vzbsbzy1j5i56d"))

(define rust-litemap-0.7.3
  (crate-source "litemap" "0.7.3"
                "0157lf44c3s2piqiwpppnynzzpv1rxyddl2z9l089hpwsjwb0g34"))

(define rust-litemap-0.8.1
  (crate-source "litemap" "0.8.1"
                "0xsy8pfp9s802rsj1bq2ys2kbk1g36w5dr3gkfip7gphb5x60wv3"))

(define rust-litrs-0.4.2
  (crate-source "litrs" "0.4.2"
                "1v8bxsrkm0w2k9nmbp8hsspy9i1lawajywqdw4hx87rjzqv41rgm"))

(define rust-lock-api-0.4.12
  (crate-source "lock_api" "0.4.12"
                "05qvxa6g27yyva25a5ghsg85apdxkvr77yhkyhapj6r8vnf8pbq7"))

(define rust-log-0.3.9
  (crate-source "log" "0.3.9"
                "0jq23hhn5h35k7pa8r7wqnsywji6x3wn1q5q7lif5q536if8v7p1"))

(define rust-log-0.4.21
  (crate-source "log" "0.4.21"
                "074hldq1q8rlzq2s2qa8f25hj4s3gpw71w64vdwzjd01a4g8rvch"))

(define rust-log-0.4.28
  (crate-source "log" "0.4.28"
                "0cklpzrpxafbaq1nyxarhnmcw9z3xcjrad3ch55mmr58xw2ha21l"))

(define rust-log-0.4.29
  (crate-source "log" "0.4.29"
                "15q8j9c8g5zpkcw0hnd6cf2z7fxqnvsjh3rw5mv5q10r83i34l2y"))

(define rust-lru-slab-0.1.2
  (crate-source "lru-slab" "0.1.2"
                "0m2139k466qj3bnpk66bwivgcx3z88qkxvlzk70vd65jq373jaqi"))

(define rust-lzma-rust-0.1.7
  (crate-source "lzma-rust" "0.1.7"
                "0q96pl4wfd4hsdpkidrgv5bkk40f4xwzzsbisr2a2xbxpnxv5ajv"))

(define rust-lzma-sys-0.1.20
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "lzma-sys" "0.1.20"
                "09sxp20waxyglgn3cjz8qjkspb3ryz2fwx4rigkwvrk46ymh9njz"))

(define rust-mach-object-0.1.17
  (crate-source "mach_object" "0.1.17"
                "0s303n8174jn2k1hk95bdic1dr644yfjqnh8b2pjfh5rfrqjsvwb"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-matchers-0.2.0
  (crate-source "matchers" "0.2.0"
                "1sasssspdj2vwcwmbq3ra18d3qniapkimfcbr47zmx6750m5llni"))

(define rust-memchr-2.7.2
  (crate-source "memchr" "2.7.2"
                "07bcqxb0vx4ji0648ny5xsicjnpma95x1n07v7mi7jrhsz2l11kc"))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"))

(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))

(define rust-memchr-2.8.0
  (crate-source "memchr" "2.8.0"
                "0y9zzxcqxvdqg6wyag7vc3h0blhdn7hkq164bxyx2vph8zs5ijpq"))

(define rust-memo-map-0.3.2
  (crate-source "memo-map" "0.3.2"
                "10va9wc8jbc6vs0924ak0ac10rdw7i3h6c9jrga657pi5mdk6k1p"))

(define rust-miette-5.10.0
  (crate-source "miette" "5.10.0"
                "0vl5qvl3bgha6nnkdl7kiha6v4ypd6d51wyc4q1bvdpamr75ifsr"))

(define rust-miette-7.6.0
  (crate-source "miette" "7.6.0"
                "1dwjnnpcff4jzpf5ns1m19di2p0n5j31zmjv5dskrih7i3nfz62z"))

(define rust-miette-derive-5.10.0
  (crate-source "miette-derive" "5.10.0"
                "0p33msrngkxlp5ajm8nijamii9vcwwpy8gfh4m53qnmrc0avrrs9"))

(define rust-miette-derive-7.6.0
  (crate-source "miette-derive" "7.6.0"
                "12w13a67n2cc37nzidvv0v0vrvf4rsflzxz6slhbn3cm9rqjjnyv"))

(define rust-migrations-internals-2.3.0
  (crate-source "migrations_internals" "2.3.0"
                "03g1g9aaq4zmvgkab2b61x3jfdvjsw2h8a13bzs9jz4pvzn93irn"))

(define rust-migrations-macros-2.3.0
  (crate-source "migrations_macros" "2.3.0"
                "00r7511bl17317dvmblaxla4yg2xbpghzkzjsg9cy973dg3mmz1n"))

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-minijinja-2.16.0
  (crate-source "minijinja" "2.16.0"
                "1kif0icv5km9aprb1w2z5mkqc0bi1gyjkaawdd4p9p9lq2yg6m2w"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-mio-1.0.1
  (crate-source "mio" "1.0.1"
                "1r5g65s5acsx440m0a3pylclbrd0dqz93hg15k9crpllsdbf8sa5"))

(define rust-mustache-0.9.0
  (crate-source "mustache" "0.9.0"
                "1dfakqld6zf995nnkgs9ybccgps4zcbfd4adaa2162njqpqnx5ai"))

(define rust-mysqlclient-src-0.2.0+9.5.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "mysqlclient-src" "0.2.0+9.5.0"
                "14yv6x986m68b3ymyvj8vv9i10nkssb0is6kbbxf35k4jafk25fb"))

(define rust-mysqlclient-sys-0.5.0 rust-mysqlclient-sys)

(define rust-nested-0.1.1
  (crate-source "nested" "0.1.1"
                "17lwhdw0z8c4g00yfdasxh4zc5dq1ccylmbb0n1zw1wgcc7l4aya"))

(define rust-newline-converter-0.3.0
  (crate-source "newline-converter" "0.3.0"
                "0zyw2hyjl89rj1zmp9n8fq69pbfp9zl1cbal73agxjxixjbv1dj7"))

(define rust-nix-0.29.0
  (crate-source "nix" "0.29.0"
                "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"))

(define rust-node-semver-2.2.0
  (crate-source "node-semver" "2.2.0"
                "19s0ms6i3jkrs4ajqrz30jf2mk2nb9xcikripb7x4dywllz266iv"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-normalize-line-endings-0.3.0
  (crate-source "normalize-line-endings" "0.3.0"
                "1gp52dfn2glz26a352zra8h04351icf0fkqzw1shkwrgh1vpz031"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-nu-ansi-term-0.50.1
  (crate-source "nu-ansi-term" "0.50.1"
                "16a3isvbxx8pa3lk71h3cq2fsx2d17zzq42j4mhpxy81gl2qx8nl"))

(define rust-nu-ansi-term-0.50.3
  (crate-source "nu-ansi-term" "0.50.3"
                "1ra088d885lbd21q1bxgpqdlk1zlndblmarn948jz2a40xsbjmvr"))

(define rust-num-conv-0.2.0
  (crate-source "num-conv" "0.2.0"
                "0l4hj7lp8zbb9am4j3p7vlcv47y9bbazinvnxx9zjhiwkibyr5yg"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-object-0.37.3
  (crate-source "object" "0.37.3"
                "1zikiy9xhk6lfx1dn2gn2pxbnfpmlkn0byd7ib1n720x0cgj0xpz"))

(define rust-once-cell-1.19.0
  (crate-source "once_cell" "1.19.0"
                "14kvw7px5z96dk4dwdm1r9cqhhy2cyj1l5n5b29mynbb8yr15nrz"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-polyfill-1.70.2
  (crate-source "once_cell_polyfill" "1.70.2"
                "1zmla628f0sk3fhjdjqzgxhalr2xrfna958s632z65bjsfv8ljrq"))

(define rust-openssl-probe-0.2.1
  (crate-source "openssl-probe" "0.2.1"
                "1gpwpb7smfhkscwvbri8xzbab39wcnby1jgz1s49vf1aqgsdx1vw"))

(define rust-openssl-src-300.5.5+3.5.5
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "openssl-src" "300.5.5+3.5.5"
                "02gpasd6j7iv0pw8jxzvqpn993njy1jsgl2gjfkrfdg06gaqf5rz"))

(define rust-openssl-sys-0.9.111 rust-openssl-sys)

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-oro-common-0.3.34
  (crate-source "oro-common" "0.3.34"
                "14in5hsgriqhk1wkms9ahp5f97h5rmskazkxslhhg8agbdwcsb72"))

(define rust-oro-package-spec-0.3.34
  (crate-source "oro-package-spec" "0.3.34"
                "1cjwjrcidhd8k0h4lspf6ch5c62irvrbpyrn5c1m58mds86vqwyi"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"))

(define rust-owo-colors-4.0.0
  (crate-source "owo-colors" "4.0.0"
                "0grsk47cllj0s4nc4qxvy4gdhj2lyiglbqx4lmw2m7grdmq59zya"))

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

(define rust-parking-lot-0.12.2
  (crate-source "parking_lot" "0.12.2"
                "1ys2dzz6cysjmwyivwxczl1ljpcf5cj4qmhdj07d5bkc9z5g0jky"))

(define rust-parking-lot-core-0.9.10
  (crate-source "parking_lot_core" "0.9.10"
                "1y3cf9ld9ijf7i4igwzffcn0xl16dxyn4c5bwgjck1dkgabiyh0y"))

(define rust-parse-changelog-0.6.15
  (crate-source "parse-changelog" "0.6.15"
                "1lkc6qwky4mn5m9arqhs81g4rnah05cmsn2xjk9spc3h921iq6ag"))

(define rust-parse-zoneinfo-0.3.1
  (crate-source "parse-zoneinfo" "0.3.1"
                "093cs8slbd6kyfi6h12isz0mnaayf5ha8szri1xrbqj4inqhaahz"))

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

(define rust-pathdiff-0.2.3
  (crate-source "pathdiff" "0.2.3"
                "1lrqp4ip05df8dzldq6gb2c1sq2gs54gly8lcnv3rhav1qhwx56z"))

(define rust-pbkdf2-0.12.2
  (crate-source "pbkdf2" "0.12.2"
                "1wms79jh4flpy1zi8xdp4h8ccxv4d85adc6zjagknvppc5vnmvgq"))

(define rust-percent-encoding-2.3.1
  (crate-source "percent-encoding" "2.3.1"
                "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-peresil-0.3.0
  (crate-source "peresil" "0.3.0"
                "0mwyw03yqp0yqdjf4a89vn86szxaksmxvgzv1j2nw69fsmp8hn7n"))

(define rust-pest-2.7.10
  (crate-source "pest" "2.7.10"
                "1s4fvis7h6l872g6nk17r130kcllj4c0hjvwkzd3hi196g3320an"))

(define rust-pest-2.8.2
  (crate-source "pest" "2.8.2"
                "1a6g94pr4npsg0s6ljddwp4g127ks0nygzhxcpwfmyik6yis7q11"))

(define rust-pest-derive-2.7.10
  (crate-source "pest_derive" "2.7.10"
                "0n8lsk9s21dp7958p9yarbk2gsc8wg0rvdzr7cd7pjpvjf8kqa96"))

(define rust-pest-derive-2.8.2
  (crate-source "pest_derive" "2.8.2"
                "0qy6nv84q1m6m2rzw1qjfbxlcizz7h557rkk16yivjqafxpp0n5w"))

(define rust-pest-generator-2.7.10
  (crate-source "pest_generator" "2.7.10"
                "11s6q0vf25lckbzak0qndzpv87ksaxy6pa9cvn2hlizvsgvjmhiy"))

(define rust-pest-generator-2.8.2
  (crate-source "pest_generator" "2.8.2"
                "0bws5i6g3v1sldvy66p7qbzmz5mqbiflcqilaywgf1zy3n0kckvd"))

(define rust-pest-meta-2.7.10
  (crate-source "pest_meta" "2.7.10"
                "1kdxl164yyjsmn01lvllsll4sz3xbgy4dmkq33n63hrp5w1418np"))

(define rust-pest-meta-2.8.2
  (crate-source "pest_meta" "2.8.2"
                "0844iv71ibf414yid0bvhi9i0zfi0jrmyh6mvjjx1jws102rp4a2"))

(define rust-petgraph-0.8.3
  (crate-source "petgraph" "0.8.3"
                "0mblnaqbx1y20h5y7pz6y11hk9jjk6k87lsmn7jxaq3hm67ba0c7"))

(define rust-phf-0.11.2
  (crate-source "phf" "0.11.2"
                "1p03rsw66l7naqhpgr1a34r9yzi1gv9jh16g3fsk6wrwyfwdiqmd"))

(define rust-phf-0.11.3
  (crate-source "phf" "0.11.3"
                "0y6hxp1d48rx2434wgi5g8j1pr8s5jja29ha2b65435fh057imhz"))

(define rust-phf-codegen-0.11.2
  (crate-source "phf_codegen" "0.11.2"
                "0nia6h4qfwaypvfch3pnq1nd2qj64dif4a6kai3b7rjrsf49dlz8"))

(define rust-phf-codegen-0.11.3
  (crate-source "phf_codegen" "0.11.3"
                "0si1n6zr93kzjs3wah04ikw8z6npsr39jw4dam8yi9czg2609y5f"))

(define rust-phf-generator-0.11.2
  (crate-source "phf_generator" "0.11.2"
                "1c14pjyxbcpwkdgw109f7581cc5fa3fnkzdq1ikvx7mdq9jcrr28"))

(define rust-phf-generator-0.11.3
  (crate-source "phf_generator" "0.11.3"
                "0gc4np7s91ynrgw73s2i7iakhb4lzdv1gcyx7yhlc0n214a2701w"))

(define rust-phf-shared-0.11.2
  (crate-source "phf_shared" "0.11.2"
                "0azphb0a330ypqx3qvyffal5saqnks0xvl8rj73jlk3qxxgbkz4h"))

(define rust-phf-shared-0.11.3
  (crate-source "phf_shared" "0.11.3"
                "1rallyvh28jqd9i916gk5gk2igdmzlgvv5q0l3xbf3m6y8pbrsk7"))

(define rust-pin-project-lite-0.2.14
  (crate-source "pin-project-lite" "0.2.14"
                "00nx3f04agwjlsmd3mc5rx5haibj2v8q9b52b0kwn63wcv4nz9mx"))

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pin-project-lite-0.2.17
  (crate-source "pin-project-lite" "0.2.17"
                "1kfmwvs271si96zay4mm8887v5khw0c27jc9srw1a75ykvgj54x8"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-pkg-config-0.3.30
  (crate-source "pkg-config" "0.3.30"
                "1v07557dj1sa0aly9c90wsygc0i8xv5vnmyv0g94lpkvj8qb4cfj"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-plain-0.2.3
  (crate-source "plain" "0.2.3"
                "19n1xbxb4wa7w891268bzf6cbwq4qvdb86bik1z129qb0xnnnndl"))

(define rust-potential-utf-0.1.4
  (crate-source "potential_utf" "0.1.4"
                "0xxg0pkfpq299wvwln409z4fk80rbv55phh3f1jhjajy5x1ljfdp"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.17
  (crate-source "ppv-lite86" "0.2.17"
                "1pp6g52aw970adv3x2310n7glqnji96z0a9wiamzw89ibf0ayh2v"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-pq-src-0.3.11+libpq-18.3
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "pq-src" "0.3.11+libpq-18.3"
                "03rdgildg8c5mxqwbxv15ypqpkifj5n7vkwavmqx6i9l1jzbzdbg"))

(define rust-pq-sys-0.7.5
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "pq-sys" "0.7.5"
                "0qdsb8l3pajad2270wbggk7l6334n0kaf0hb2hzl753j4rmdskap"))

(define rust-predicates-3.1.2
  (crate-source "predicates" "3.1.2"
                "15rcyjax4ykflw5425wsyzcfkgl08c9zsa8sdlsrmhj0fv68d43y"))

(define rust-predicates-core-1.0.6
  (crate-source "predicates-core" "1.0.6"
                "0x7ij95n63mhgkyrb7hly5ngm41mwfsassfvnjz7lbk10wk0755p"))

(define rust-predicates-tree-1.0.9
  (crate-source "predicates-tree" "1.0.9"
                "1kyfq3r0s2vg94a9r59n7ar5gv66zvpa0s1fd6mm4l4czcas72rn"))

(define rust-prettyplease-0.2.37
  (crate-source "prettyplease" "0.2.37"
                "0azn11i1kh0byabhsgab6kqs74zyrg69xkirzgqyhz6xmjnsi727"))

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

(define rust-proc-macro2-1.0.106
  (crate-source "proc-macro2" "1.0.106"
                "0d09nczyaj67x4ihqr5p7gxbkz38gxhk4asc0k8q23g9n85hzl4g"))

(define rust-proc-macro2-1.0.85
  (crate-source "proc-macro2" "1.0.85"
                "08zwg5l5f3czp62g4cvzgjwnk176lsrwq6kdi4x0arm9bbhlq912"))

(define rust-quick-error-1.2.3
  (crate-source "quick-error" "1.2.3"
                "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))

(define rust-quinn-0.11.9
  (crate-source "quinn" "0.11.9"
                "086gzj666dr3slmlynkvxlndy28hahgl361d6bf93hk3i6ahmqmr"))

(define rust-quinn-proto-0.11.13
  (crate-source "quinn-proto" "0.11.13"
                "0cca3mgja9p4w66f6sl1kfhj8rdf4mwsg1jxzssh9g63n14np47i"))

(define rust-quinn-udp-0.5.14
  (crate-source "quinn-udp" "0.5.14"
                "1gacawr17a2zkyri0r3m0lc9spzmxbq1by3ilyb8v2mdvjhcdpmd"))

(define rust-quote-1.0.36
  (crate-source "quote" "1.0.36"
                "19xcmh445bg6simirnnd4fvkmp6v2qiwxh5f6rw4a70h76pnm9qg"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-quote-1.0.45
  (crate-source "quote" "1.0.45"
                "095rb5rg7pbnwdp6v8w5jw93wndwyijgci1b5lw8j1h5cscn3wj1"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-r-efi-6.0.0
  (crate-source "r-efi" "6.0.0"
                "1gyrl2k5fyzj9k7kchg2n296z5881lg7070msabid09asp3wkp7q"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-0.9.2
  (crate-source "rand" "0.9.2"
                "1lah73ainvrgl7brcxx0pwhpnqa3sm3qaj672034jz8i0q7pgckd"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rand-core-0.9.5
  (crate-source "rand_core" "0.9.5"
                "0g6qc5r3f0hdmz9b11nripyp9qqrzb0xqk9piip8w8qlvqkcibvn"))

(define rust-redox-syscall-0.4.1
  (crate-source "redox_syscall" "0.4.1"
                "1aiifyz5dnybfvkk4cdab9p2kmphag1yad6iknc7aszlxxldf8j7"))

(define rust-redox-syscall-0.5.1
  (crate-source "redox_syscall" "0.5.1"
                "0zja6y3av9z50gg1hh0vsc053941wng21r43whhk8mfb9n4m5426"))

(define rust-redox-users-0.4.6
  (crate-source "redox_users" "0.4.6"
                "0hya2cxx6hxmjfxzv9n8rjl5igpychav7zfi1f81pz6i4krry05s"))

(define rust-ref-cast-1.0.25
  (crate-source "ref-cast" "1.0.25"
                "0zdzc34qjva9xxgs889z5iz787g81hznk12zbk4g2xkgwq530m7k"))

(define rust-ref-cast-impl-1.0.25
  (crate-source "ref-cast-impl" "1.0.25"
                "1nkhn1fklmn342z5c4mzfzlxddv3x8yhxwwk02cj06djvh36065p"))

(define rust-regex-1.10.4
  (crate-source "regex" "1.10.4"
                "0k5sb0h2mkwf51ab0gvv3x38jp1q7wgxf63abfbhi0wwvvgxn5y1"))

(define rust-regex-1.11.3
  (crate-source "regex" "1.11.3"
                "0b58ya98c4i5cjjiwhpcnjr61cv9g143qhdwhsryggj09098hllb"))

(define rust-regex-1.12.3
  (crate-source "regex" "1.12.3"
                "0xp2q0x7ybmpa5zlgaz00p8zswcirj9h8nry3rxxsdwi9fhm81z1"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.4.11
  (crate-source "regex-automata" "0.4.11"
                "1bawj908pxixpggcnma3xazw53mwyz68lv9hn4yg63nlhv7bjgl3"))

(define rust-regex-automata-0.4.14
  (crate-source "regex-automata" "0.4.14"
                "13xf7hhn4qmgfh784llcp2kzrvljd13lb2b1ca0mwnf15w9d87bf"))

(define rust-regex-automata-0.4.6
  (crate-source "regex-automata" "0.4.6"
                "1spaq7y4im7s56d1gxa2hi4hzf6dwswb1bv8xyavzya7k25kpf46"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.8.10
  (crate-source "regex-syntax" "0.8.10"
                "02jx311ka0daxxc7v45ikzhcl3iydjbbb0mdrpc1xgg8v7c7v2fw"))

(define rust-regex-syntax-0.8.3
  (crate-source "regex-syntax" "0.8.3"
                "0mhzkm1pkqg6y53xv056qciazlg47pq0czqs94cn302ckvi49bdd"))

(define rust-regex-syntax-0.8.6
  (crate-source "regex-syntax" "0.8.6"
                "00chjpglclfskmc919fj5aq308ffbrmcn7kzbkz92k231xdsmx6a"))

(define rust-reqwest-0.13.2
  (crate-source "reqwest" "0.13.2"
                "00d8xyrbcp0519rr9rhl685ymb6hi3lv0i2bca5lic9s53il6gxb"))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-rle-decode-fast-1.0.3
  (crate-source "rle-decode-fast" "1.0.3"
                "08kljzl29rpm12fiz0qj5pask49aiswdvcjigdcq73s224rgd0im"))

(define rust-rmp-0.8.14
  (crate-source "rmp" "0.8.14"
                "1i1l6dhv7vws5vp0ikakj44fk597xi59g3j6ng1q55x3dz0xg3i2"))

(define rust-rmp-serde-1.3.0
  (crate-source "rmp-serde" "1.3.0"
                "1nylmh7w2vpa1bwrnx1jfp2l4yz6i5qrmpic5zll166gfyj9kraj"))

(define rust-rsqlite-vfs-0.1.0
  (crate-source "rsqlite-vfs" "0.1.0"
                "0kap86yzwl355byfs891185h723nxvl746fdp8gnpvrna0qz58d8"))

(define rust-rust-embed-8.7.2
  (crate-source "rust-embed" "8.7.2"
                "12hprnl569f1pg2sn960gfla913mk1mxdwpn2a6vl9iad2w0hn82"))

(define rust-rust-embed-impl-8.7.2
  (crate-source "rust-embed-impl" "8.7.2"
                "171lshvdh122ypbf23gmhvrqnhbk0q9g27gaq6g82w9b76jg2rb0"))

(define rust-rust-embed-utils-8.7.2
  (crate-source "rust-embed-utils" "8.7.2"
                "151m1966qk75y10msazdp0xj4fqw1khcry0z946bf84bcj0hrk7n"))

(define rust-rustc-cfg-0.5.0
  (crate-source "rustc-cfg" "0.5.0"
                "0fk3g0dqg8yyz6g7hb95vvfl6c0iqvqrgawam2jh700y8ig7mpwx"))

(define rust-rustc-demangle-0.1.24
  (crate-source "rustc-demangle" "0.1.24"
                "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustc-hash-2.0.0
  (crate-source "rustc-hash" "2.0.0"
                "0lni0lf846bzrf3jvci6jaf4142n1mdqxvcpczk5ch9pfgyk8c2q"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rustix-0.38.34
  (crate-source "rustix" "0.38.34"
                "03vkqa2ism7q56rkifyy8mns0wwqrk70f4i4fd53r97p8b05xp3h"))

(define rust-rustix-0.38.37
  (crate-source "rustix" "0.38.37"
                "04b8f99c2g36gyggf4aphw8742k2b1vls3364n2z493whj5pijwa"))

(define rust-rustix-0.38.44
  (crate-source "rustix" "0.38.44"
                "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))

(define rust-rustix-1.0.8
  (crate-source "rustix" "1.0.8"
                "1j6ajqi61agdnh1avr4bplrsgydjw1n4mycdxw3v8g94pyx1y60i"))

(define rust-rustix-1.1.2
  (crate-source "rustix" "1.1.2"
                "0gpz343xfzx16x82s1x336n0kr49j02cvhgxdvaq86jmqnigh5fd"))

(define rust-rustix-1.1.4
  (crate-source "rustix" "1.1.4"
                "14511f9yjqh0ix07xjrjpllah3325774gfwi9zpq72sip5jlbzmn"))

(define rust-rustls-0.23.36
  (crate-source "rustls" "0.23.36"
                "06w0077ssk3blpp93613lkny046mwj0nhxjgc7cmg9nf70yz6rf6"))

(define rust-rustls-native-certs-0.8.3
  (crate-source "rustls-native-certs" "0.8.3"
                "0qrajg2n90bcr3bcq6j95gjm7a9lirfkkdmjj32419dyyzan0931"))

(define rust-rustls-pki-types-1.14.0
  (crate-source "rustls-pki-types" "1.14.0"
                "1p9zsgslvwzzkzhm6bqicffqndr4jpx67992b0vl0pi21a5hy15y"))

(define rust-rustls-platform-verifier-0.6.2
  (crate-source "rustls-platform-verifier" "0.6.2"
                "110pqkn3px9115pb6h6a23cq738v29gbp559dfvpmbibqzmzx68x"))

(define rust-rustls-platform-verifier-android-0.1.1
  (crate-source "rustls-platform-verifier-android" "0.1.1"
                "13vq6sxsgz9547xm2zbdxiw8x7ad1g8n8ax6xvxsjqszk7q6awgq"))

(define rust-rustls-webpki-0.103.9
  (crate-source "rustls-webpki" "0.103.9"
                "0lwg1nnyv7pp2lfwwjhy81bxm233am99jnsp3iymdhd6k8827pyp"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-ryu-1.0.18
  (crate-source "ryu" "1.0.18"
                "17xx2s8j1lln7iackzd9p0sv546vjq71i779gphjq923vjh5pjzk"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-saphyr-parser-0.0.6
  (crate-source "saphyr-parser" "0.0.6"
                "1xvadmva2dkvngglzv5wd8ji9yvwz4lfq9b3838qa6bbkysp3dsg"))

(define rust-schannel-0.1.23
  (crate-source "schannel" "0.1.23"
                "0d1m156bsjrws6xzzr1wyfyih9i22mb2csb5pc5kmkrvci2ibjgv"))

(define rust-schemars-1.2.1
  (crate-source "schemars" "1.2.1"
                "1k16qzpdpy6p9hrh18q2l6cwawxzyqi25f8masa13l0wm8v2zd52"))

(define rust-schemars-derive-1.2.1
  (crate-source "schemars_derive" "1.2.1"
                "0zrh1ckcc63sqy5hyhnh2lbxh4vmbij2z4f1g5za1vmayi85n4bx"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-scroll-0.13.0
  (crate-source "scroll" "0.13.0"
                "1pbs3vxhrxcvj9hbjw4hiijbqlz0lkfxc9351mv34hcb4ka7q9f1"))

(define rust-scroll-derive-0.13.1
  (crate-source "scroll_derive" "0.13.1"
                "0zd6xq7ffz8nbyjyl603dq16h5d8v95ljl89aw86daqk4gkfyxpd"))

(define rust-security-framework-3.7.0
  (crate-source "security-framework" "3.7.0"
                "07fd0j29j8yczb3hd430vwz784lx9knb5xwbvqna1nbkbivvrx5p"))

(define rust-security-framework-sys-2.17.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "security-framework-sys" "2.17.0"
                "1qr0w0y9iwvmv3hwg653q1igngnc5b74xcf0679cbv23z0fnkqkc"))

(define rust-self-cell-1.0.4
  (crate-source "self_cell" "1.0.4"
                "0jki9brixzzy032d799xspz1gikc5n2w81w8q8yyn8w6jxpsjsfk"))

(define rust-self-replace-1.5.0
  (crate-source "self-replace" "1.5.0"
                "1drganasvf5b0x6c9g60jkfhzjc9in3r6cznjfw0lhmbbrdq3v03"))

(define rust-semver-1.0.23
  (crate-source "semver" "1.0.23"
                "12wqpxfflclbq4dv8sa6gchdh92ahhwn4ci1ls22wlby3h57wsb1"))

(define rust-semver-1.0.27
  (crate-source "semver" "1.0.27"
                "1qmi3akfrnqc2hfkdgcxhld5bv961wbk8my3ascv5068mc5fnryp"))

(define rust-serde-1.0.210
  (crate-source "serde" "1.0.210"
                "0flc0z8wgax1k4j5bf2zyq48bgzyv425jkd5w0i6wbh7f8j5kqy8"))

(define rust-serde-1.0.227
  (crate-source "serde" "1.0.227"
                "0ia2p85z8ypyjvl7x6a0pxy5fgjd6c3hrd9a76slxvgvqqzy9v40"))

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-core-1.0.227
  (crate-source "serde_core" "1.0.227"
                "1r9vnglazz5vfpi32by80c1nig1jvy9h2hcyl9pci8h7nrsn4mvs"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.210
  (crate-source "serde_derive" "1.0.210"
                "07yzy4wafk79ps0hmbqmsqh5xjna4pm4q57wc847bb8gl3nh4f94"))

(define rust-serde-derive-1.0.227
  (crate-source "serde_derive" "1.0.227"
                "016y5ryfv99z7a1khyrmiws5zq6lc07xyaiqkc7cy9487f999rji"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-serde-derive-internals-0.29.1
  (crate-source "serde_derive_internals" "0.29.1"
                "04g7macx819vbnxhi52cx0nhxi56xlhrybgwybyy7fb9m4h6mlhq"))

(define rust-serde-json-1.0.128
  (crate-source "serde_json" "1.0.128"
                "1n43nia50ybpcfmh3gcw4lcc627qsg9nyakzwgkk9pm10xklbxbg"))

(define rust-serde-json-1.0.145
  (crate-source "serde_json" "1.0.145"
                "1767y6kxjf7gwpbv8bkhgwc50nhg46mqwm9gy9n122f7v1k6yaj0"))

(define rust-serde-json-1.0.149
  (crate-source "serde_json" "1.0.149"
                "11jdx4vilzrjjd1dpgy67x5lgzr0laplz30dhv75lnf5ffa07z43"))

(define rust-serde-regex-1.1.0
  (crate-source "serde_regex" "1.1.0"
                "1pxsnxb8c198szghk1hvzvhva36w2q5zs70hqkmdf5d89qd6y4x8"))

(define rust-serde-spanned-0.6.9
  (crate-source "serde_spanned" "0.6.9"
                "18vmxq6qfrm110caszxrzibjhy2s54n1g5w1bshxq9kjmz7y0hdz"))

(define rust-serde-spanned-1.0.4
  (crate-source "serde_spanned" "1.0.4"
                "0xkp0qdzams5sqwndbw3xrhf4c0bb5r46w2ywkp1aqsdb8ggkfzq"))

(define rust-serde-yaml-0.9.34+deprecated
  (crate-source "serde_yaml" "0.9.34+deprecated"
                "0isba1fjyg3l6rxk156k600ilzr8fp7crv82rhal0rxz5qd1m2va"))

(define rust-serde-yaml-bw-2.5.1
  (crate-source "serde_yaml_bw" "2.5.1"
                "0j1ln9ha730lwszlh9941wfpbajagnap7vi7xf0riydbj71q3p5k"))

(define rust-sha1-0.10.6
  (crate-source "sha1" "0.10.6"
                "1fnnxlfg08xhkmwf2ahv634as30l1i3xhlhkvxflmasi5nd85gz3"))

(define rust-sha2-0.10.8
  (crate-source "sha2" "0.10.8"
                "1j1x78zk9il95w9iv46dh9wm73r6xrgj32y6lzzw7bxws9dbfgbr"))

(define rust-sha2-0.10.9
  (crate-source "sha2" "0.10.9"
                "10xjj843v31ghsksd9sl9y12qfc48157j1xpb8v1ml39jy0psl57"))

(define rust-sha3-0.10.8
  (crate-source "sha3" "0.10.8"
                "0q5s3qlwnk8d5j34jya98j1v2p3009wdmnqdza3yydwgi8kjv1vm"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shell-words-1.1.0
  (crate-source "shell-words" "1.1.0"
                "1plgwx8r0h5ismbbp6cp03740wmzgzhip85k5hxqrrkaddkql614"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-registry-1.4.2
  (crate-source "signal-hook-registry" "1.4.2"
                "1cb5akgq8ajnd5spyn587srvs4n26ryq0p78nswffwhv46sf1sd9"))

(define rust-simd-adler32-0.3.7
  (crate-source "simd-adler32" "0.3.7"
                "1zkq40c3iajcnr5936gjp9jjh1lpzhy44p3dq3fiw75iwr1w2vfn"))

(define rust-similar-2.7.0
  (crate-source "similar" "2.7.0"
                "1aidids7ymfr96s70232s6962v5g9l4zwhkvcjp4c5hlb6b5vfxv"))

(define rust-similar-asserts-1.7.0
  (crate-source "similar-asserts" "1.7.0"
                "16pb45hmcl5hx3xc4bb48gzhr8q35zc2p8j7i1836zl15jb43d5m"))

(define rust-siphasher-0.3.11
  (crate-source "siphasher" "0.3.11"
                "03axamhmwsrmh0psdw3gf7c0zc4fyl5yjxfifz9qfka6yhkqid9q"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-slab-0.4.11
  (crate-source "slab" "0.4.11"
                "12bm4s88rblq02jjbi1dw31984w61y2ldn13ifk5gsqgy97f8aks"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-slug-0.1.5
  (crate-source "slug" "0.1.5"
                "1i68hkvpbf04ga5kcssyads2wdy0kyikbqgq0l069nn8r774mn9v"))

(define rust-slug-0.1.6
  (crate-source "slug" "0.1.6"
                "0977cyp88xrwbpmqwzafkvv8vm9i0gdb5zjskb6f6pg45vvq0al8"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-socket2-0.6.0
  (crate-source "socket2" "0.6.0"
                "01qqdzfnr0bvdwq6wl56c9c4m2cvbxn43dfpcv8gjx208sph8d93"))

(define rust-spdx-0.13.3
  (crate-source "spdx" "0.13.3"
                "1vf75k52h5489kh6rwcvvqkbsihddf61saz0j9hx1k8jklbnbmxg"))

(define rust-sqlite-wasm-rs-0.5.2
  (crate-source "sqlite-wasm-rs" "0.5.2"
                "06qq401l0xjgr61nv15z2s7yfg6cda7p4zdp56f0nsb77bnhchig"))

(define rust-sscanf-0.4.3
  (crate-source "sscanf" "0.4.3"
                "1w6lfy9sr1fh1ar3k68wjyscc9kpdi4ngygwixf0613aafdh1lfb"))

(define rust-sscanf-macro-0.4.3
  (crate-source "sscanf_macro" "0.4.3"
                "0dqsrabv6zmphzm0ssrq3h07gq67ccrp7kvn4kdbqjsp19iy1z6g"))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

(define rust-stable-deref-trait-1.2.1
  (crate-source "stable_deref_trait" "1.2.1"
                "15h5h73ppqyhdhx6ywxfj88azmrpml9gl6zp3pwy2malqa6vxqkc"))

(define rust-static-assertions-1.1.0
  (crate-source "static_assertions" "1.1.0"
                "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))

(define rust-strsim-0.10.0
  (crate-source "strsim" "0.10.0"
                "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-strsim-0.8.0
  (crate-source "strsim" "0.8.0"
                "0sjsm7hrvjdifz661pjxq5w4hf190hx53fra8dfvamacvff139cf"))

(define rust-subtle-2.5.0
  (crate-source "subtle" "2.5.0"
                "1g2yjs7gffgmdvkkq0wrrh0pxds3q0dv6dhkw9cdpbib656xdkc1"))

(define rust-supports-color-3.0.0
  (crate-source "supports-color" "3.0.0"
                "0kw5miaai8sarcikzdvsf2ys6rkakngyf2g4yifmgz0xc8ab6acq"))

(define rust-supports-hyperlinks-3.0.0
  (crate-source "supports-hyperlinks" "3.0.0"
                "1vh4wjflrpa0vadfirzn57glk1wwrdfxkxwgyqzmy7q4d18iw2ic"))

(define rust-supports-unicode-3.0.0
  (crate-source "supports-unicode" "3.0.0"
                "1qpc344453x3ai4k9iygxnbk6lr2nw5jflj8ns5q3dbcmwq1lh5p"))

(define rust-sxd-document-0.3.2
  (crate-source "sxd-document" "0.3.2"
                "0y10shqmy9xb73g403rg1108wsagny9d8jrcm081pbwzpqvjzn4l"))

(define rust-sxd-xpath-0.4.2
  (crate-source "sxd-xpath" "0.4.2"
                "1sin3g8lzans065gjcwrpm7gdpwdpdg4rpi91rlvb1q8sfjrvqrn"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.106
  (crate-source "syn" "2.0.106"
                "19mddxp1ia00hfdzimygqmr1jqdvyl86k48427bkci4d08wc9rzd"))

(define rust-syn-2.0.117
  (crate-source "syn" "2.0.117"
                "16cv7c0wbn8amxc54n4w15kxlx5ypdmla8s0gxr2l7bv7s0bhrg6"))

(define rust-syn-2.0.66
  (crate-source "syn" "2.0.66"
                "1xfgrprsbz8j31kabvfinb4fyhajlk2q7lxa18fb006yl90kyby4"))

(define rust-sync-wrapper-1.0.1
  (crate-source "sync_wrapper" "1.0.1"
                "150k6lwvr4nl237ngsz8fj5j78k712m4bggrfyjsidllraz5l1m7"))

(define rust-synstructure-0.13.1
  (crate-source "synstructure" "0.13.1"
                "0wc9f002ia2zqcbj0q2id5x6n7g1zjqba7qkg2mr0qvvmdk7dby8"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-system-deps-6.2.2
  (crate-source "system-deps" "6.2.2"
                "0j93ryw031n3h8b0nfpj5xwh3ify636xmv8kxianvlyyipmkbrd3"))

(define rust-tar-0.4.44
  (crate-source "tar" "0.4.44"
                "0yk69a8j9xv51mdcy0853jai5zh1pd9yn456q4cpmj0js9w3i1hx"))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-target-lexicon-0.13.3
  (crate-source "target-lexicon" "0.13.3"
                "0355pbycq0cj29h1rp176l57qnfwmygv7hwzchs7iq15gibn4zyz"))

(define rust-target-spec-3.5.7
  (crate-source "target-spec" "3.5.7"
                "06j19azf66j02ggwpki1msfapccfwi4pj41azdy2bdklwhy1fp2q"))

(define rust-temp-dir-0.2.0
  (crate-source "temp-dir" "0.2.0"
                "12fncrdanmxhw4y5162ddlhwymymwf7mi71vk366z6a9jrrzjvh1"))

(define rust-tempfile-3.10.1
  (crate-source "tempfile" "3.10.1"
                "1wdzz35ri168jn9al4s1g2rnsrr5ci91khgarc2rvpb3nappzdw5"))

(define rust-tempfile-3.13.0
  (crate-source "tempfile" "3.13.0"
                "0nyagmbd4v5g6nzfydiihcn6l9j1w9bxgzyca5lyzgnhcbyckwph"))

(define rust-tempfile-3.26.0
  (crate-source "tempfile" "3.26.0"
                "182lfcv9d5w9349i0rjlgn4431k2m3yqfn9ls84p9d3ifxv2r9w2"))

(define rust-tera-1.20.0
  (crate-source "tera" "1.20.0"
                "1vnj9imw2h9szkd1izsrhwrc9jvazvdsp84x65wg2rg88ldqb7db"))

(define rust-term-0.6.1
  (crate-source "term" "0.6.1"
                "1ddqxq9hrk8zqq1f8pqhz72vrlfc8vh2xcza2gb623z78lrkm1n0"))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-terminal-size-0.4.1
  (crate-source "terminal_size" "0.4.1"
                "1sd4nq55h9sjirkx0138zx711ddxq1k1a45lc77ninhzj9zl8ljk"))

(define rust-termtree-0.4.1
  (crate-source "termtree" "0.4.1"
                "0xkal5l2r3r9p9j90x35qy4npbdwxz4gskvbijs6msymaangas9k"))

(define rust-textwrap-0.11.0
  (crate-source "textwrap" "0.11.0"
                "0q5hky03ik3y50s9sz25r438bc4nwhqc6dqwynv4wylc807n29nk"))

(define rust-textwrap-0.16.1
  (crate-source "textwrap" "0.16.1"
                "1fgqn3mg9gdbjxwfxl76fg0qiq53w3mk4hdh1x40jylnz39k9m13"))

(define rust-thiserror-1.0.64
  (crate-source "thiserror" "1.0.64"
                "114s8lmssxl0c2480s671am88vzlasbaikxbvfv8pyqrq6mzh2nm"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.16
  (crate-source "thiserror" "2.0.16"
                "1h30bqyjn5s9ypm668yd9849371rzwk185klwgjg503k2hadcrrl"))

(define rust-thiserror-2.0.18
  (crate-source "thiserror" "2.0.18"
                "1i7vcmw9900bvsmay7mww04ahahab7wmr8s925xc083rpjybb222"))

(define rust-thiserror-impl-1.0.64
  (crate-source "thiserror-impl" "1.0.64"
                "1hvzmjx9iamln854l74qyhs0jl2pg3hhqzpqm9p8gszmf9v4x408"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.16
  (crate-source "thiserror-impl" "2.0.16"
                "0q3r1ipr1rhff6cgrcvc0njffw17rpcqz9hdc7p754cbqkhinpkc"))

(define rust-thiserror-impl-2.0.18
  (crate-source "thiserror-impl" "2.0.18"
                "1mf1vrbbimj1g6dvhdgzjmn6q09yflz2b92zs1j9n3k7cxzyxi7b"))

(define rust-thread-local-1.1.8
  (crate-source "thread_local" "1.1.8"
                "173i5lyjh011gsimk21np9jn8al18rxsrkjli20a7b8ks2xgk7lb"))

(define rust-thread-local-1.1.9
  (crate-source "thread_local" "1.1.9"
                "1191jvl8d63agnq06pcnarivf63qzgpws5xa33hgc92gjjj4c0pn"))

(define rust-time-0.3.47
  (crate-source "time" "0.3.47"
                "0b7g9ly2iabrlgizliz6v5x23yq5d6bpp0mqz6407z1s526d8fvl"))

(define rust-time-core-0.1.8
  (crate-source "time-core" "0.1.8"
                "1jidl426mw48i7hjj4hs9vxgd9lwqq4vyalm4q8d7y4iwz7y353n"))

(define rust-time-macros-0.2.27
  (crate-source "time-macros" "0.2.27"
                "058ja265waq275wxvnfwavbz9r1hd4dgwpfn7a1a9a70l32y8w1f"))

(define rust-tinystr-0.7.6
  (crate-source "tinystr" "0.7.6"
                "0bxqaw7z8r2kzngxlzlgvld1r6jbnwyylyvyjbv1q71rvgaga5wi"))

(define rust-tinystr-0.8.2
  (crate-source "tinystr" "0.8.2"
                "0sa8z88axdsf088hgw5p4xcyi6g3w3sgbb6qdp81bph9bk2fkls2"))

(define rust-tinyvec-1.6.0
  (crate-source "tinyvec" "1.6.0"
                "0l6bl2h62a5m44jdnpn7lmj14rd44via8180i7121fvm73mmrk47"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-tokio-1.49.0
  (crate-source "tokio" "1.49.0"
                "11ix3pl03s0bp71q3wddrbf8xr0cpn47d7fzr6m42r3kswy918kj"))

(define rust-tokio-macros-2.6.0
  (crate-source "tokio-macros" "2.6.0"
                "19czvgliginbzyhhfbmj77wazqn2y8g27y2nirfajdlm41bphh5g"))

(define rust-tokio-rustls-0.26.0
  (crate-source "tokio-rustls" "0.26.0"
                "1m00czrmk8x7pdjnz10a3da3i1d0sdf9j9vfp5dnk5ss1q6w8yqc"))

(define rust-toml-0.8.23
  (crate-source "toml" "0.8.23"
                "0qnkrq4lm2sdhp3l6cb6f26i8zbnhqb7mhbmksd550wxdfcyn6yw"))

(define rust-toml-0.9.12+spec-1.1.0
  (crate-source "toml" "0.9.12+spec-1.1.0"
                "0qwqbrymqn88mg2yqyq3rj52z6p20448z0jxdbpjsbpwg5g894ng"))

(define rust-toml-1.0.3+spec-1.1.0
  (crate-source "toml" "1.0.3+spec-1.1.0"
                "033cc36cl3w7mfq9xzgxnslz573j06idzb94vd3q70dd36plwqf7"))

(define rust-toml-datetime-0.6.11
  (crate-source "toml_datetime" "0.6.11"
                "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))

(define rust-toml-datetime-0.7.2
  (crate-source "toml_datetime" "0.7.2"
                "1hgff8gdk9yx7dljkqfijmj0sc5ln4xhpj045divdhi7xifhiw9j"))

(define rust-toml-datetime-0.7.5+spec-1.1.0
  (crate-source "toml_datetime" "0.7.5+spec-1.1.0"
                "0iqkgvgsxmszpai53dbip7sf2igic39s4dby29dbqf1h9bnwzqcj"))

(define rust-toml-datetime-1.0.0+spec-1.1.0
  (crate-source "toml_datetime" "1.0.0+spec-1.1.0"
                "0gpiaddhignli6whj52ysjxwmmy82r8qxihckzss8y4md5f5bhij"))

(define rust-toml-edit-0.22.27
  (crate-source "toml_edit" "0.22.27"
                "16l15xm40404asih8vyjvnka9g0xs9i4hfb6ry3ph9g419k8rzj1"))

(define rust-toml-edit-0.23.6
  (crate-source "toml_edit" "0.23.6"
                "0jqq4wz6is0497a42m0wh4j3x4vgp70wrlndd57zzzc61rygxvzk"))

(define rust-toml-edit-0.25.3+spec-1.1.0
  (crate-source "toml_edit" "0.25.3+spec-1.1.0"
                "1qaw4p1rkkvhml66k2z62lavfwsiljjn761d2javqn1pwq9pk850"))

(define rust-toml-parser-1.0.3
  (crate-source "toml_parser" "1.0.3"
                "09x6i0b57lwc7yn6w1kbd2ypm4vpcrgd2vdax7h745g77g1r7y2c"))

(define rust-toml-parser-1.0.9+spec-1.1.0
  (crate-source "toml_parser" "1.0.9+spec-1.1.0"
                "1i54qpvvcppy8ybdn9gssas81vfzq0kmgkcnxzhyf8w9w0al8bbh"))

(define rust-toml-write-0.1.2
  (crate-source "toml_write" "0.1.2"
                "008qlhqlqvljp1gpp9rn5cqs74gwvdgbvs92wnpq8y3jlz4zi6ax"))

(define rust-toml-writer-1.0.6+spec-1.1.0
  (crate-source "toml_writer" "1.0.6+spec-1.1.0"
                "01r6x42d1p8p5kzfsi1fm4dakm3w53vi69f2ivyqpvi1xm5g25mb"))

(define rust-tower-0.5.3
  (crate-source "tower" "0.5.3"
                "1m5i3a2z1sgs8nnz1hgfq2nr4clpdmizlp1d9qsg358ma5iyzrgb"))

(define rust-tower-http-0.6.8
  (crate-source "tower-http" "0.6.8"
                "1y514jwzbyrmrkbaajpwmss4rg0mak82k16d6588w9ncaffmbrnl"))

(define rust-tower-layer-0.3.3
  (crate-source "tower-layer" "0.3.3"
                "03kq92fdzxin51w8iqix06dcfgydyvx7yr6izjq0p626v9n2l70j"))

(define rust-tower-service-0.3.3
  (crate-source "tower-service" "0.3.3"
                "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd"))

(define rust-tracing-0.1.40
  (crate-source "tracing" "0.1.40"
                "1vv48dac9zgj9650pg2b4d0j3w6f3x9gbggf43scq5hrlysklln3"))

(define rust-tracing-0.1.44
  (crate-source "tracing" "0.1.44"
                "006ilqkg1lmfdh3xhg3z762izfwmxcvz0w7m4qx2qajbz9i1drv3"))

(define rust-tracing-attributes-0.1.30
  (crate-source "tracing-attributes" "0.1.30"
                "00v9bhfgfg3v101nmmy7s3vdwadb7ngc8c1iw6wai9vj9sv3lf41"))

(define rust-tracing-attributes-0.1.31
  (crate-source "tracing-attributes" "0.1.31"
                "1np8d77shfvz0n7camx2bsf1qw0zg331lra0hxb4cdwnxjjwz43l"))

(define rust-tracing-core-0.1.34
  (crate-source "tracing-core" "0.1.34"
                "0y3nc4mpnr79rzkrcylv5f5bnjjp19lsxwis9l4kzs97ya0jbldr"))

(define rust-tracing-core-0.1.36
  (crate-source "tracing-core" "0.1.36"
                "16mpbz6p8vd6j7sf925k9k8wzvm9vdfsjbynbmaxxyq6v7wwm5yv"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.18
  (crate-source "tracing-subscriber" "0.3.18"
                "12vs1bwk4kig1l2qqjbbn2nm5amwiqmkcmnznylzmnfvjy6083xd"))

(define rust-tracing-subscriber-0.3.20
  (crate-source "tracing-subscriber" "0.3.20"
                "1m9447bxq7236avgl6n5yb2aqwplrghm61dgipw03mh7ad7s2m10"))

(define rust-tracing-subscriber-0.3.22
  (crate-source "tracing-subscriber" "0.3.22"
                "07hz575a0p1c2i4xw3gs3hkrykhndnkbfhyqdwjhvayx4ww18c1g"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-typed-arena-1.7.0
  (crate-source "typed-arena" "1.7.0"
                "0va4q7439qzlxh9acd9nba7m7sljdh7xz1gp8l0i597b0y025cm9"))

(define rust-typed-path-0.12.3
  (crate-source "typed-path" "0.12.3"
                "03k051dafrnyg3lbm4c85zg0mpfhbn6l9aq4ryq8yyy8h2dzha4f"))

(define rust-typenum-1.17.0
  (crate-source "typenum" "1.17.0"
                "09dqxv69m9lj9zvv6xw5vxaqx15ps0vxyy5myg33i0kbqvq0pzs2"))

(define rust-typenum-1.18.0
  (crate-source "typenum" "1.18.0"
                "0gwgz8n91pv40gabrr1lzji0b0hsmg0817njpy397bq7rvizzk0x"))

(define rust-ucd-trie-0.1.6
  (crate-source "ucd-trie" "0.1.6"
                "1ff4yfksirqs37ybin9aw71aa5gva00hw7jdxbw8w668zy964r7d"))

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

(define rust-unicode-ident-1.0.12
  (crate-source "unicode-ident" "1.0.12"
                "0jzf1znfpb2gx8nr8mvmyqs1crnv79l57nxnbiszc7xf7ynbjm1k"))

(define rust-unicode-ident-1.0.19
  (crate-source "unicode-ident" "1.0.19"
                "17bx1j1zf6b9j3kpyf74mraary7ava3984km0n8kh499h5a58fpn"))

(define rust-unicode-ident-1.0.24
  (crate-source "unicode-ident" "1.0.24"
                "0xfs8y1g7syl2iykji8zk5hgfi5jw819f5zsrbaxmlzwsly33r76"))

(define rust-unicode-linebreak-0.1.5
  (crate-source "unicode-linebreak" "0.1.5"
                "07spj2hh3daajg335m4wdav6nfkl0f6c0q72lc37blr97hych29v"))

(define rust-unicode-segmentation-1.11.0
  (crate-source "unicode-segmentation" "1.11.0"
                "00kjpwp1g8fqm45drmwivlacn3y9jx73bvs09n6s3x73nqi7vj6l"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-width-0.1.12
  (crate-source "unicode-width" "0.1.12"
                "1mk6mybsmi5py8hf8zy9vbgs4rw4gkdqdq3gzywd9kwf2prybxb8"))

(define rust-unicode-width-0.2.0
  (crate-source "unicode-width" "0.2.0"
                "1zd0r5vs52ifxn25rs06gxrgz8cmh4xpra922k0xlmrchib1kj0z"))

(define rust-unicode-width-0.2.2
  (crate-source "unicode-width" "0.2.2"
                "0m7jjzlcccw716dy9423xxh0clys8pfpllc5smvfxrzdf66h9b5l"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-unsafe-libyaml-0.2.11
  (crate-source "unsafe-libyaml" "0.2.11"
                "0qdq69ffl3v5pzx9kzxbghzn0fzn266i1xn70y88maybz9csqfk7"))

(define rust-unsafe-libyaml-norway-0.2.15
  (crate-source "unsafe-libyaml-norway" "0.2.15"
                "0111lbq845fwqv8cn89m02v7bjd2lq2jvd814dziqlijpxcvv6mk"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-url-2.5.8
  (crate-source "url" "2.5.8"
                "1v8f7nx3hpr1qh76if0a04sj08k86amsq4h8cvpw6wvk76jahrzz"))

(define rust-utf16-iter-1.0.5
  (crate-source "utf16_iter" "1.0.5"
                "0ik2krdr73hfgsdzw0218fn35fa09dg2hvbi1xp3bmdfrp9js8y8"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8parse-0.2.1
  (crate-source "utf8parse" "0.2.1"
                "02ip1a0az0qmc2786vxk2nqwsgcwf17d3a38fkf0q7hrmwh9c6vi"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-1.21.0
  (crate-source "uuid" "1.21.0"
                "1nsxfd17gfkvl1jmwcy5lnq6z32b8kf19is04byl6b95an2k6wmn"))

(define rust-valuable-0.1.0
  (crate-source "valuable" "0.1.0"
                "0v9gp3nkjbl30z0fd56d8mx7w1csk86wwjhfjhr400wh9mfpw2w3"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"))

(define rust-vec-map-0.8.2
  (crate-source "vec_map" "0.8.2"
                "1481w9g1dw9rxp3l6snkdqihzyrd2f8vispzqmwjwsdyhw8xzggi"))

(define rust-version-check-0.9.4
  (crate-source "version_check" "0.9.4"
                "0gs8grwdlgh0xq660d7wr80x14vxbizmd8dbp29p2pdncx8lp1s9"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-compare-0.2.0
  (crate-source "version-compare" "0.2.0"
                "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5"))

(define rust-wait-timeout-0.2.0
  (crate-source "wait-timeout" "0.2.0"
                "1xpkk0j5l9pfmjfh1pi0i89invlavfrd9av5xp0zhxgb29dhy84z"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasi-0.14.2+wasi-0.2.4
  (crate-source "wasi" "0.14.2+wasi-0.2.4"
                "1cwcqjr3dgdq8j325awgk8a715h0hg0f7jqzsb077n4qm6jzk0wn"))

(define rust-wasi-0.14.7+wasi-0.2.4
  (crate-source "wasi" "0.14.7+wasi-0.2.4"
                "133fq3mq7h65mzrsphcm7bbbx1gsz7srrbwh01624zin43g7hd48"))

(define rust-wasip2-1.0.1+wasi-0.2.4
  (crate-source "wasip2" "1.0.1+wasi-0.2.4"
                "1rsqmpspwy0zja82xx7kbkbg9fv34a4a2if3sbd76dy64a244qh5"))

(define rust-wasip2-1.0.2+wasi-0.2.9
  (crate-source "wasip2" "1.0.2+wasi-0.2.9"
                "1xdw7v08jpfjdg94sp4lbdgzwa587m5ifpz6fpdnkh02kwizj5wm"))

(define rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
  (crate-source "wasip3" "0.4.0+wasi-0.3.0-rc-2026-01-06"
                "19dc8p0y2mfrvgk3qw3c3240nfbylv22mvyxz84dqpgai2zzha2l"))

(define rust-wasm-bindgen-0.2.104
  (crate-source "wasm-bindgen" "0.2.104"
                "0b8f4l6pqm0bz0lj5xgwmchb6977n71vmh7srd0axwg93b011nn1"))

(define rust-wasm-bindgen-0.2.109
  (crate-source "wasm-bindgen" "0.2.109"
                "08n6m45dpnlrcs8crg025xdghqpbfnnwkgyq2w73rb1mxyxcgycz"))

(define rust-wasm-bindgen-0.2.114
  (crate-source "wasm-bindgen" "0.2.114"
                "13nkhw552hpllrrmkd2x9y4bmcxr82kdpky2n667kqzcq6jzjck5"))

(define rust-wasm-bindgen-0.2.92
  (crate-source "wasm-bindgen" "0.2.92"
                "1a4mcw13nsk3fr8fxjzf9kk1wj88xkfsmnm0pjraw01ryqfm7qjb"))

(define rust-wasm-bindgen-backend-0.2.104
  (crate-source "wasm-bindgen-backend" "0.2.104"
                "069vnhhn2j4w2gwd8rch6g8d3iwkrgi45fas6i3qm7glcrd9l737"))

(define rust-wasm-bindgen-backend-0.2.92
  (crate-source "wasm-bindgen-backend" "0.2.92"
                "1nj7wxbi49f0rw9d44rjzms26xlw6r76b2mrggx8jfbdjrxphkb1"))

(define rust-wasm-bindgen-futures-0.4.42
  (crate-source "wasm-bindgen-futures" "0.4.42"
                "1h322zjvpjllcpj7dahfxjsv6inkr6y0baw7nkdwivr1c4v19g3n"))

(define rust-wasm-bindgen-macro-0.2.104
  (crate-source "wasm-bindgen-macro" "0.2.104"
                "06d1m5bg272h6jabq0snm7c50fifjz6r20f5hqlmz7y5wivh99kw"))

(define rust-wasm-bindgen-macro-0.2.109
  (crate-source "wasm-bindgen-macro" "0.2.109"
                "1zgiqyh2h89ylhsz5mkxwcwhmfzi0cjdf5n1jfy0nyl8mn25wi9r"))

(define rust-wasm-bindgen-macro-0.2.114
  (crate-source "wasm-bindgen-macro" "0.2.114"
                "1rhq9kkl7n0zjrag9p25xsi4aabpgfkyf02zn4xv6pqhrw7xb8hq"))

(define rust-wasm-bindgen-macro-0.2.92
  (crate-source "wasm-bindgen-macro" "0.2.92"
                "09npa1srjjabd6nfph5yc03jb26sycjlxhy0c2a1pdrpx4yq5y51"))

(define rust-wasm-bindgen-macro-support-0.2.104
  (crate-source "wasm-bindgen-macro-support" "0.2.104"
                "1mr18kx7ima1pmsqlkk982q4a0vf3r8s1x6901jb59sd1prd41wz"))

(define rust-wasm-bindgen-macro-support-0.2.109
  (crate-source "wasm-bindgen-macro-support" "0.2.109"
                "1mmnaqnnznz1by054gnfk12c7g3vgcb691zc7z8izddhc0gpdx6z"))

(define rust-wasm-bindgen-macro-support-0.2.114
  (crate-source "wasm-bindgen-macro-support" "0.2.114"
                "1qriqqjpn922kv5c7f7627fj823k5aifv06j2gvwsiy5map4rkh3"))

(define rust-wasm-bindgen-macro-support-0.2.92
  (crate-source "wasm-bindgen-macro-support" "0.2.92"
                "1dqv2xs8zcyw4kjgzj84bknp2h76phmsb3n7j6hn396h4ssifkz9"))

(define rust-wasm-bindgen-shared-0.2.104
  (crate-source "wasm-bindgen-shared" "0.2.104"
                "1la1xj9v3gmawnlyi7lc3mb3xi447r6frb98hi2fb9m1nb47vmms"))

(define rust-wasm-bindgen-shared-0.2.109
  (crate-source "wasm-bindgen-shared" "0.2.109"
                "1pdn33r40xp22ay73n0pjmzsjsypg52llk277alj306racf1fsmw"))

(define rust-wasm-bindgen-shared-0.2.114
  (crate-source "wasm-bindgen-shared" "0.2.114"
                "05lc6w64jxlk4wk8rjci4z61lhx2ams90la27a41gvi3qaw2d8vm"))

(define rust-wasm-bindgen-shared-0.2.92
  (crate-source "wasm-bindgen-shared" "0.2.92"
                "15kyavsrna2cvy30kg03va257fraf9x00ny554vxngvpyaa0q6dg"))

(define rust-wasm-encoder-0.244.0
  (crate-source "wasm-encoder" "0.244.0"
                "06c35kv4h42vk3k51xjz1x6hn3mqwfswycmr6ziky033zvr6a04r"))

(define rust-wasm-metadata-0.244.0
  (crate-source "wasm-metadata" "0.244.0"
                "02f9dhlnryd2l7zf03whlxai5sv26x4spfibjdvc3g9gd8z3a3mv"))

(define rust-wasmparser-0.244.0
  (crate-source "wasmparser" "0.244.0"
                "1zi821hrlsxfhn39nqpmgzc0wk7ax3dv6vrs5cw6kb0v5v3hgf27"))

(define rust-web-sys-0.3.69
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "web-sys" "0.3.69"
                "1vqkxk935xa8zcnsi4bd88sb267ly2i24xl1yiq26d1n32hskbvp"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-webpki-root-certs-1.0.6
  (crate-source "webpki-root-certs" "1.0.6"
                "1jm844z3caldlsb4ycb2h7q6vw4awfdgmddmx2sgyxi6mjj1hkw0"))

(define rust-which-4.4.2
  (crate-source "which" "4.4.2"
                "1ixzmx3svsv5hbdvd8vdhd3qwvf6ns8jdpif1wmwsy10k90j9fl7"))

(define rust-widestring-1.1.0
  (crate-source "widestring" "1.1.0"
                "048kxd6iykzi5la9nikpc5hvpp77hmjf1sw43sl3z2dcdrmx66bj"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-winapi-util-0.1.8
  (crate-source "winapi-util" "0.1.8"
                "0svcgddd2rw06mj4r76gj655qsa1ikgz3d3gzax96fz7w62c6k2d"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-0.57.0
  (crate-source "windows" "0.57.0"
                "0hqid10bqvxa3pbpgvrh2cilf950lxsd9zqfv3rldc73v2s2qd0j"))

(define rust-windows-aarch64-gnullvm-0.42.2
  (crate-source "windows_aarch64_gnullvm" "0.42.2"
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"))

(define rust-windows-aarch64-msvc-0.42.2
  (crate-source "windows_aarch64_msvc" "0.42.2"
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-aarch64-msvc-0.53.0
  (crate-source "windows_aarch64_msvc" "0.53.0"
                "0v766yqw51pzxxwp203yqy39ijgjamp54hhdbsyqq6x1c8gilrf7"))

(define rust-windows-core-0.52.0
  (crate-source "windows-core" "0.52.0"
                "1nc3qv7sy24x0nlnb32f7alzpd6f72l4p24vl65vydbyil669ark"))

(define rust-windows-core-0.57.0
  (crate-source "windows-core" "0.57.0"
                "0bc3jxw2jw76xkk3ddvnp5b2m76qmbzv1qncgvb6qrlhl8wj9vfj"))

(define rust-windows-core-0.62.1
  (crate-source "windows-core" "0.62.1"
                "1aa94x61q0x39xnlzxjmahwck9i5p51xgzrz7m6hi1dj2rafwi38"))

(define rust-windows-core-0.62.2
  (crate-source "windows-core" "0.62.2"
                "1swxpv1a8qvn3bkxv8cn663238h2jccq35ff3nsj61jdsca3ms5q"))

(define rust-windows-i686-gnu-0.42.2
  (crate-source "windows_i686_gnu" "0.42.2"
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnu-0.53.0
  (crate-source "windows_i686_gnu" "0.53.0"
                "1hvjc8nv95sx5vdd79fivn8bpm7i517dqyf4yvsqgwrmkmjngp61"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-gnullvm-0.53.0
  (crate-source "windows_i686_gnullvm" "0.53.0"
                "04df1in2k91qyf1wzizvh560bvyzq20yf68k8xa66vdzxnywrrlw"))

(define rust-windows-i686-msvc-0.42.2
  (crate-source "windows_i686_msvc" "0.42.2"
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-i686-msvc-0.53.0
  (crate-source "windows_i686_msvc" "0.53.0"
                "0pcvb25fkvqnp91z25qr5x61wyya12lx8p7nsa137cbb82ayw7sq"))

(define rust-windows-implement-0.57.0
  (crate-source "windows-implement" "0.57.0"
                "1mqs7qypclnmx5r8yq5jy3g2d8i27vzag9yzzzxzpdnmb70ds1wi"))

(define rust-windows-implement-0.60.1
  (crate-source "windows-implement" "0.60.1"
                "1q2lfwdqrkfzsrlshvvyr2cj7ckq4rqxj0ispzlnvyvl5bj0gczd"))

(define rust-windows-implement-0.60.2
  (crate-source "windows-implement" "0.60.2"
                "1psxhmklzcf3wjs4b8qb42qb6znvc142cb5pa74rsyxm1822wgh5"))

(define rust-windows-interface-0.57.0
  (crate-source "windows-interface" "0.57.0"
                "19zwlzr0q1z9s692681yb5w2lhvwcyx4v95s25hfdkd3isry9gi9"))

(define rust-windows-interface-0.59.2
  (crate-source "windows-interface" "0.59.2"
                "19a6if8dfnazjgjw4hm0kayk9vrjclyj3iqivcaaqr39pkfx3ay0"))

(define rust-windows-interface-0.59.3
  (crate-source "windows-interface" "0.59.3"
                "0n73cwrn4247d0axrk7gjp08p34x1723483jxjxjdfkh4m56qc9z"))

(define rust-windows-link-0.2.0
  (crate-source "windows-link" "0.2.0"
                "0r9w2z96d5phmm185aq92z54jp9h2nqisa4wgc71idxbc436rr25"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-result-0.1.2
  (crate-source "windows-result" "0.1.2"
                "1y274q1v0vy21lhkgslpxpq1m08hvr1mcs2l88h1b1gcx0136f2y"))

(define rust-windows-result-0.4.0
  (crate-source "windows-result" "0.4.0"
                "0zqn8kmmf7y9yw9g7q6pbcg9dbry9m03fqi0b92q767q0v1xr13h"))

(define rust-windows-result-0.4.1
  (crate-source "windows-result" "0.4.1"
                "1d9yhmrmmfqh56zlj751s5wfm9a2aa7az9rd7nn5027nxa4zm0bp"))

(define rust-windows-strings-0.5.0
  (crate-source "windows-strings" "0.5.0"
                "1nld65azvms87rdm2bdm8gskwdmsswh4pxbc8babxc2klmawc63j"))

(define rust-windows-strings-0.5.1
  (crate-source "windows-strings" "0.5.1"
                "14bhng9jqv4fyl7lqjz3az7vzh8pw0w4am49fsqgcz67d67x0dvq"))

(define rust-windows-sys-0.45.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.45.0"
                "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m"))

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

(define rust-windows-sys-0.60.2
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-sys-0.61.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.61.0"
                "1ajpwsmzfcsa1r7i0dxzvfn24dp3525rcd7aq95ydvdj8171h0g2"))

(define rust-windows-sys-0.61.1
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.61.1"
                "03vg2rxm0lyiyq64b5sm95lkg2x95sjdb0zb0y4q8g2avm0rw43g"))

(define rust-windows-sys-0.61.2
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define rust-windows-targets-0.42.2
  (crate-source "windows-targets" "0.42.2"
                "0wfhnib2fisxlx8c507dbmh97kgij4r6kcxdi0f9nk6l1k080lcf"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.2
  (crate-source "windows-targets" "0.53.2"
                "1vwanhx2br7dh8mmrszdbcf01bccjr01mcyxcscxl4ffr7y6jvy6"))

(define rust-windows-x86-64-gnu-0.42.2
  (crate-source "windows_x86_64_gnu" "0.42.2"
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"))

(define rust-windows-x86-64-gnullvm-0.42.2
  (crate-source "windows_x86_64_gnullvm" "0.42.2"
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"))

(define rust-windows-x86-64-msvc-0.42.2
  (crate-source "windows_x86_64_msvc" "0.42.2"
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"))

(define rust-winnow-0.7.13
  (crate-source "winnow" "0.7.13"
                "1krrjc1wj2vx0r57m9nwnlc1zrhga3fq41d8w9hysvvqb5mj7811"))

(define rust-winnow-0.7.14
  (crate-source "winnow" "0.7.14"
                "0a88ahjqhyn2ln1yplq2xsigm09kxqkdkkk2c2mfxkbzszln8lss"))

(define rust-winnow-0.7.15
  (crate-source "winnow" "0.7.15"
                "0i9rkl2rqpbnnxlgs20gmkj3nd0b2k8q55mjmpc2ybb84xwxjyfz"))

(define rust-wit-bindgen-0.46.0
  (crate-source "wit-bindgen" "0.46.0"
                "0ngysw50gp2wrrfxbwgp6dhw1g6sckknsn3wm7l00vaf7n48aypi"))

(define rust-wit-bindgen-0.51.0
  (crate-source "wit-bindgen" "0.51.0"
                "19fazgch8sq5cvjv3ynhhfh5d5x08jq2pkw8jfb05vbcyqcr496p"))

(define rust-wit-bindgen-core-0.51.0
  (crate-source "wit-bindgen-core" "0.51.0"
                "1p2jszqsqbx8k7y8nwvxg65wqzxjm048ba5phaq8r9iy9ildwqga"))

(define rust-wit-bindgen-rt-0.39.0
  (crate-source "wit-bindgen-rt" "0.39.0"
                "1hd65pa5hp0nl664m94bg554h4zlhrzmkjsf6lsgsb7yc4734hkg"))

(define rust-wit-bindgen-rust-0.51.0
  (crate-source "wit-bindgen-rust" "0.51.0"
                "08bzn5fsvkb9x9wyvyx98qglknj2075xk1n7c5jxv15jykh6didp"))

(define rust-wit-bindgen-rust-macro-0.51.0
  (crate-source "wit-bindgen-rust-macro" "0.51.0"
                "0ymizapzv2id89igxsz2n587y2hlfypf6n8kyp68x976fzyrn3qc"))

(define rust-wit-component-0.244.0
  (crate-source "wit-component" "0.244.0"
                "1clwxgsgdns3zj2fqnrjcp8y5gazwfa1k0sy5cbk0fsmx4hflrlx"))

(define rust-wit-parser-0.244.0
  (crate-source "wit-parser" "0.244.0"
                "0dm7avvdxryxd5b02l0g5h6933z1cw5z0d4wynvq2cywq55srj7c"))

(define rust-write16-1.0.0
  (crate-source "write16" "1.0.0"
                "0dnryvrrbrnl7vvf5vb1zkmwldhjkf2n5znliviam7bm4900z2fi"))

(define rust-writeable-0.5.5
  (crate-source "writeable" "0.5.5"
                "0lawr6y0bwqfyayf3z8zmqlhpnzhdx0ahs54isacbhyjwa7g778y"))

(define rust-writeable-0.6.2
  (crate-source "writeable" "0.6.2"
                "1fg08y97n6vk7l0rnjggw3xyrii6dcqg54wqaxldrlk98zdy1pcy"))

(define rust-x11rb-0.13.2
  (crate-source "x11rb" "0.13.2"
                "053lvnaw9ycbl791mgwly2hw27q6vqgzrb1y5kz1as52wmdsm4wr"))

(define rust-x11rb-protocol-0.13.2
  (crate-source "x11rb-protocol" "0.13.2"
                "1g81cznbyn522b0fbis0i44wh3adad2vhsz5pzf99waf3sbc4vza"))

(define rust-xattr-1.3.1
  (crate-source "xattr" "1.3.1"
                "0kqxm36w89vc6qcpn6pizlhgjgzq138sx4hdhbv2g6wk4ld4za4d"))

(define rust-xml-rs-0.8.27
  (crate-source "xml-rs" "0.8.27"
                "1irplg223x6w3lvj0yig6czbiwci06495wc9xg3660kh6cvl1n3g"))

(define rust-xz2-0.1.7
  (crate-source "xz2" "0.1.7"
                "1qk7nzpblizvayyq4xzi4b0zacmmbqr6vb9fc0v1avyp17f4931q"))

(define rust-yoke-0.7.4
  (crate-source "yoke" "0.7.4"
                "198c4jkh6i3hxijia7mfa4cpnxg1iqym9bz364697c3rn0a16nvc"))

(define rust-yoke-0.8.1
  (crate-source "yoke" "0.8.1"
                "0m29dm0bf5iakxgma0bj6dbmc3b8qi9b1vaw9sa76kdqmz3fbmkj"))

(define rust-yoke-derive-0.7.4
  (crate-source "yoke-derive" "0.7.4"
                "15cvhkci2mchfffx3fmva84fpmp34dsmnbzibwfnzjqq3ds33k18"))

(define rust-yoke-derive-0.8.1
  (crate-source "yoke-derive" "0.8.1"
                "0pbyja133jnng4mrhimzdq4a0y26421g734ybgz8wsgbfhl0andn"))

(define rust-zerocopy-0.8.27
  (crate-source "zerocopy" "0.8.27"
                "0b1870gf2zzlckca69v2k4mqwmf8yh2li37qldnzvvd3by58g508"))

(define rust-zerocopy-derive-0.8.27
  (crate-source "zerocopy-derive" "0.8.27"
                "0c9qrylm2p55dvaplxsl24ma48add9qk4y0d6kjbkllaqvcvill8"))

(define rust-zerofrom-0.1.4
  (crate-source "zerofrom" "0.1.4"
                "0mdbjd7vmbix2ynxbrbrrli47a5yrpfx05hi99wf1l4pwwf13v4i"))

(define rust-zerofrom-0.1.6
  (crate-source "zerofrom" "0.1.6"
                "19dyky67zkjichsb7ykhv0aqws3q0jfvzww76l66c19y6gh45k2h"))

(define rust-zerofrom-derive-0.1.4
  (crate-source "zerofrom-derive" "0.1.4"
                "19b31rrs2ry1lrq5mpdqjzgg65va51fgvwghxnf6da3ycfiv99qf"))

(define rust-zerofrom-derive-0.1.6
  (crate-source "zerofrom-derive" "0.1.6"
                "00l5niw7c1b0lf1vhvajpjmcnbdp2vn96jg4nmkhq2db0rp5s7np"))

(define rust-zeroize-1.8.1
  (crate-source "zeroize" "1.8.1"
                "1pjdrmjwmszpxfd7r860jx54cyk94qk59x13sc307cvr5256glyf"))

(define rust-zeroize-derive-1.4.2
  (crate-source "zeroize_derive" "1.4.2"
                "0sczjlqjdmrp3wn62g7mw6p438c9j4jgp2f9zamd56991mdycdnf"))

(define rust-zerotrie-0.2.3
  (crate-source "zerotrie" "0.2.3"
                "0lbqznlqazmrwwzslw0ci7p3pqxykrbfhq29npj0gmb2amxc2n9a"))

(define rust-zerovec-0.10.4
  (crate-source "zerovec" "0.10.4"
                "0yghix7n3fjfdppwghknzvx9v8cf826h2qal5nqvy8yzg4yqjaxa"))

(define rust-zerovec-0.11.5
  (crate-source "zerovec" "0.11.5"
                "00m0p47k2g9mkv505hky5xh3r6ps7v8qc0dy4pspg542jj972a3c"))

(define rust-zerovec-derive-0.10.3
  (crate-source "zerovec-derive" "0.10.3"
                "1ik322dys6wnap5d3gcsn09azmssq466xryn5czfm13mn7gsdbvf"))

(define rust-zerovec-derive-0.11.2
  (crate-source "zerovec-derive" "0.11.2"
                "1wsig4h5j7a1scd5hrlnragnazjny9qjc44hancb6p6a76ay7p7a"))

(define rust-zip-8.1.0
  (crate-source "zip" "8.1.0"
                "0wizlw8l73cx8di7j4mqa8ifwynlwqyp7a7lhv8a15vbbjpryjbf"))

(define rust-zlib-rs-0.6.0
  (crate-source "zlib-rs" "0.6.0"
                "0g3ydblvzjn6lfs04q3myxxmvzn1a660whp9nr137g6chbv8m557"))

(define rust-zmij-1.0.12
  (crate-source "zmij" "1.0.12"
                "1y3ryrh5rg1aqv92vndmf0680jyczni5m6fy3cjz32q741madi9g"))

(define rust-zmij-1.0.21
  (crate-source "zmij" "1.0.21"
                "1amb5i6gz7yjb0dnmz5y669674pqmwbj44p4yfxfv2ncgvk8x15q"))

(define rust-zopfli-0.8.3
  (crate-source "zopfli" "0.8.3"
                "0jaj5dyh3mks0805h4ldrsh5pwq4i2jc9dc9zwjm91k3gmwxhp7h"))

(define rust-zstd-0.13.3
  (crate-source "zstd" "0.13.3"
                "12n0h4w9l526li7jl972rxpyf012jw3nwmji2qbjghv9ll8y67p9"))

(define rust-zstd-safe-7.1.0
  (crate-source "zstd-safe" "7.1.0"
                "02pcwzf8j69hvzwkhkh64rayb1wccwjn1a5qr0ca00xwqr2rpn8w"))

(define rust-zstd-safe-7.2.4
  (crate-source "zstd-safe" "7.2.4"
                "179vxmkzhpz6cq6mfzvgwc99bpgllkr6lwxq7ylh5dmby3aw8jcg"))

(define rust-zstd-sys-2.0.10+zstd.1.5.6
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "zstd-sys" "2.0.10+zstd.1.5.6"
                "1ak51pq1ni6q3qgyr58iq1pcz0vyh80f8vn8m27zrfpm9a8s8ly2"))

(define rust-zstd-sys-2.0.16+zstd.1.5.7
  ;; TODO: Check bundled sources.
  (crate-source "zstd-sys" "2.0.16+zstd.1.5.7"
                "0j1pd2iaqpvaxlgqmmijj68wma7xwdv9grrr63j873yw5ay9xqci"))

(define ssss-separator 'end-of-crates)


;;;
;;; Cargo inputs.
;;;

(define-cargo-inputs lookup-cargo-inputs
                     (cargo-dist =>
                                 (list rust-addr2line-0.25.1
                                  rust-adler2-2.0.0
                                  rust-aes-0.8.4
                                  rust-ahash-0.8.12
                                  rust-aho-corasick-1.1.3
                                  rust-android-tzdata-0.1.1
                                  rust-android-system-properties-0.1.5
                                  rust-anstream-0.6.14
                                  rust-anstyle-1.0.13
                                  rust-anstyle-parse-0.2.4
                                  rust-anstyle-query-1.0.3
                                  rust-anstyle-wincon-3.0.3
                                  rust-anyhow-1.0.102
                                  rust-arraydeque-0.5.1
                                  rust-autocfg-1.3.0
                                  rust-aws-lc-rs-1.16.0
                                  rust-aws-lc-sys-0.37.1
                                  rust-axoasset-2.0.1
                                  rust-axocli-0.3.0
                                  rust-axoprocess-0.2.1
                                  rust-axoproject-0.31.0
                                  rust-axotag-0.3.0
                                  rust-axoupdater-0.10.0
                                  rust-backtrace-0.3.76
                                  rust-backtrace-ext-0.2.1
                                  rust-base64-0.22.1
                                  rust-bitflags-1.3.2
                                  rust-bitflags-2.11.0
                                  rust-blake2-0.10.6
                                  rust-block-buffer-0.10.4
                                  rust-bumpalo-3.19.0
                                  rust-bytecount-0.6.8
                                  rust-bytemuck-1.16.0
                                  rust-byteorder-1.5.0
                                  rust-byteorder-lite-0.1.0
                                  rust-bytes-1.11.1
                                  rust-bzip2-0.6.0
                                  rust-camino-1.2.2
                                  rust-cargo-dist-schema-0.31.0
                                  rust-cargo-platform-0.1.8
                                  rust-cargo-platform-0.3.1
                                  rust-cargo-wix-0.3.9
                                  rust-cargo-metadata-0.18.1
                                  rust-cargo-metadata-0.23.1
                                  rust-cc-1.2.56
                                  rust-cesu8-1.1.0
                                  rust-cfg-expr-0.20.6
                                  rust-cfg-if-1.0.4
                                  rust-cfg-aliases-0.2.1
                                  rust-chrono-0.4.38
                                  rust-cipher-0.4.4
                                  rust-clap-4.5.60
                                  rust-clap-cargo-0.18.3
                                  rust-clap-builder-4.5.60
                                  rust-clap-derive-4.5.55
                                  rust-clap-lex-1.0.0
                                  rust-cmake-0.1.57
                                  rust-color-backtrace-0.7.2
                                  rust-colorchoice-1.0.1
                                  rust-combine-4.6.7
                                  rust-comfy-table-7.2.2
                                  rust-console-0.15.10
                                  rust-console-0.16.2
                                  rust-constant-time-eq-0.4.2
                                  rust-core-foundation-0.10.1
                                  rust-core-foundation-sys-0.8.7
                                  rust-cpufeatures-0.2.12
                                  rust-crc32fast-1.5.0
                                  rust-crossterm-0.29.0
                                  rust-crossterm-winapi-0.9.1
                                  rust-crypto-common-0.1.6
                                  rust-current-platform-0.2.0
                                  rust-darling-0.14.4
                                  rust-darling-core-0.14.4
                                  rust-darling-macro-0.14.4
                                  rust-debug-ignore-1.0.5
                                  rust-deranged-0.5.6
                                  rust-derive-builder-0.11.2
                                  rust-derive-builder-core-0.11.2
                                  rust-derive-builder-macro-0.11.2
                                  rust-dialoguer-0.12.0
                                  rust-digest-0.10.7
                                  rust-displaydoc-0.2.5
                                  rust-document-features-0.2.11
                                  rust-dunce-1.0.5
                                  rust-dyn-clone-1.0.17
                                  rust-either-1.12.0
                                  rust-encode-unicode-1.0.0
                                  rust-encoding-rs-0.8.34
                                  rust-encoding-rs-io-0.1.7
                                  rust-env-logger-0.10.2
                                  rust-equivalent-1.0.1
                                  rust-errno-0.3.13
                                  rust-fastrand-2.1.0
                                  rust-filetime-0.2.23
                                  rust-find-msvc-tools-0.1.9
                                  rust-fixedbitset-0.5.7
                                  rust-flate2-1.1.9
                                  rust-fnv-1.0.7
                                  rust-foldhash-0.1.5
                                  rust-form-urlencoded-1.2.2
                                  rust-fs-extra-1.3.0
                                  rust-futures-channel-0.3.30
                                  rust-futures-core-0.3.30
                                  rust-futures-task-0.3.30
                                  rust-futures-util-0.3.30
                                  rust-generic-array-0.14.7
                                  rust-getrandom-0.2.15
                                  rust-getrandom-0.3.3
                                  rust-getrandom-0.4.1
                                  rust-gimli-0.32.3
                                  rust-goblin-0.10.5
                                  rust-guppy-0.17.25
                                  rust-guppy-workspace-hack-0.1.0
                                  rust-hashbrown-0.12.3
                                  rust-hashbrown-0.15.2
                                  rust-hashlink-0.10.0
                                  rust-heck-0.5.0
                                  rust-hermit-abi-0.3.9
                                  rust-hmac-0.12.1
                                  rust-homedir-0.3.4
                                  rust-http-1.1.0
                                  rust-http-body-1.0.0
                                  rust-http-body-util-0.1.3
                                  rust-httparse-1.10.1
                                  rust-humantime-2.1.0
                                  rust-hyper-1.6.0
                                  rust-hyper-rustls-0.27.2
                                  rust-hyper-util-0.1.17
                                  rust-iana-time-zone-0.1.60
                                  rust-iana-time-zone-haiku-0.1.2
                                  rust-icu-collections-1.5.0
                                  rust-icu-locid-1.5.0
                                  rust-icu-locid-transform-1.5.0
                                  rust-icu-locid-transform-data-1.5.0
                                  rust-icu-normalizer-1.5.0
                                  rust-icu-normalizer-data-1.5.0
                                  rust-icu-properties-1.5.1
                                  rust-icu-properties-data-1.5.0
                                  rust-icu-provider-1.5.0
                                  rust-icu-provider-macros-1.5.0
                                  rust-id-arena-2.3.0
                                  rust-ident-case-1.0.1
                                  rust-idna-1.1.0
                                  rust-idna-adapter-1.2.0
                                  rust-image-0.25.4
                                  rust-include-dir-0.7.4
                                  rust-include-dir-macros-0.7.4
                                  rust-indexmap-1.9.3
                                  rust-indexmap-2.11.4
                                  rust-inout-0.1.3
                                  rust-insta-1.45.1
                                  rust-ipnet-2.9.0
                                  rust-iri-string-0.7.10
                                  rust-is-terminal-0.4.12
                                  rust-is-ci-1.2.0
                                  rust-is-terminal-polyfill-1.70.0
                                  rust-itertools-0.11.0
                                  rust-itertools-0.14.0
                                  rust-itoa-1.0.11
                                  rust-jni-0.21.1
                                  rust-jni-sys-0.3.0
                                  rust-jobserver-0.1.31
                                  rust-js-sys-0.3.86
                                  rust-keccak-0.1.6
                                  rust-lazy-static-1.5.0
                                  rust-leb128fmt-0.1.0
                                  rust-lexopt-0.3.0
                                  rust-libbz2-rs-sys-0.2.2
                                  rust-libc-0.2.174
                                  rust-linux-raw-sys-0.4.14
                                  rust-linux-raw-sys-0.9.4
                                  rust-litemap-0.7.3
                                  rust-litrs-0.4.2
                                  rust-lock-api-0.4.12
                                  rust-log-0.3.9
                                  rust-log-0.4.29
                                  rust-lru-slab-0.1.2
                                  rust-lzma-sys-0.1.20
                                  rust-mach-object-0.1.17
                                  rust-memchr-2.7.4
                                  rust-memo-map-0.3.2
                                  rust-miette-5.10.0
                                  rust-miette-7.6.0
                                  rust-miette-derive-5.10.0
                                  rust-miette-derive-7.6.0
                                  rust-mime-0.3.17
                                  rust-minijinja-2.16.0
                                  rust-minimal-lexical-0.2.1
                                  rust-miniz-oxide-0.8.9
                                  rust-mio-1.0.1
                                  rust-mustache-0.9.0
                                  rust-nested-0.1.1
                                  rust-newline-converter-0.3.0
                                  rust-nix-0.29.0
                                  rust-node-semver-2.2.0
                                  rust-nom-7.1.3
                                  rust-nu-ansi-term-0.50.1
                                  rust-num-conv-0.2.0
                                  rust-num-traits-0.2.19
                                  rust-object-0.37.3
                                  rust-once-cell-1.21.3
                                  rust-openssl-probe-0.2.1
                                  rust-oro-common-0.3.34
                                  rust-oro-package-spec-0.3.34
                                  rust-owo-colors-4.0.0
                                  rust-parking-lot-0.12.2
                                  rust-parking-lot-core-0.9.10
                                  rust-parse-changelog-0.6.15
                                  rust-pathdiff-0.2.3
                                  rust-pbkdf2-0.12.2
                                  rust-percent-encoding-2.3.2
                                  rust-peresil-0.3.0
                                  rust-petgraph-0.8.3
                                  rust-pin-project-lite-0.2.14
                                  rust-pin-utils-0.1.0
                                  rust-pkg-config-0.3.30
                                  rust-plain-0.2.3
                                  rust-powerfmt-0.2.0
                                  rust-ppv-lite86-0.2.17
                                  rust-prettyplease-0.2.37
                                  rust-proc-macro2-1.0.106
                                  rust-quick-error-1.2.3
                                  rust-quinn-0.11.9
                                  rust-quinn-proto-0.11.13
                                  rust-quinn-udp-0.5.14
                                  rust-quote-1.0.36
                                  rust-r-efi-5.3.0
                                  rust-rand-0.9.2
                                  rust-rand-chacha-0.9.0
                                  rust-rand-core-0.9.5
                                  rust-redox-syscall-0.4.1
                                  rust-redox-syscall-0.5.1
                                  rust-ref-cast-1.0.25
                                  rust-ref-cast-impl-1.0.25
                                  rust-regex-1.10.4
                                  rust-regex-automata-0.4.6
                                  rust-regex-syntax-0.8.3
                                  rust-reqwest-0.13.2
                                  rust-ring-0.17.14
                                  rust-rustc-cfg-0.5.0
                                  rust-rustc-demangle-0.1.24
                                  rust-rustc-hash-2.0.0
                                  rust-rustix-0.38.34
                                  rust-rustix-1.0.8
                                  rust-rustls-0.23.36
                                  rust-rustls-native-certs-0.8.3
                                  rust-rustls-pki-types-1.14.0
                                  rust-rustls-platform-verifier-0.6.2
                                  rust-rustls-platform-verifier-android-0.1.1
                                  rust-rustls-webpki-0.103.9
                                  rust-rustversion-1.0.22
                                  rust-ryu-1.0.18
                                  rust-same-file-1.0.6
                                  rust-saphyr-parser-0.0.6
                                  rust-schannel-0.1.23
                                  rust-schemars-1.2.1
                                  rust-schemars-derive-1.2.1
                                  rust-scopeguard-1.2.0
                                  rust-scroll-0.13.0
                                  rust-scroll-derive-0.13.1
                                  rust-security-framework-3.7.0
                                  rust-security-framework-sys-2.17.0
                                  rust-self-replace-1.5.0
                                  rust-self-cell-1.0.4
                                  rust-semver-1.0.27
                                  rust-serde-1.0.228
                                  rust-serde-core-1.0.228
                                  rust-serde-derive-1.0.228
                                  rust-serde-derive-internals-0.29.1
                                  rust-serde-json-1.0.149
                                  rust-serde-spanned-1.0.4
                                  rust-serde-yaml-bw-2.5.1
                                  rust-sha1-0.10.6
                                  rust-sha2-0.10.9
                                  rust-sha3-0.10.8
                                  rust-sharded-slab-0.1.7
                                  rust-shell-words-1.1.0
                                  rust-shlex-1.3.0
                                  rust-signal-hook-registry-1.4.2
                                  rust-simd-adler32-0.3.7
                                  rust-similar-2.7.0
                                  rust-slab-0.4.9
                                  rust-smallvec-1.15.1
                                  rust-socket2-0.6.0
                                  rust-spdx-0.13.3
                                  rust-stable-deref-trait-1.2.0
                                  rust-static-assertions-1.1.0
                                  rust-strsim-0.10.0
                                  rust-strsim-0.11.1
                                  rust-subtle-2.5.0
                                  rust-supports-color-3.0.0
                                  rust-supports-hyperlinks-3.0.0
                                  rust-supports-unicode-3.0.0
                                  rust-sxd-document-0.3.2
                                  rust-sxd-xpath-0.4.2
                                  rust-syn-1.0.109
                                  rust-syn-2.0.117
                                  rust-sync-wrapper-1.0.1
                                  rust-synstructure-0.13.1
                                  rust-tar-0.4.44
                                  rust-target-lexicon-0.13.3
                                  rust-target-spec-3.5.7
                                  rust-temp-dir-0.2.0
                                  rust-tempfile-3.10.1
                                  rust-termcolor-1.4.1
                                  rust-terminal-size-0.4.1
                                  rust-textwrap-0.16.1
                                  rust-thiserror-1.0.69
                                  rust-thiserror-2.0.18
                                  rust-thiserror-impl-1.0.69
                                  rust-thiserror-impl-2.0.18
                                  rust-thread-local-1.1.8
                                  rust-time-0.3.47
                                  rust-time-core-0.1.8
                                  rust-time-macros-0.2.27
                                  rust-tinystr-0.7.6
                                  rust-tinyvec-1.6.0
                                  rust-tinyvec-macros-0.1.1
                                  rust-tokio-1.49.0
                                  rust-tokio-macros-2.6.0
                                  rust-tokio-rustls-0.26.0
                                  rust-toml-1.0.3+spec-1.1.0
                                  rust-toml-datetime-1.0.0+spec-1.1.0
                                  rust-toml-edit-0.25.3+spec-1.1.0
                                  rust-toml-parser-1.0.9+spec-1.1.0
                                  rust-toml-writer-1.0.6+spec-1.1.0
                                  rust-tower-0.5.3
                                  rust-tower-http-0.6.8
                                  rust-tower-layer-0.3.3
                                  rust-tower-service-0.3.3
                                  rust-tracing-0.1.44
                                  rust-tracing-attributes-0.1.31
                                  rust-tracing-core-0.1.36
                                  rust-tracing-log-0.2.0
                                  rust-tracing-subscriber-0.3.20
                                  rust-try-lock-0.2.5
                                  rust-typed-arena-1.7.0
                                  rust-typed-path-0.12.3
                                  rust-typenum-1.17.0
                                  rust-unicode-ident-1.0.12
                                  rust-unicode-linebreak-0.1.5
                                  rust-unicode-segmentation-1.11.0
                                  rust-unicode-width-0.1.12
                                  rust-unicode-width-0.2.0
                                  rust-unicode-xid-0.2.6
                                  rust-unsafe-libyaml-norway-0.2.15
                                  rust-untrusted-0.9.0
                                  rust-url-2.5.8
                                  rust-utf16-iter-1.0.5
                                  rust-utf8-iter-1.0.4
                                  rust-utf8parse-0.2.1
                                  rust-uuid-1.21.0
                                  rust-valuable-0.1.0
                                  rust-version-check-0.9.4
                                  rust-walkdir-2.5.0
                                  rust-want-0.3.1
                                  rust-wasi-0.11.0+wasi-snapshot-preview1
                                  rust-wasi-0.14.2+wasi-0.2.4
                                  rust-wasip2-1.0.2+wasi-0.2.9
                                  rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                                  rust-wasm-bindgen-0.2.109
                                  rust-wasm-bindgen-futures-0.4.42
                                  rust-wasm-bindgen-macro-0.2.109
                                  rust-wasm-bindgen-macro-support-0.2.109
                                  rust-wasm-bindgen-shared-0.2.109
                                  rust-wasm-encoder-0.244.0
                                  rust-wasm-metadata-0.244.0
                                  rust-wasmparser-0.244.0
                                  rust-web-sys-0.3.69
                                  rust-web-time-1.1.0
                                  rust-webpki-root-certs-1.0.6
                                  rust-widestring-1.1.0
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-util-0.1.8
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-0.57.0
                                  rust-windows-core-0.52.0
                                  rust-windows-core-0.57.0
                                  rust-windows-implement-0.57.0
                                  rust-windows-interface-0.57.0
                                  rust-windows-link-0.2.0
                                  rust-windows-result-0.1.2
                                  rust-windows-sys-0.45.0
                                  rust-windows-sys-0.52.0
                                  rust-windows-sys-0.59.0
                                  rust-windows-sys-0.60.2
                                  rust-windows-sys-0.61.0
                                  rust-windows-targets-0.42.2
                                  rust-windows-targets-0.52.6
                                  rust-windows-targets-0.53.2
                                  rust-windows-aarch64-gnullvm-0.42.2
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-gnullvm-0.53.0
                                  rust-windows-aarch64-msvc-0.42.2
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-aarch64-msvc-0.53.0
                                  rust-windows-i686-gnu-0.42.2
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnu-0.53.0
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-gnullvm-0.53.0
                                  rust-windows-i686-msvc-0.42.2
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-i686-msvc-0.53.0
                                  rust-windows-x86-64-gnu-0.42.2
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnu-0.53.0
                                  rust-windows-x86-64-gnullvm-0.42.2
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-gnullvm-0.53.0
                                  rust-windows-x86-64-msvc-0.42.2
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-windows-x86-64-msvc-0.53.0
                                  rust-winnow-0.7.14
                                  rust-wit-bindgen-0.51.0
                                  rust-wit-bindgen-core-0.51.0
                                  rust-wit-bindgen-rt-0.39.0
                                  rust-wit-bindgen-rust-0.51.0
                                  rust-wit-bindgen-rust-macro-0.51.0
                                  rust-wit-component-0.244.0
                                  rust-wit-parser-0.244.0
                                  rust-write16-1.0.0
                                  rust-writeable-0.5.5
                                  rust-xattr-1.3.1
                                  rust-xz2-0.1.7
                                  rust-yoke-0.7.4
                                  rust-yoke-derive-0.7.4
                                  rust-zerocopy-0.8.27
                                  rust-zerocopy-derive-0.8.27
                                  rust-zerofrom-0.1.4
                                  rust-zerofrom-derive-0.1.4
                                  rust-zeroize-1.8.1
                                  rust-zeroize-derive-1.4.2
                                  rust-zerovec-0.10.4
                                  rust-zerovec-derive-0.10.3
                                  rust-zip-8.1.0
                                  rust-zlib-rs-0.6.0
                                  rust-zmij-1.0.12
                                  rust-zopfli-0.8.3
                                  rust-zstd-0.13.3
                                  rust-zstd-safe-7.1.0
                                  rust-zstd-sys-2.0.10+zstd.1.5.6))
                     (diesel-cli =>
                                 (list rust-aho-corasick-1.1.4
                                  rust-android-system-properties-0.1.5
                                  rust-anstream-0.6.21
                                  rust-anstyle-1.0.13
                                  rust-anstyle-parse-0.2.7
                                  rust-anstyle-query-1.1.5
                                  rust-anstyle-wincon-3.0.11
                                  rust-anyhow-1.0.102
                                  rust-autocfg-1.5.0
                                  rust-bitflags-2.11.0
                                  rust-bstr-1.12.1
                                  rust-bumpalo-3.20.2
                                  rust-byteorder-1.5.0
                                  rust-cc-1.2.56
                                  rust-cfg-if-1.0.4
                                  rust-chrono-0.4.44
                                  rust-clap-4.5.60
                                  rust-clap-builder-4.5.60
                                  rust-clap-complete-4.5.66
                                  rust-clap-lex-1.0.0
                                  rust-cmake-0.1.57
                                  rust-colorchoice-1.0.4
                                  rust-console-0.15.11
                                  rust-core-foundation-sys-0.8.7
                                  rust-darling-0.21.3
                                  rust-darling-core-0.21.3
                                  rust-darling-macro-0.21.3
                                  rust-deranged-0.5.8
                                  rust-diesel-2.3.6
                                  rust-diesel-derives-2.3.7
                                  rust-diesel-migrations-2.3.1
                                  rust-diesel-table-macro-syntax-0.3.0
                                  rust-diffy-0.4.2
                                  rust-displaydoc-0.2.5
                                  rust-dotenvy-0.15.7
                                  rust-downcast-rs-2.0.2
                                  rust-dsl-auto-type-0.2.0
                                  rust-dunce-1.0.5
                                  rust-either-1.15.0
                                  rust-encode-unicode-1.0.0
                                  rust-equivalent-1.0.2
                                  rust-errno-0.3.14
                                  rust-fastrand-2.3.0
                                  rust-fd-lock-4.0.4
                                  rust-find-msvc-tools-0.1.9
                                  rust-fnv-1.0.7
                                  rust-foldhash-0.1.5
                                  rust-foldhash-0.2.0
                                  rust-form-urlencoded-1.2.2
                                  rust-getrandom-0.4.2
                                  rust-hashbrown-0.15.5
                                  rust-hashbrown-0.16.1
                                  rust-heck-0.5.0
                                  rust-iana-time-zone-0.1.65
                                  rust-iana-time-zone-haiku-0.1.2
                                  rust-icu-collections-2.1.1
                                  rust-icu-locale-core-2.1.1
                                  rust-icu-normalizer-2.1.1
                                  rust-icu-normalizer-data-2.1.1
                                  rust-icu-properties-2.1.2
                                  rust-icu-properties-data-2.1.2
                                  rust-icu-provider-2.1.1
                                  rust-id-arena-2.3.0
                                  rust-ident-case-1.0.1
                                  rust-idna-1.1.0
                                  rust-idna-adapter-1.2.1
                                  rust-indexmap-2.13.0
                                  rust-insta-1.46.3
                                  rust-is-terminal-polyfill-1.70.2
                                  rust-itoa-1.0.17
                                  rust-js-sys-0.3.91
                                  rust-lazy-static-1.5.0
                                  rust-leb128fmt-0.1.0
                                  rust-libc-0.2.183
                                  rust-libsqlite3-sys-0.35.0
                                  rust-link-cplusplus-1.0.12
                                  rust-linux-raw-sys-0.12.1
                                  rust-litemap-0.8.1
                                  rust-log-0.4.29
                                  rust-matchers-0.2.0
                                  rust-memchr-2.8.0
                                  rust-migrations-internals-2.3.0
                                  rust-migrations-macros-2.3.0
                                  rust-mysqlclient-src-0.2.0+9.5.0
                                  rust-mysqlclient-sys-0.5.0
                                  rust-nu-ansi-term-0.50.3
                                  rust-num-conv-0.2.0
                                  rust-num-traits-0.2.19
                                  rust-once-cell-1.21.3
                                  rust-once-cell-polyfill-1.70.2
                                  rust-openssl-src-300.5.5+3.5.5
                                  rust-openssl-sys-0.9.111
                                  rust-percent-encoding-2.3.2
                                  rust-pin-project-lite-0.2.17
                                  rust-pkg-config-0.3.32
                                  rust-potential-utf-0.1.4
                                  rust-powerfmt-0.2.0
                                  rust-pq-src-0.3.11+libpq-18.3
                                  rust-pq-sys-0.7.5
                                  rust-prettyplease-0.2.37
                                  rust-proc-macro2-1.0.106
                                  rust-quote-1.0.45
                                  rust-r-efi-6.0.0
                                  rust-regex-1.12.3
                                  rust-regex-automata-0.4.14
                                  rust-regex-syntax-0.8.10
                                  rust-rsqlite-vfs-0.1.0
                                  rust-rustix-1.1.4
                                  rust-rustversion-1.0.22
                                  rust-semver-1.0.27
                                  rust-serde-1.0.228
                                  rust-serde-core-1.0.228
                                  rust-serde-derive-1.0.228
                                  rust-serde-json-1.0.149
                                  rust-serde-regex-1.1.0
                                  rust-serde-spanned-1.0.4
                                  rust-sharded-slab-0.1.7
                                  rust-shlex-1.3.0
                                  rust-similar-2.7.0
                                  rust-similar-asserts-1.7.0
                                  rust-smallvec-1.15.1
                                  rust-sqlite-wasm-rs-0.5.2
                                  rust-stable-deref-trait-1.2.1
                                  rust-strsim-0.11.1
                                  rust-syn-2.0.117
                                  rust-synstructure-0.13.2
                                  rust-tempfile-3.26.0
                                  rust-thiserror-2.0.18
                                  rust-thiserror-impl-2.0.18
                                  rust-thread-local-1.1.9
                                  rust-time-0.3.47
                                  rust-time-core-0.1.8
                                  rust-time-macros-0.2.27
                                  rust-tinystr-0.8.2
                                  rust-toml-0.9.12+spec-1.1.0
                                  rust-toml-datetime-0.7.5+spec-1.1.0
                                  rust-toml-parser-1.0.9+spec-1.1.0
                                  rust-tracing-0.1.44
                                  rust-tracing-attributes-0.1.31
                                  rust-tracing-core-0.1.36
                                  rust-tracing-log-0.2.0
                                  rust-tracing-subscriber-0.3.22
                                  rust-unicode-ident-1.0.24
                                  rust-unicode-segmentation-1.12.0
                                  rust-unicode-xid-0.2.6
                                  rust-url-2.5.8
                                  rust-utf8-iter-1.0.4
                                  rust-utf8parse-0.2.2
                                  rust-valuable-0.1.1
                                  rust-vcpkg-0.2.15
                                  rust-wasip2-1.0.2+wasi-0.2.9
                                  rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                                  rust-wasm-bindgen-0.2.114
                                  rust-wasm-bindgen-macro-0.2.114
                                  rust-wasm-bindgen-macro-support-0.2.114
                                  rust-wasm-bindgen-shared-0.2.114
                                  rust-wasm-encoder-0.244.0
                                  rust-wasm-metadata-0.244.0
                                  rust-wasmparser-0.244.0
                                  rust-windows-core-0.62.2
                                  rust-windows-implement-0.60.2
                                  rust-windows-interface-0.59.3
                                  rust-windows-link-0.2.1
                                  rust-windows-result-0.4.1
                                  rust-windows-strings-0.5.1
                                  rust-windows-sys-0.59.0
                                  rust-windows-sys-0.61.2
                                  rust-windows-targets-0.52.6
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-winnow-0.7.15
                                  rust-wit-bindgen-0.51.0
                                  rust-wit-bindgen-core-0.51.0
                                  rust-wit-bindgen-rust-0.51.0
                                  rust-wit-bindgen-rust-macro-0.51.0
                                  rust-wit-component-0.244.0
                                  rust-wit-parser-0.244.0
                                  rust-writeable-0.6.2
                                  rust-yoke-0.8.1
                                  rust-yoke-derive-0.8.1
                                  rust-zerofrom-0.1.6
                                  rust-zerofrom-derive-0.1.6
                                  rust-zerotrie-0.2.3
                                  rust-zerovec-0.11.5
                                  rust-zerovec-derive-0.11.2
                                  rust-zmij-1.0.21))
                     (mysqlclient-sys =>
                                      (list rust-aho-corasick-1.1.4
                                            rust-bindgen-0.72.1
                                            rust-bitflags-2.11.0
                                            rust-cc-1.2.56
                                            rust-cexpr-0.6.0
                                            rust-cfg-if-1.0.4
                                            rust-clang-sys-1.8.1
                                            rust-cmake-0.1.57
                                            rust-either-1.15.0
                                            rust-find-msvc-tools-0.1.9
                                            rust-glob-0.3.3
                                            rust-itertools-0.13.0
                                            rust-libc-0.2.183
                                            rust-libloading-0.8.9
                                            rust-link-cplusplus-1.0.12
                                            rust-log-0.4.29
                                            rust-memchr-2.8.0
                                            rust-minimal-lexical-0.2.1
                                            rust-nom-7.1.3
                                            rust-openssl-sys-0.9.111
                                            rust-pkg-config-0.3.32
                                            rust-prettyplease-0.2.37
                                            rust-proc-macro2-1.0.106
                                            rust-quote-1.0.45
                                            rust-regex-1.12.3
                                            rust-regex-automata-0.4.14
                                            rust-regex-syntax-0.8.10
                                            rust-rustc-hash-2.1.1
                                            rust-semver-1.0.27
                                            rust-shlex-1.3.0
                                            rust-syn-2.0.117
                                            rust-unicode-ident-1.0.24
                                            rust-vcpkg-0.2.15
                                            rust-windows-link-0.2.1))
                     (openssl-sys =>
                                  (list rust-aho-corasick-1.1.4
                                   rust-annotate-snippets-0.11.5
                                   rust-anstyle-1.0.13
                                   rust-aws-lc-fips-sys-0.13.12
                                   rust-aws-lc-sys-0.27.1
                                   rust-bindgen-0.69.5
                                   rust-bindgen-0.72.1
                                   rust-bitflags-1.3.2
                                   rust-bitflags-2.11.0
                                   rust-bssl-sys-0.1.0
                                   rust-cc-1.2.56
                                   rust-cexpr-0.6.0
                                   rust-cfg-if-0.1.10
                                   rust-cfg-if-1.0.4
                                   rust-clang-sys-1.8.1
                                   rust-cmake-0.1.57
                                   rust-ctest-0.4.11
                                   rust-dirs-2.0.2
                                   rust-dirs-sys-0.3.7
                                   rust-dunce-1.0.5
                                   rust-either-1.15.0
                                   rust-errno-0.3.14
                                   rust-find-msvc-tools-0.1.9
                                   rust-foreign-types-0.3.2
                                   rust-foreign-types-shared-0.1.1
                                   rust-fs-extra-1.3.0
                                   rust-garando-errors-0.1.0
                                   rust-garando-pos-0.1.0
                                   rust-garando-syntax-0.1.1
                                   rust-getrandom-0.2.17
                                   rust-getrandom-0.3.4
                                   rust-glob-0.3.3
                                   rust-hex-0.4.3
                                   rust-home-0.5.12
                                   rust-indoc-2.0.7
                                   rust-itertools-0.12.1
                                   rust-itertools-0.13.0
                                   rust-itoa-1.0.17
                                   rust-jobserver-0.1.34
                                   rust-lazy-static-1.5.0
                                   rust-lazycell-1.3.0
                                   rust-libc-0.2.183
                                   rust-libloading-0.8.9
                                   rust-libredox-0.1.14
                                   rust-linux-raw-sys-0.4.15
                                   rust-log-0.4.29
                                   rust-memchr-2.8.0
                                   rust-minimal-lexical-0.2.1
                                   rust-nom-7.1.3
                                   rust-once-cell-1.21.3
                                   rust-openssl-src-300.5.5+3.5.5
                                   rust-pkg-config-0.3.32
                                   rust-prettyplease-0.2.37
                                   rust-proc-macro2-1.0.106
                                   rust-quote-1.0.45
                                   rust-r-efi-5.3.0
                                   rust-redox-users-0.4.6
                                   rust-regex-1.12.3
                                   rust-regex-automata-0.4.14
                                   rust-regex-syntax-0.8.10
                                   rust-rustc-hash-1.1.0
                                   rust-rustc-hash-2.1.1
                                   rust-rustc-version-0.4.1
                                   rust-rustix-0.38.44
                                   rust-rustversion-1.0.22
                                   rust-semver-1.0.27
                                   rust-serde-1.0.228
                                   rust-serde-core-1.0.228
                                   rust-serde-derive-1.0.228
                                   rust-serde-json-1.0.149
                                   rust-shlex-1.3.0
                                   rust-syn-2.0.117
                                   rust-term-0.6.1
                                   rust-thiserror-1.0.69
                                   rust-thiserror-impl-1.0.69
                                   rust-unicode-ident-1.0.24
                                   rust-unicode-width-0.2.2
                                   rust-unicode-xid-0.2.6
                                   rust-vcpkg-0.2.15
                                   rust-wasi-0.11.1+wasi-snapshot-preview1
                                   rust-wasip2-1.0.2+wasi-0.2.9
                                   rust-which-4.4.2
                                   rust-winapi-0.3.9
                                   rust-winapi-i686-pc-windows-gnu-0.4.0
                                   rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                   rust-windows-link-0.2.1
                                   rust-windows-sys-0.59.0
                                   rust-windows-sys-0.61.2
                                   rust-windows-targets-0.52.6
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-msvc-0.52.6
                                   rust-wit-bindgen-0.51.0
                                   rust-zmij-1.0.21))
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
                                  rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (whiskers =>
                               (list rust-aho-corasick-1.1.3
                                     rust-android-tzdata-0.1.1
                                     rust-android-system-properties-0.1.5
                                     rust-anstream-0.6.14
                                     rust-anstyle-1.0.8
                                     rust-anstyle-parse-0.2.4
                                     rust-anstyle-query-1.0.3
                                     rust-anstyle-wincon-3.0.3
                                     rust-anyhow-1.0.89
                                     rust-assert-cmd-2.0.16
                                     rust-autocfg-1.3.0
                                     rust-base64-0.22.1
                                     rust-bitflags-2.5.0
                                     rust-block-buffer-0.10.4
                                     rust-bstr-1.9.1
                                     rust-bumpalo-3.16.0
                                     rust-byteorder-1.5.0
                                     rust-catppuccin-2.4.0
                                     rust-cc-1.0.98
                                     rust-cfg-if-1.0.0
                                     rust-chrono-0.4.38
                                     rust-chrono-tz-0.9.0
                                     rust-chrono-tz-build-0.3.0
                                     rust-clap-4.5.20
                                     rust-clap-stdin-0.5.1
                                     rust-clap-builder-4.5.20
                                     rust-clap-derive-4.5.18
                                     rust-clap-lex-0.7.0
                                     rust-colorchoice-1.0.1
                                     rust-core-foundation-sys-0.8.6
                                     rust-cpufeatures-0.2.12
                                     rust-crossbeam-deque-0.8.5
                                     rust-crossbeam-epoch-0.9.18
                                     rust-crossbeam-utils-0.8.20
                                     rust-crypto-common-0.1.6
                                     rust-css-colors-1.0.1
                                     rust-detect-newline-style-0.1.2
                                     rust-deunicode-1.6.0
                                     rust-difflib-0.4.0
                                     rust-digest-0.10.7
                                     rust-doc-comment-0.3.3
                                     rust-either-1.12.0
                                     rust-encoding-rs-0.8.34
                                     rust-encoding-rs-io-0.1.7
                                     rust-equivalent-1.0.1
                                     rust-errno-0.3.9
                                     rust-fastrand-2.1.1
                                     rust-float-cmp-0.9.0
                                     rust-generic-array-0.14.7
                                     rust-getrandom-0.2.15
                                     rust-globset-0.4.14
                                     rust-globwalk-0.9.1
                                     rust-hashbrown-0.15.0
                                     rust-heck-0.5.0
                                     rust-humansize-2.1.3
                                     rust-iana-time-zone-0.1.60
                                     rust-iana-time-zone-haiku-0.1.2
                                     rust-ignore-0.4.22
                                     rust-indexmap-2.6.0
                                     rust-is-terminal-polyfill-1.70.0
                                     rust-itertools-0.13.0
                                     rust-itoa-1.0.11
                                     rust-js-sys-0.3.69
                                     rust-lazy-static-1.4.0
                                     rust-libc-0.2.159
                                     rust-libm-0.2.8
                                     rust-linux-raw-sys-0.4.14
                                     rust-log-0.4.21
                                     rust-lzma-rust-0.1.7
                                     rust-memchr-2.7.2
                                     rust-normalize-line-endings-0.3.0
                                     rust-num-traits-0.2.19
                                     rust-once-cell-1.19.0
                                     rust-parse-zoneinfo-0.3.1
                                     rust-paste-1.0.15
                                     rust-percent-encoding-2.3.1
                                     rust-pest-2.7.10
                                     rust-pest-derive-2.7.10
                                     rust-pest-generator-2.7.10
                                     rust-pest-meta-2.7.10
                                     rust-phf-0.11.2
                                     rust-phf-codegen-0.11.2
                                     rust-phf-generator-0.11.2
                                     rust-phf-shared-0.11.2
                                     rust-ppv-lite86-0.2.17
                                     rust-predicates-3.1.2
                                     rust-predicates-core-1.0.6
                                     rust-predicates-tree-1.0.9
                                     rust-proc-macro2-1.0.85
                                     rust-quote-1.0.36
                                     rust-rand-0.8.5
                                     rust-rand-chacha-0.3.1
                                     rust-rand-core-0.6.4
                                     rust-regex-1.10.4
                                     rust-regex-automata-0.4.6
                                     rust-regex-syntax-0.8.3
                                     rust-rmp-0.8.14
                                     rust-rmp-serde-1.3.0
                                     rust-rustix-0.38.37
                                     rust-ryu-1.0.18
                                     rust-same-file-1.0.6
                                     rust-semver-1.0.23
                                     rust-serde-1.0.210
                                     rust-serde-derive-1.0.210
                                     rust-serde-json-1.0.128
                                     rust-serde-yaml-0.9.34+deprecated
                                     rust-sha2-0.10.8
                                     rust-siphasher-0.3.11
                                     rust-slug-0.1.5
                                     rust-strsim-0.11.1
                                     rust-syn-2.0.66
                                     rust-tempfile-3.13.0
                                     rust-tera-1.20.0
                                     rust-termtree-0.4.1
                                     rust-thiserror-1.0.64
                                     rust-thiserror-impl-1.0.64
                                     rust-typenum-1.17.0
                                     rust-ucd-trie-0.1.6
                                     rust-unic-char-property-0.9.0
                                     rust-unic-char-range-0.9.0
                                     rust-unic-common-0.9.0
                                     rust-unic-segment-0.9.0
                                     rust-unic-ucd-segment-0.9.0
                                     rust-unic-ucd-version-0.9.0
                                     rust-unicode-ident-1.0.12
                                     rust-unsafe-libyaml-0.2.11
                                     rust-utf8parse-0.2.1
                                     rust-version-check-0.9.4
                                     rust-wait-timeout-0.2.0
                                     rust-walkdir-2.5.0
                                     rust-wasi-0.11.0+wasi-snapshot-preview1
                                     rust-wasm-bindgen-0.2.92
                                     rust-wasm-bindgen-backend-0.2.92
                                     rust-wasm-bindgen-macro-0.2.92
                                     rust-wasm-bindgen-macro-support-0.2.92
                                     rust-wasm-bindgen-shared-0.2.92
                                     rust-winapi-util-0.1.8
                                     rust-windows-core-0.52.0
                                     rust-windows-sys-0.52.0
                                     rust-windows-sys-0.59.0
                                     rust-windows-targets-0.52.6
                                     rust-windows-aarch64-gnullvm-0.52.6
                                     rust-windows-aarch64-msvc-0.52.6
                                     rust-windows-i686-gnu-0.52.6
                                     rust-windows-i686-gnullvm-0.52.6
                                     rust-windows-i686-msvc-0.52.6
                                     rust-windows-x86-64-gnu-0.52.6
                                     rust-windows-x86-64-gnullvm-0.52.6
                                     rust-windows-x86-64-msvc-0.52.6)))
