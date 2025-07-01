(define-module (afistfullofash packages tree-sitter)
  #:use-module (guix gexp))

(define tree-sitter-grammar (module-ref (resolve-module '(gnu packages tree-sitter)) 'tree-sitter-grammar))

; https://lists.gnu.org/archive/html/guix-patches/2025-03/msg00285.html
(define-public tree-sitter-yaml
  (tree-sitter-grammar
   "yaml" "YAML"
   "0z5fz9hiafzapi0ijhyz8np6rksq6c1pb16xv1vhnlfh75rg6zyv" "0.7.0"
   #:grammar-directories '("schema/core" "schema/json")
   #:get-cleanup-snippet
   (lambda _
     #~(begin
         (use-modules (guix build utils))
         (delete-file-recursively "bindings")))))
