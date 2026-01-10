(lang dune 3.20)

(name raybook)

(generate_opam_files true)

(source
 (github mdales/raybook))

(authors "Michael Dales <michael@digitalflapjack.com>")

(maintainers "Michael Dales <michael@digitalflapjack.com>")

(license GPL)

(maintenance_intent "(latest)")

(documentation https://url/to/documentation)

(package
 (name raybook)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml (ounit2 :with-test) (tsdl (>= 1.1.0)))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
