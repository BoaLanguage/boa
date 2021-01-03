(lang dune 2.7)
(name boa)
(version 0.1.0-dev)

(generate_opam_files true)

(source (github BoaLanguage/boa))
(license MIT)
;(authors "Archishman Sravankumar" "James Meyers")

(package
    (name boa)
    (synopsis "A statically typed, mixed-paradigm, Python-inspired language")
    (depends
        (ounit (>= 2.2.3))
        (llvm (>= 10.0.0))
    )           
)
(using menhir 2.1)
