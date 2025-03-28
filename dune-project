(lang dune 3.17)

(generate_opam_files true)

(source
 (github suvorovrain/CotoPeS))

(authors "Rodion Suvorov <suvorov53245324@gmail.com>")

(maintainers "Rodion Suvorov <suvorov53245324@gmail.com>, Vyacheslav Kochergin <vyacheslav.kochergin1@gmail.com>")

(license LICENSE)

(package
 (name cotopes)
 (synopsis "Набор примеров преобразований функций в CPS и хвостовой стили")
 (description "Задачи с экзаменов 2024, 2025 годов, примеры из презентаций и не только")
 (depends ocaml)
 )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
