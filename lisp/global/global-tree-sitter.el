;; -*- lexical-binding: t; -*-

(use-package tree-sitter
  :straight t
  :hook (emacs-startup . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-indent
  :straight t)

(use-package tree-sitter-langs
  :straight t)

(use-package treemacs
  :straight t
  :commands (treemacs))


(setq treesit-language-source-alist
      '((ada . "https://github.com/briot/tree-sitter-ada")
	(agda . "https://github.com/AusCyberman/tree-sitter-agda")
	(meson . "https://github.com/Decodetalkers/tree-sitter-meson")
	(qmljs . "https://github.com/yuja/tree-sitter-qmljs")
	(racket . "https://github.com/6cdh/tree-sitter-racket")
	(scheme . "https://github.com/6cdh/tree-sitter-scheme")
	(javascript . "https://github.com/tree-sitter/tree-sitter-javascript")
	(rego . "https://github.com/FallenAngel97/tree-sitter-rego")
	(c . "https://github.com/tree-sitter/tree-sitter-c")
	(embedded-template . "https://github.com/tree-sitter/tree-sitter-embedded-template")
	(clojure . "https://github.com/sogaiu/tree-sitter-clojure")
	(commonlisp . "https://github.com/theHamsta/tree-sitter-commonlisp")
	(c++ . "https://github.com/tree-sitter/tree-sitter-cpp")
	(cuda . "https://github.com/theHamsta/tree-sitter-cuda")
	(d . "https://github.com/CyberShadow/tree-sitter-d")
	(glsl . "https://github.com/theHamsta/tree-sitter-glsl")
	(hlsl . "https://github.com/theHamsta/tree-sitter-hlsl")
	(dockerfile . "https://github.com/camdencheek/tree-sitter-dockerfile")
	(dot . "https://github.com/rydesun/tree-sitter-dot")
	(rust . "https://github.com/tree-sitter/tree-sitter-rust")
	(fsh . "https://github.com/mgramigna/tree-sitter-fsh")
	(fusion . "https://gitlab.com/jirgn/tree-sitter-fusion.git")
	(ledger . "https://github.com/cbarrete/tree-sitter-ledger")
	(lua . "https://github.com/MunifTanjim/tree-sitter-lua")
	(python . "https://github.com/tree-sitter/tree-sitter-python")
	(go . "https://github.com/tree-sitter/tree-sitter-go")
	(go-mod . "https://github.com/camdencheek/tree-sitter-go-mod")
	(go-work . "https://github.com/omertuc/tree-sitter-go-work")
	(graphql . "https://github.com/bkegley/tree-sitter-graphql")
	(ruby . "https://github.com/tree-sitter/tree-sitter-ruby")
	(perl . "https://github.com/ganezdragon/tree-sitter-perl")
	(bash . "https://github.com/tree-sitter/tree-sitter-bash")
	(fish . "https://github.com/ram02z/tree-sitter-fish")
	(php . "https://github.com/tree-sitter/tree-sitter-php")
	(java . "https://github.com/tree-sitter/tree-sitter-java")
	(kotlin . "https://github.com/fwcd/tree-sitter-kotlin")
	(html . "https://github.com/tree-sitter/tree-sitter-html")
	(julia . "https://github.com/tree-sitter/tree-sitter-julia")
	(json . "https://github.com/tree-sitter/tree-sitter-json")
	(jsonnet . "https://github.com/sourcegraph/tree-sitter-jsonnet")
	(css . "https://github.com/tree-sitter/tree-sitter-css")
	(scss . "https://github.com/serenadeai/tree-sitter-scss")
	(erlang . "https://github.com/WhatsApp/tree-sitter-erlang")
	(elixir . "https://github.com/elixir-lang/tree-sitter-elixir")
	(gleam . "https://github.com/J3RN/tree-sitter-gleam")
	(surface . "https://github.com/connorlay/tree-sitter-surface")
	(eex . "https://github.com/connorlay/tree-sitter-eex")
	(heex . "https://github.com/connorlay/tree-sitter-heex")
	(ocaml . "https://github.com/tree-sitter/tree-sitter-ocaml")
	(ocaml . "https://github.com/tree-sitter/tree-sitter-ocaml")
	(ocamllex . "https://github.com/atom-ocaml/tree-sitter-ocamllex")
	(menhir . "https://github.com/Kerl13/tree-sitter-menhir")
	(org . "https://github.com/milisims/tree-sitter-org")
	(swift . "https://github.com/alex-pinkus/tree-sitter-swift")
	(c-sharp . "https://github.com/tree-sitter/tree-sitter-c-sharp")
	(todotxt . "https://github.com/arnarg/tree-sitter-todotxt.git")
	(typescript . "https://github.com/tree-sitter/tree-sitter-typescript")
	(typescript . "https://github.com/tree-sitter/tree-sitter-typescript")
	(scala . "https://github.com/tree-sitter/tree-sitter-scala")
	(supercollider . "https://github.com/madskjeldgaard/tree-sitter-supercollider")
	(slint . "https://github.com/jrmoulton/tree-sitter-slint")
	(smali . "https://github.com/amaanq/tree-sitter-smali")
	(haskell . "https://github.com/tree-sitter/tree-sitter-haskell")
	(hcl . "https://github.com/MichaHoffmann/tree-sitter-hcl")
	(hcl . "https://github.com/MichaHoffmann/tree-sitter-hcl")
	(markdown . "https://github.com/MDeiml/tree-sitter-markdown")
	(markdown . "https://github.com/MDeiml/tree-sitter-markdown")
	(tlaplus . "https://github.com/tlaplus-community/tree-sitter-tlaplus")
	(toml . "https://github.com/ikatyang/tree-sitter-toml")
	(glimmer . "https://github.com/alexlafroscia/tree-sitter-glimmer")
	(pug . "https://github.com/zealot128/tree-sitter-pug")
	(vue . "https://github.com/ikatyang/tree-sitter-vue")
	(jsonc . "https://gitlab.com/WhyNotHugo/tree-sitter-jsonc.git")
	(elm . "https://github.com/elm-tooling/tree-sitter-elm")
	(yaml . "https://github.com/ikatyang/tree-sitter-yaml")
	(yang . "https://github.com/Hubro/tree-sitter-yang")
	(ninja . "https://github.com/alemuller/tree-sitter-ninja")
	(nix . "https://github.com/cstrahan/tree-sitter-nix")
	(dart . "https://github.com/UserNobody14/tree-sitter-dart")
	(rst . "https://github.com/stsewd/tree-sitter-rst")
	(fennel . "https://github.com/travonted/tree-sitter-fennel")
	(teal . "https://github.com/euclidianAce/tree-sitter-teal")
	(ql . "https://github.com/tree-sitter/tree-sitter-ql")
	(verilog . "https://github.com/tree-sitter/tree-sitter-verilog")
	(pascal . "https://github.com/Isopod/tree-sitter-pascal.git")
	(phpdoc . "https://github.com/claytonrcarter/tree-sitter-phpdoc")
	(regex . "https://github.com/tree-sitter/tree-sitter-regex")
	(comment . "https://github.com/stsewd/tree-sitter-comment")
	(jsdoc . "https://github.com/tree-sitter/tree-sitter-jsdoc")
	(query . "https://github.com/nvim-treesitter/tree-sitter-query")
	(sparql . "https://github.com/BonaBeavis/tree-sitter-sparql")
	(sql . "https://github.com/derekstride/tree-sitter-sql")
	(gdscript . "https://github.com/PrestonKnopp/tree-sitter-gdscript")
	(godot-resource . "https://github.com/PrestonKnopp/tree-sitter-godot-resource")
	(turtle . "https://github.com/BonaBeavis/tree-sitter-turtle")
	(devicetree . "https://github.com/joelspadin/tree-sitter-devicetree")
	(svelte . "https://github.com/Himujjal/tree-sitter-svelte")
	(r . "https://github.com/r-lib/tree-sitter-r")
	(beancount . "https://github.com/polarmutex/tree-sitter-beancount")
	(rnoweb . "https://github.com/bamonroe/tree-sitter-rnoweb")
	(latex . "https://github.com/latex-lsp/tree-sitter-latex")
	(bibtex . "https://github.com/latex-lsp/tree-sitter-bibtex")
	(zig . "https://github.com/maxxnino/tree-sitter-zig")
	(fortran . "https://github.com/stadelmanma/tree-sitter-fortran")
	(cmake . "https://github.com/uyha/tree-sitter-cmake")
	(viml . "https://github.com/vigoux/tree-sitter-viml")
	(vimdoc . "https://github.com/neovim/tree-sitter-vimdoc")
	(json5 . "https://github.com/Joakker/tree-sitter-json5")
	(pioasm . "https://github.com/leo60228/tree-sitter-pioasm")
	(hjson . "https://github.com/winston0410/tree-sitter-hjson")
	(hocon . "https://github.com/antosha417/tree-sitter-hocon")
	(llvm . "https://github.com/benwilliamgraham/tree-sitter-llvm")
	(http . "https://github.com/rest-nvim/tree-sitter-http")
	(prisma . "https://github.com/victorhqc/tree-sitter-prisma")
	(make . "https://github.com/alemuller/tree-sitter-make")
	(rasi . "https://github.com/Fymyte/tree-sitter-rasi")
	(foam . "https://github.com/FoamScience/tree-sitter-foam")
	(hack . "https://github.com/slackhq/tree-sitter-hack")
	(norg . "https://github.com/nvim-neorg/tree-sitter-norg")
	(vala . "https://github.com/vala-lang/tree-sitter-vala")
	(lalrpop . "https://github.com/traxys/tree-sitter-lalrpop")
	(solidity . "https://github.com/YongJieYongJie/tree-sitter-solidity")
	(cooklang . "https://github.com/addcninblue/tree-sitter-cooklang")
	(elvish . "https://github.com/ckafi/tree-sitter-elvish")
	(astro . "https://github.com/virchau13/tree-sitter-astro")
	(wgsl . "https://github.com/szebniok/tree-sitter-wgsl")
	(wgsl-bevy . "https://github.com/theHamsta/tree-sitter-wgsl-bevy")
	(m68k . "https://github.com/grahambates/tree-sitter-m68k")
	(proto . "https://github.com/mitchellh/tree-sitter-proto")
	(tiger . "https://github.com/ambroisie/tree-sitter-tiger")
	(t32 . "https://codeberg.org/xasc/tree-sitter-t32")
	(sxhkdrc . "https://github.com/RaafatTurki/tree-sitter-sxhkdrc")
	(gitignore . "https://github.com/shunsambongi/tree-sitter-gitignore")
	(nickel . "https://github.com/nickel-lang/tree-sitter-nickel")
	(gitattributes . "https://github.com/ObserverOfTime/tree-sitter-gitattributes")
	(git-rebase . "https://github.com/the-mikedavis/tree-sitter-git-rebase")
	(gitcommit . "https://github.com/gbprod/tree-sitter-gitcommit")
	(blueprint . "https://gitlab.com/gabmus/tree-sitter-blueprint.git")
	(twig . "https://github.com/gbprod/tree-sitter-twig")
	(diff . "https://github.com/the-mikedavis/tree-sitter-diff")
	(vhs . "https://github.com/charmbracelet/tree-sitter-vhs")
	(awk . "https://github.com/Beaglefoot/tree-sitter-awk")
	(arduino . "https://github.com/ObserverOfTime/tree-sitter-arduino")
	(jq . "https://github.com/flurie/tree-sitter-jq")
	(mermaid . "https://github.com/monaqa/tree-sitter-mermaid")))



(provide 'global/global-tree-sitter)
