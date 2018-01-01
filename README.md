# pcb-tools
EDA tooling written in Haskell.
Includes:
* [Gerber RS-274X](https://www.ucamco.com/files/downloads/file/81/the_gerber_file_format_specification.pdf) parser
* [Excellon](https://web.archive.org/web/20071030075236/http://www.excellon.com/manuals/program.htm) parser (WIP)

## Goals
- [x] Try [LBNF](https://bnfc.readthedocs.io/en/latest/lbnf.html) complete Alex/Happy parser with EBNF grammar
- [x] [Attoparsec](https://hackage.haskell.org/package/attoparsec) parser for Gerber format
- [ ] Understand monads
- [ ] Gerber interpreter/exporter (40%, State monad prepared)
- [ ] Excellon parser
- [ ] Excellon interpreter
- [ ] G-Code interpreter
- [ ] Documentation
- [ ] Refactor to use pipes

## Design decisions
TBA
