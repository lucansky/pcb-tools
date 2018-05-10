# pcb-tools
PCB tooling written in Haskell. This library provides [Gerber RS-274X](https://www.ucamco.com/files/downloads/file/81/the_gerber_file_format_specification.pdf) and [Excellon](https://web.archive.org/web/20071030075236/http://www.excellon.com/manuals/program.htm) parser, as well as some other useful programs generally used in [PCB Milling hobby industry](https://wiki.base48.cz/PCBMilling), locally in Brno hackerspace [base48.cz](https://base48.cz).

## Programs
### drillmerge (work in-progress)
Rounds diameters of the holes to available drills. Current strategy is regular rounding.
```
drillmerge --drills 0.8,0.9,1.0,1.1,1.2,1.4,2.0,3.0 --tolerance 0.1 drill.drl
```
Suggested features:
- ```--drills 0.8,0.9..1.4,2.0,3.0```
- ability to define whether to round up or down for each drill `--drills 1.0,1.4,2.0+,3.0-` (or whatever better)

### drawgerber
Mock-up parsing gerber file and drawing to file. Uses [diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/).
```
stack exec -- drawgerber --width=400 -o test.pdf
```
Currently has some perfomance issues `O(2^n)` :/

### generategerber
A generator of sample random gerber files for testing performance.
```
stack exec -- generategerber --count 200 generated/sample1.gbr
```

### gcoder (work in-progress)
[pcb2gcode](https://github.com/pcb2gcode/pcb2gcode) alternative (with fixed thermals!), not quite done, will come after drawgerber outputs valid diagrams for all edge cases

## Install
Install [stack](https://haskellstack.org) and run ```stack install```

## Goals
- [x] Try [LBNF](https://bnfc.readthedocs.io/en/latest/lbnf.html) complete Alex/Happy parser with EBNF grammar
- [x] [Attoparsec](https://hackage.haskell.org/package/attoparsec) parser for Gerber format
- [ ] Understand monads
- [x] Use monads
- [x] Gerber interpreter/exporter
- [x] Excellon parser / interpreter
- [ ] drillmerge
- [ ] Outliner & G-Code generator
 

## What's missing
- Aperture macros
- Polygon definition
- Circular interpolation mode

## Demo
![Parsed gerber](https://raw.githubusercontent.com/lucansky/pcb-tools/master/example/scale.png)
