#!/usr/bin/env bash

function grammar {
  grep -- '-- >' $1 | sed 's/^.*-- \> //' | tr -s '::=' '&' | column -t -s'&' | awk 'sub(/[>]/, "> ::=")' > $2
}

grammar ../src/Data/Gerber/Parser.hs gerber.ebnf
grammar ../src/Data/Excellon/Parser.hs excellon.ebnf
