#!/bin/bash
(
  echo 'digraph G {' &&
  fgrep import *.hs |
  sed 's/.hs:import /->/g' |
  sed 's/(.*)//g' |
  sed 's/qualified //g' |
  fgrep -v '.' |
  fgrep -v 'Sound.MED.Basic.Amiga' |
  fgrep -v 'Sound.MED.Basic.Human' |
  sed 's/$/;/g' |
  sort |
  uniq &&
  echo '};'
) > ../docs/modules.dot &&
dot -Tpng <../docs/modules.dot >../docs/modules.png
