#!/bin/sh

set -e

stack install

BINHWK=$HOME/.local/bin/hwk
LIBHWK=$HOME/.local/lib/hwk

mv -f $BINHWK $LIBHWK

RESOLVER=$(grep -i "resolver:" stack.yaml | sed -e "s/.*: *//")

cat > $BINHWK <<EOF
#!/bin/sh

stack --resolver $RESOLVER exec -- $LIBHWK "\$@"
EOF

chmod u+x $BINHWK
