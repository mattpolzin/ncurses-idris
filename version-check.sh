#/bin/sh

pkgversion="$(cat ncurses-idris.ipkg | sed -n 's/version = \(.*\)/\1/p')"
pkgsupversion="$(cat ncurses-idris.ipkg | sed -n "s/postinstall = .*VERSION=.\([^ ]*\). .*$/\1/p")"
supversion="$(cat support/Makefile | sed -n 's/TARGET_VERSION .= \(.*\)/\1/p')"

if [ "$pkgversion" == "$pkgsupversion" ] && [ "$pkgversion" == "$supversion" ]; then
  exit 0
else
  echo "Idris package manifest version (${pkgversion}), manifest support version (${pkgsupversion}) and support Makefile version (${supversion}) do not agree!"
  exit 1
fi

