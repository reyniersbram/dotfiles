#!/usr/bin/env sh

set -eu

COUNTRIES="BE,FR,NL,GB,DE"

COUNTRIES_QUERY=$(echo $COUNTRIES | sed -E 's/([[:upper:]][[:upper:]]),?/country=\1\&/g')
URL="https://archlinux.org/mirrorlist/?${COUNTRIES_QUERY}protocol=https"

curl -s "${URL}" \
    | sed -e 's/^#Server/Server/' -e '/^#/d' \
    | rankmirrors -n 15 - \
