#!/bin/bash

set -ex

elm-make --yes
mkdir -p docs
(cd examples && elm-make --yes Main.elm --output ../docs/index.html)
