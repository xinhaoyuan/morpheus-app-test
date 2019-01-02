#!/bin/sh

rm -rf patched/partisan
git clone https://github.com/lasp-lang/partisan.git patched/partisan
(cd patched/partisan; git checkout dcf5d8908440bc5b0be41f669bb9f8f407ba6cd3; git apply ../../partisan.patch)
