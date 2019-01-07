#!/bin/sh

rm -rf patched/partisan
git clone https://github.com/lasp-lang/partisan.git patched/partisan
(cd patched/partisan; git checkout 1282f517e6db05ac101c9a749d8c4412e41cb878; git apply ../../partisan.patch)
