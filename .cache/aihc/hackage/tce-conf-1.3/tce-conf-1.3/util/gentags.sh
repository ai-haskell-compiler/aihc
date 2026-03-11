#! /bin/sh

find src -regex '.*\..?hs' | xargs hasktags -c
