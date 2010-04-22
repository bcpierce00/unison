#!/bin/sh
export UNISON_LOCALE=_build/share/locale 
exec ./unison "$@"
