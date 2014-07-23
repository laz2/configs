svn status | grep '^[?I]' |  sed "s/^[?I] //" | xargs -I{} rm -rf "{}"
