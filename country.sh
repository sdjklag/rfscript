find ~/projects/Extra-Flags-for-4chan/flags/ | grep -F -s "$*" | sed 's/^.*flags\/\([^\/]*\)\/.*$/\1/' | uniq
