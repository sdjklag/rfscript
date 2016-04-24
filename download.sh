board=int
nrs="$(seq -s '%2C' $1 $2)"
echo "post_nrs=$nrs&board=$board" | curl 'https://whatisthisimnotgoodwithcomputers.com/get_flags.php' -d "@-" | sed 's/\[//;s/\]//'
