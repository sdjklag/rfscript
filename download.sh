board=int
nrs="$(seq -s '%2C' $1 $2)"
echo "post_nrs=$nrs&board=$board" | curl 'https://whatisthisimnotgoodwithcomputers.com/int/get_flags_api2.php' -d "@-" | sed 's/\[//;s/\]//'
