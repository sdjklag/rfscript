sed 's/},*/}\n/g;s/{"post_nr":"\([0-9]*\)","region":"\([^"]*\)"}/\2 >\1/g' | sort -k 1,2r | sed 's/\(.*\) >\([0-9]*\)/>>\2 \1/' | uniq -f1 | sort -k2
