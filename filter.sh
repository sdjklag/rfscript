pushd "$1"
filter="___"
for i in *.png; do
        filter="$filter"'\|'"$(echo "$i" | sed 's/\.png//')"
done
grep -v "$filter" | sort -k 2
popd
