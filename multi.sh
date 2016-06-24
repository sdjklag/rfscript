for i in $(seq $1 -50000 $2); do
        echo $i >&2
        ./download.sh $[ $i - 50000 ] $i
        sleep 45
done # sed {} ...


# !!! replace }{ with },{ in the output
