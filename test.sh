filename="allComb.txt"
while read -r line
do
    var="$line"
    ./Proj1Test $var >> output.txt
done < "$filename"
