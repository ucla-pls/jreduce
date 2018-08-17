output=$1

mkdir -p "$output/classes"
find "$output/src" -name *.java > "$output/sourcefiles.txt"
javac -cp "$classpath":"$output/classes" -d "$output/classes" -sourcepath "$output/src" @"$output/sourcefiles.txt" 
X=$?

if [[ "$X" == "1" ]]; 
then
    echo "0"
else
    echo "1"
fi
