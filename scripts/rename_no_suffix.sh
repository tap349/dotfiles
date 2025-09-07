suffix=" 2.PNG"
for f in *.PNG; do
    if [[ "$f" == *"$suffix" ]]; then
        new_name="${f%$suffix}.PNG"
        mv "$f" "$new_name"
    fi
done
