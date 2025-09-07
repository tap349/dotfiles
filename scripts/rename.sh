#suffix=" 2.PNG"
#for f in *.PNG; do
#    if [[ "$f" == *"$suffix" ]]; then
#        new_name="${f%$suffix}.PNG"
#        mv "$f" "$new_name"
#    fi
#done

for f in *.JPG; do
  new_name="${f%.JPG}.PNG"
  mv "$f" "$new_name"
done

for f in *.HEIC; do
  new_name="${f%.HEIC}.PNG"
  mv "$f" "$new_name"
done
