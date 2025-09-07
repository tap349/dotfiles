for f in *.PNG; do \
  ts=$(mdls -raw -name kMDItemFSCreationDate "$f" | \
  sed -E 's/^([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2}).*/\1\2\3_\4\5\6/'); \
  mv "$f" "${ts}_$f"; \
done

mogrify -quality 40 -format jpg *.PNG && rm *.PNG
