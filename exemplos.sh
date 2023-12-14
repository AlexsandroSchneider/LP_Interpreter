files=(*.mylang)

for file in "${files[@]}"; do
    echo "Executando programa: $file"
    cat "$file" | runghc Main.hs
done