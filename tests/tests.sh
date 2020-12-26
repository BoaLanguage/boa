echo "# Boa: A demonstration"

echo "## Running passing tests..."
for f in tests/passing/*; do 
    echo "### File: $f";
    echo "\`\`\`";
    ./bin/boa $f ;
    echo "\`\`\`";
done;
echo "## Running tests without type-checking..."	
for f in tests/passing_nocheck/*; do 
    echo "### File: $f";
    echo "\`\`\`";
    ./bin/boa $f -nocheck;
    echo "\`\`\`";
done;

echo "## Running failing..."	
for f in tests/failing/*; do
    echo "### File: $f";
    echo "\`\`\`";
    ./bin/boa $f ;
    echo "\`\`\`";
done;