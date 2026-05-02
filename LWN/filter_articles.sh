#!/bin/bash

# Filter out non-technical articles from downloaded article list
# Input: article list file (default: .git/lwn_articles_links.new350.txt)
# Output: filtered file (default: .git/lwn_articles_technical.txt)

# Configurable variables
INPUT_FILE=${INPUT_FILE:-.git/lwn_articles_links.new350.txt}
OUTPUT_FILE=${OUTPUT_FILE:-.git/lwn_articles_technical.txt}
EXCLUSION_LOG=${EXCLUSION_LOG:-.git/excluded_articles.log}

# Exclusion patterns (non-technical content)
# Updated patterns to catch more non-technical articles
EXCLUSION_PATTERN='weekly edition|alert summary|announcement|announcing|conference|debconf|fosdem|linuxcon|settlement|policy|legal|obituary|RIP|thunderbird|FCC|net neutrality|quote of the week|quotes of the week|summit|CFP|call for papers|kernel release|released|release status|distribution release|fedora release|ubuntu release|debian release|security advisory|cve|bug.*report|bug.*fix'

# Validate input file
if [[ ! -f "$INPUT_FILE" ]]; then
    echo "Error: Input file '$INPUT_FILE' not found"
    exit 1
fi

# Delete output files if they exist and create fresh
rm -f "$OUTPUT_FILE" "$EXCLUSION_LOG"
echo "# Technical Articles - Filtered $(date)" > "$OUTPUT_FILE"
echo "# Input: $INPUT_FILE" >> "$OUTPUT_FILE"
echo "# Excluded patterns: $EXCLUSION_PATTERN" >> "$OUTPUT_FILE"

echo "# Excluded Articles - Generated $(date)" > "$EXCLUSION_LOG"
echo "# Input: $INPUT_FILE" >> "$EXCLUSION_LOG"
echo "# Excluded patterns: $EXCLUSION_PATTERN" >> "$EXCLUSION_LOG"

# Count articles
total_articles=$(grep -c 'https://lwn.net/Articles/' "$INPUT_FILE")
echo "Processing $total_articles articles from $INPUT_FILE..."

# Process each article
while IFS= read -r line; do
    # Skip comment lines
    if [[ "$line" =~ ^# ]]; then
        continue
    fi
    
    # Skip empty lines
    if [[ -z "$line" ]]; then
        continue
    fi
    
    # Extract article title (between first : and second :)
    title=$(echo "$line" | sed 's|^[^:]*: \([^:]*\) :.*|\1|')
    
    # Check if article should be excluded
    if echo "$title" | grep -q -i -E "$EXCLUSION_PATTERN"; then
        echo "EXCLUDED: $line" >> "$EXCLUSION_LOG"
        echo "Excluded: $title"
    else
        echo "$line" >> "$OUTPUT_FILE"
        echo "Included: $title"
    fi
done < "$INPUT_FILE"

# Count results
included=$(grep -c 'https://lwn.net/Articles/' "$OUTPUT_FILE")
excluded=$(grep -c 'EXCLUDED:' "$EXCLUSION_LOG")

echo ""
echo "Filtering complete!"
echo "==================="
echo "Total articles processed: $total_articles"
echo "Included (technical): $included"
echo "Excluded (non-technical): $excluded"
echo ""
echo "Output files:"
echo "  Technical articles: $OUTPUT_FILE"
echo "  Excluded articles: $EXCLUSION_LOG"
