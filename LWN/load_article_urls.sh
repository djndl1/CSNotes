#!/bin/bash

# Download ALL article links from LWN.net bigpages (no filtering)
# Input: .git/bigpages.txt (bigpage URLs)
# Output: configurable output file (default: .git/lwn_articles_links.new350.txt)

# Configurable variables
OUTPUT_FILE=${OUTPUT_FILE:-.git/lwn_articles_links.new350.txt}

# Delete the output file if it exists and create fresh
rm -f "$OUTPUT_FILE"
echo "# LWN Article Links - Generated $(date)" > "$OUTPUT_FILE"
echo "# Output file: $OUTPUT_FILE" >> "$OUTPUT_FILE"
echo "# All articles downloaded (no filtering applied)" >> "$OUTPUT_FILE"

# Read only valid URLs (lines starting with https://)
while IFS= read -r bigpage; do
    # Skip lines that don't look like URLs
    if [[ ! "$bigpage" =~ ^https://lwn.net/Articles/.*/bigpage ]]; then
        continue
    fi
    
    echo "Processing $bigpage..."
    
    # Fetch the bigpage and extract article info
    curl -s "$bigpage" | \
        grep SummaryHL | \
        # Extract article ID and title from href and link text
        sed -n 's|.*<a href="/Articles/\([0-9]*\)/">\([^<]*\)</a>.*|\1:\2|p' | \
        # Format to standard format with placeholder for summary
        sed 's|^\([0-9]*\):\(.*\)|https://lwn.net/Articles/\1/ : \2 : SUMMARY_PLACEHOLDER|' \
        >> "$OUTPUT_FILE"
    
    sleep 5  # Rate limiting
done < <(grep '^https://lwn.net/Articles/.*/bigpage' .git/bigpages.txt)

echo "All article links downloaded and saved to $OUTPUT_FILE"
echo "Total articles: $(grep -c 'https://lwn.net/Articles/' "$OUTPUT_FILE")"
