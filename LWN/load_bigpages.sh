#!/bin/bash

# Load bigpage URLs from LWN.net archives
# This script fetches bigpage links from multiple archive pages

# Configurable variables
PAGE_SIZE=${PAGE_SIZE:-50}  # Default page size, can be overridden
START_OFFSET=${START_OFFSET:-350}  # Starting offset, default 0
END_OFFSET=${END_OFFSET:-399}  # Ending offset, default 500

# Clear the file first
echo "# LWN.net Bigpage URLs - Generated $(date)" > .git/bigpages.txt
echo "# Page size: $PAGE_SIZE, Start offset: $START_OFFSET, End offset: $END_OFFSET" >> .git/bigpages.txt

# Fetch from specified archive pages
for ((offset=START_OFFSET; offset<=END_OFFSET; offset+=PAGE_SIZE)); do
    echo "Fetching offset $offset (page size: $PAGE_SIZE)..."
    curl -s "https://lwn.net/Archives/?n=$PAGE_SIZE&offset=$offset" | \
        grep 'bigpage' | \
        sed 's|.*href="\([^"]*bigpage[^"]*\)".*|https://lwn.net\1|' >> .git/bigpages.txt
    sleep 1  # Rate limiting
    
    # Check if we've reached the end (no more articles)
    if ! curl -s "https://lwn.net/Archives/?n=$PAGE_SIZE&offset=$((offset+PAGE_SIZE))" | grep -q 'bigpage'; then
        echo "Reached end of archives at offset $offset"
        break
    fi
done

# Remove duplicates and sort
sort -u .git/bigpages.txt > .git/bigpages.tmp
mv .git/bigpages.tmp .git/bigpages.txt

echo "Bigpage URLs saved to .git/bigpages.txt"
echo "Total URLs: $(wc -l < .git/bigpages.txt)"
