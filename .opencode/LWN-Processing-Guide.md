# LWN Article Processing Guide

## Overview
This guide documents the workflow for batch processing LWN.org articles from link files to summarized wiki entries.

## File Formats

### Input: lwn_articles_links.new.txt
- **Location**: `.git/lwn_articles_links.new.txt`
- **Format**: `#HL line#hash#anchor|#HL line#hash#anchor|https://lwn.net/Articles/ID/: Title`
- **Lines**: 433 articles total
- **Header**: First 2 lines are comments/headers

### Output: LWN.org
- **Location**: `LWN.org`
- **Format**: `- [[article_link][article_title]]: article_summary`
- **Structure**: Org-mode file with headers and content sections

## ⚠️ CRITICAL WARNINGS

### Context Limit Safety
- **NEVER read entire LWN.org file** - it's massive (2000+ lines) and will hit context limits
- **Read lwn_articles_links.new.txt line by line** - avoid reading large chunks
- **Use grep for existence checks**: `grep "article_link" LWN.org`
- **Process in small batches** to avoid overwhelming the system

### Quality Assurance
- **Check existing articles**: Some articles in LWN.org may be poorly summarized or badly formatted
- **Fix poor summaries**: After checking existence, verify summary quality and fix if needed
- **Standardize formatting**: Ensure all entries follow `- [[link][title]]: summary` format
- **Review for consistency**: Maintain consistent summary length and technical depth

## Processing Workflow

### Batch Processing Pattern
- **Batch Size**: 5 articles per batch
- **Subagent**: Use general subagent for processing
- **Check**: Use grep to verify article doesn't exist (DO NOT read full LWN.org)
- **Fetch**: Use webfetch to get article content
- **Summarize**: Create 1-3 sentence technical summary
- **Format**: Add to LWN.org with proper formatting

### Steps for Each Batch
1. **Read Links**: Extract next 5 articles from links file (line by line)
2. **Check Existence**: Use `grep "article_link" LWN.org` for each article
3. **Quality Check**: If article exists, verify summary quality and formatting
4. **Fix Poor Entries**: Improve badly formatted or poorly summarized existing articles
5. **Fetch Content**: Use webfetch for new articles only
6. **Create Summary**: Focus on key technical concepts
7. **Add Entry**: Use format: `- [[link][title]]: summary`
8. **Verify**: Check formatting and integration

## Context Files Used
- `/data/data/com.termux/files/home/.config/opencode/context/project-intelligence/lwn-processing-workflow.md`
- `/data/data/com.termux/files/home/.config/opencode/context/core/context-system/operations/extract.md`
- `/data/data/com.termux/files/home/.config/opencode/context/core/context-system/operations/harvest.md`

## Success Criteria
- ✅ No duplicate articles added
- ✅ Concise, informative summaries
- ✅ Proper org-mode formatting
- ✅ Batch processing to avoid system overload
- ✅ Error handling for failed fetches

## Example Entry
```
- [[https://lwn.net/Articles/601823/][Kuhn: Why your project doesn't need a contributor licensing agreement]]: 
  Bradley Kuhn argues that CLAs create unnecessary barriers for open source contributors and 
  suggests using Developer Certificate of Origin instead for simpler contribution management.
```

## Next Steps
- Process remaining articles in batches of 5
- Monitor for new articles periodically
- Update summaries if articles are revised
- Consider automated scheduling for regular updates