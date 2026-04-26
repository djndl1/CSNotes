# LWN Article Processing Context

## Purpose
Standardized workflow for batch processing LWN.org articles from link files to summarized wiki entries.

## ⚠️ CRITICAL WARNINGS
- **NEVER read entire LWN.org** - use grep for existence checks
- **Read lwn_articles_links.new.txt line by line** - avoid large reads
- **Process in small batches** to avoid context limits
- **Fix poor entries**: Check existing articles for bad formatting or poor summaries

## Quick Reference

### Command Template
```bash
# Process next batch of 5 articles (SAFE VERSION)
task(
  subagent_type="general",
  description="Process LWN articles batch",
  prompt="Process next 5 articles from lwn_articles_links.new.txt. Use grep to check LWN.org for duplicates (DO NOT read entire file). Fix poorly formatted or summarized existing articles. Use webfetch for new articles. Format: - [[link][title]]: 1-3 sentence summary. Read links file line by line."
)
```

### File Locations
- **Links**: `.git/lwn_articles_links.new.txt` (433 articles)
- **Output**: `LWN.org` (org-mode format)

### Batch Parameters
- **Size**: 5 articles per batch
- **Skip**: First 2 lines (headers)
- **Format Check**: Verify `- [[link][title]]: summary` format
- **Safety**: Read links file line by line, use grep for LWN.org checks

## Workflow Summary

1. **Extract Links**: Read next 5 articles from links file (line by line)
2. **Duplicate Check**: Use `grep "article_link" LWN.org` (DO NOT read full file)
3. **Quality Check**: Verify existing article summaries and formatting
4. **Fix Poor Entries**: Improve badly formatted or poorly summarized articles
5. **Content Fetch**: webfetch new articles only
6. **Summarize**: 1-3 sentence technical summary
7. **Format**: Org-mode entry with proper spacing
8. **Verify**: Check formatting and integration

## Quality Standards
- **Summaries**: Focus on technical concepts, 1-3 sentences
- **Formatting**: Maintain org-mode structure with blank lines
- **Accuracy**: No duplicates, correct links and titles
- **Efficiency**: Batch processing to avoid system overload
- **Quality Assurance**: Fix poorly summarized or formatted existing articles

## Error Handling
- Skip articles that fail to fetch
- Log failures for manual review
- Continue processing remaining articles in batch
- Verify final formatting before completion

## Integration Points
- Uses existing context: `lwn-processing-workflow.md`
- Follows patterns from: `extract.md`, `harvest.md`
- Compatible with Mastra AI parallel processing concepts

## Status Tracking
- **Processed**: First 5 articles completed
- **Remaining**: 428 articles across 86 batches
- **Last Update**: 2026-04-26