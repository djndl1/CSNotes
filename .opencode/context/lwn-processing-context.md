# LWN Article Processing Context

## Purpose
Standardized workflow for batch processing LWN.org articles from link files to summarized wiki entries.

## ⚠️ CRITICAL WARNINGS
- **NEVER read entire LWN.org** - only use grep for article ID checks (e.g., `grep "684830" LWN.org`)
- **Read lwn_articles_links.new.txt line by line** - avoid large reads
- **Process in small batches** to avoid context limits
- **Fix poor entries**: Check existing articles for bad formatting or poor summaries

## Revised Architecture: Single Subagent Pattern (Stable)

### Lessons Learned from Dual Subagent Approach
The dual subagent (Fetcher/Summarizer) approach proved unstable due to:
- **Timing issues**: Subagents timing out on large article batches
- **Context limits**: Multiple subagents consuming too much context
- **Coordination complexity**: Hard to maintain proper sequencing
- **JSON file management**: TMPDIR evaluation and file path issues

### Current Stable Approach: Single General Subagent
Use a single `general` subagent per batch with simplified workflow:

```
Lines from links file → Single Subagent → Direct append to LWN.org
```

### Workflow (Single Subagent)
1. **Select batch**: Extract 5-10 articles from links file using line-by-line reading
2. **Process batch**: Single subagent fetches, filters, summarizes, and appends
   - **CRITICAL**: Subagent must NOT read LWN.org file - only process provided articles
   - Use grep for duplicates ONLY when explicitly instructed
3. **Quality check**: Verify formatting and skip duplicates (using grep on article numbers only when needed)
4. **Continue**: Move to next batch when complete

### Command Template (Single Subagent)
```javascript
task(
  subagent_type="general",
  description="Process LWN batch N",
  prompt="Please process this batch of LWN articles and create technical summaries for CSNotes.\n\n**🚨 CRITICAL INSTRUCTIONS - READ FIRST:**\n- **NEVER read the entire LWN.org file** - it's 2700+ lines and wastes context\n- **NEVER use grep on LWN.org unless checking for specific article IDs** (e.g., `grep \"684830\" LWN.org`)\n- **NEVER search LWN.org for patterns, titles, or general content**\n- **Only check for duplicates by article number if explicitly instructed**\n- **Focus only on the articles provided in this batch**\n\nArticles:\n- https://lwn.net/Articles/XXXXXX/ TITLE\n- https://lwn.net/Articles/XXXXXX/ TITLE\n...\n\nInstructions:\n1. For each URL, fetch the article using webfetch\n2. Analyze if it contains substantive technical content (skip if it's just release announcements, news, or non-technical content)\n3. For technical articles, create a concise 1-3 sentence summary focusing on technical details\n4. Output format for each technical article: - [[URL][Title]]: Summary\n\nFocus on:\n- Technical implementations, algorithms, APIs\n- Programming concepts, kernel patches, security mechanisms\n- Research papers, significant technical innovations\n\nSkip:\n- Release announcements, conference news, vulnerability reports\n- Distribution-specific news, opinion pieces without technical substance\n- Quotes of the week, newsletters, book promotions\n\nReturn only the formatted technical summaries, one per line."
)
```

## File Locations
- **Filtered Links**: `.git/lwn_articles_links.new2.filtered.txt` (1360 articles)
- **Output**: `LWN.org` (org-mode format)

## Batch Parameters (Optimized)
- **Size**: 5-10 articles per batch (depends on content density)
- **Progress**: Currently at line ~1160, ~200 articles remaining
- **Throughput**: ~5 technical articles per batch average
- **Total processed**: ~1000 articles, ~200 remaining

## Quality Standards
- **Technical focus**: Only substantive technical content (kernel patches, APIs, algorithms)
- **Summary length**: 1-3 sentences focusing on technical implementation
- **Format**: `- [[URL][Title]]: Technical summary`
- **Duplicates**: Check with `grep` only for article ID patterns (e.g., `grep "684830" LWN.org`), never search for other patterns

## Efficiency Improvements
- **Title scanning**: Quickly identify technical vs non-technical content by title
- **Batch sizing**: Larger batches (10 articles) when content density is high
- **Direct processing**: No intermediate JSON files or TMPDIR management
- **Parallel processing**: Process multiple independent batches when stable

## Grep Usage Guidelines
- **✅ ALLOWED**: `grep "684830" LWN.org` - exact article ID matching
- **✅ ALLOWED**: `grep "Articles/684830" LWN.org` - URL pattern matching
- **❌ FORBIDDEN**: `grep "kernel" LWN.org` - general content searching
- **❌ FORBIDDEN**: `grep "memory" LWN.org` - topic-based searching
- **❌ FORBIDDEN**: `grep -i "linux" LWN.org` - case-insensitive pattern searching
- **Always use specific article IDs when duplicate checking is needed**

## Content Filtering Criteria (What to Include)
✅ **Include**: Kernel patches, filesystem improvements, security mechanisms, programming language features, distributed systems, research papers

❌ **Skip**: Release announcements, conference news, vulnerability reports, distribution-specific news, opinion pieces, quotes of the week

## Current Status
- **Progress**: ~1000 articles processed, ~200 remaining
- **Rate**: ~5 technical articles per batch
- **Stability**: Single subagent approach working reliably
- **Quality**: High-quality technical summaries maintained

## Error Handling
- Skip articles that fail to fetch
- Continue processing remaining articles in batch
- Verify final formatting before completion
- Log failures for manual review if needed

## Integration Points
- Uses existing context: `lwn-processing-workflow.md`
- Follows patterns from: `extract.md`, `harvest.md`
- Compatible with Mastra AI parallel processing concepts