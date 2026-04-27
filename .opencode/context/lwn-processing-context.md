# LWN Article Processing Context

## Purpose
Standardized workflow for batch processing LWN.org articles from link files to summarized wiki entries.

## ⚠️ CRITICAL WARNINGS
- **NEVER read entire LWN.org** - use grep for existence checks
- **Read lwn_articles_links.new.txt line by line** - avoid large reads
- **Process in small batches** to avoid context limits
- **Fix poor entries**: Check existing articles for bad formatting or poor summaries

## Architecture: Two-Subagent Pattern

### Components
1. **Fetcher**: Reads links from lwn_articles_links.new.txt, fetches article content via webfetch, writes JSON to `$TMPDIR/lwn_batch_N.json` (evaluated via `echo $TMPDIR` at runtime)
2. **Summarizer**: Reads JSON from `$TMPDIR/lwn_batch_N.json`, creates 1-3 sentence summaries, appends to LWN.org

### Workflow
```
Line N..N+4 from links file → Fetcher → $TMPDIR/lwn_batch_N.json → Summarizer → LWN.org
                                                         ↑
                                                         └── Fetcher N+1 starts here (pipelining)
```

### Key Principles
- **Hard Ordering**: Fetcher completes BEFORE Summarizer starts (prevents infinite wait)
- **Main Agent Orchestrates**: Subagents NEVER trigger next batches - only main agent decides when to continue
- **Pipelining**: Fetcher N+1 can start while Summarizer N finishes (staggered parallel)
- **JSON Intermediate**: Fetcher writes JSON, Summarizer reads JSON ($TMPDIR/lwn_batch_N.json format)

### Subagent Responsibilities
| Agent | Reads | Writes | Must NOT |
|-------|-------|--------|----------|
| Fetcher | Links file (lines) | $TMPDIR/lwn_batch_N.json | Append to LWN.org |
| Summarizer | $TMPDIR/lwn_batch_N.json | LWN.org (append only) | Trigger next batch |
| Main Agent | Context files, status | Orchestrates all | Read LWN.org directly |

## Quick Reference

### Command Template
```bash
# Process batch N: Fetch 5 articles, write JSON to $TMPDIR/lwn_batch_N.json
# IMPORTANT: First evaluate TMPDIR with: echo $TMPDIR
task(
  subagent_type="general",
  description="Fetch LWN batch N",
  prompt="First run: echo $TMPDIR to get the actual path. Fetch these articles via webfetch and write to ${TMPDIR}/lwn_batch_N.json as JSON array with format: [{\"url\": \"...\", \"title\": \"...\", \"content\": \"...\"}, ...]\n\nArticles:\n- https://lwn.net/Articles/XXXXXX/ TITLE\n- https://lwn.net/Articles/XXXXXX/ TITLE\n..."
)

# Process batch N: Read JSON, summarize, append to LWN.org  
task(
  subagent_type="general", 
  description="Summarize LWN batch N",
  prompt="First run: echo $TMPDIR to get the actual path. Read ${TMPDIR}/lwn_batch_N.json. The JSON contains articles with url, title, and content fields. For each article: create 1-3 sentence technical summary. Use grep to check LWN.org for duplicates (DO NOT read entire file - it has 2400+ lines). Append new entries to LWN.org with format: - [[url][title]]: summary. Fix poorly formatted existing articles if found."
)
```

### Duplicate Prevention
- **BEFORE adding any entry**: Run `grep "article_number" LWN.org` to check if exists
- **If exists**: Skip (don't add duplicate)
- **If not exists**: Add with proper format
- **Never assume**: Always check even if previous batches were clean

### File Locations
- **Links**: `.git/lwn_articles_links.new.txt` (433 articles)
- **Output**: `LWN.org` (org-mode format)

### Batch Parameters
- **Size**: 5 articles per batch
- **Skip**: First 2 lines (headers)
- **Format Check**: Verify `- [[link][title]]: summary` format
- **Safety**: Read links file line by line, use grep for LWN.org checks

## Workflow Summary (Two-Subagent)

### Fetcher Stage
1. **Evaluate TMPDIR**: Run `echo $TMPDIR` to get actual path
2. **Read Links**: Extract next 5 articles from links file (line by line)
3. **Fetch Content**: webfetch each article URL
4. **Write JSON**: Output to `${TMPDIR}/lwn_batch_N.json`

### Summarizer Stage
4. **Evaluate TMPDIR**: Run `echo $TMPDIR` to get actual path
5. **Read JSON**: Load `${TMPDIR}/lwn_batch_N.json`
6. **Check Duplicate**: `grep "article_number" LWN.org` (DO NOT read LWN.org)
6. **Summarize**: 1-3 sentence technical summary for each NEW article
7. **Fix Poor Entries**: Improve any badly formatted existing articles found
8. **Append**: Add new entries to LWN.org

### Infinite Wait Prevention
- **Fetcher MUST complete before Summarizer starts** (hard ordering constraint)
- **Main agent monitors completion** and triggers next batch after both stages finish
- Subagents should NOT trigger next batches - only main agent orchestrates

### $TMPDIR Note
- Evaluate at runtime with `echo $TMPDIR` - value varies by environment
- Common locations: `/tmp`, `/data/data/com.termux/files/usr/tmp/`, `$HOME/tmp`
- If JSON not in expected location, check `.git/` or current working directory
- Fetcher MUST evaluate TMPDIR before writing; Summarizer MUST evaluate before reading

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