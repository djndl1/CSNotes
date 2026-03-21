# AGENTS.md - CSNotes Repository Guidelines

This document provides guidelines for agents working with the CSNotes repository from an educational/instructor's perspective.

## Repository Overview

CSNotes is primarily an **educational note repository** containing comprehensive computer science learning materials, not a traditional software project. The repository serves as:

- **Teaching materials**: Org-mode (.org) files with structured educational content
- **Code examples**: Illustrative implementations to demonstrate concepts
- **Reference documentation**: Technical notes across various CS domains
- **Learning resources**: Links to external materials and tutorials

## Key Repository Characteristics

### Educational Focus
- **Primary content**: Org-mode files containing structured educational materials
- **Code examples**: Serve as illustrations, not production systems
- **Documentation-driven**: Notes explain concepts first, code demonstrates second
- **Multi-language**: Examples across programming languages for comparison

### Submodule Structure
The repository uses git submodules for organization:
- `lang/CodeOfLanguages/` - Programming language examples
- `os/OSCode/` - Operating system programming
- `database/DatabaseTutorials/` - Database systems
- `algorithm/AlgorithmCode/` - Algorithm implementations
- `CP/ProblemCode/` - Competitive programming
- `cloud/middleware/MiddlewareTutorials/` - Distributed systems
- `desktop/GUITutorials/` - Desktop application development

**Note**: Avoid modifying submodule content directly; focus on the main repository's educational content.

## Summaries Index

A `summaries.md` file contains one-sentence descriptions for all `.org` and `.md` files in the repository. Location: `os/windows/summaries.md`. Use this as quick lookup before searching individual files:

```bash
# View summaries for a specific file path pattern
grep "filepath_pattern" os/windows/summaries.md
```

### Using grep and ripgrep
This repository emphasizes effective information retrieval. Key search patterns:

```bash
# Search for concepts across all notes
grep -r "concept_name" --include="*.org" --include="*.md"

# Find code examples for specific topics
rg "algorithm_name" --type org --type md

# Search with context (before/after lines)
grep -A 3 -B 3 "search_term" *.org

# Case-insensitive search for broader results
grep -i "topic" *.md
```

### Effective Search Strategies
- Use `*.org` and `*.md` file patterns to focus on educational content
- Search for both technical terms and conceptual explanations
- Look for cross-references between different topic areas
- Use `-A` and `-B` flags to get context around search results

## Content Organization Principles

### Topic-Based Structure
Content is organized by computer science domains:
- **Languages**: Programming language tutorials and examples
- **Systems**: Operating systems, networking, databases
- **Theory**: Algorithms, data structures, computational theory
- **Applications**: Machine learning, computer vision, etc.

### File Naming Conventions
- `.org` files: Primary educational content with rich formatting
- `.md` files: Supplementary markdown notes
- Code files: Named descriptively within language directories
- Test files: Use `*_test.*` or `*test.*` patterns

## Educational Content Guidelines

### Writing Style for Notes
- **Clear explanations**: Focus on understanding over brevity
- **Structured content**: Use Org-mode headings and lists effectively
- **Code integration**: Include relevant code examples within explanations
- **Cross-references**: Link related concepts across different files

### Code Example Standards
- **Educational clarity**: Code should be easy to understand, not optimized
- **Comments**: Include explanatory comments for non-obvious parts
- **Minimal dependencies**: Keep examples self-contained when possible
- **Multiple implementations**: Show different approaches when educational

### Concept Documentation
- **Start with theory**: Explain concepts before showing implementation
- **Practical examples**: Include real-world applications
- **Common pitfalls**: Document typical mistakes and misunderstandings
- **Further reading**: Include references to external resources

## Build and Execution Patterns

### Code Example Execution
Most code examples are meant for study, not deployment:

```bash
# C/C++ examples (educational focus)
make                    # Build if Makefile exists
gcc -o example example.c  # Manual compilation for study

# Java examples
javac Example.java      # Compile for understanding
java Example            # Run to see behavior

# Python examples
python example.py       # Direct execution for learning
```

### Testing for Understanding
- Run examples to verify understanding
- Modify parameters to explore behavior
- Add print statements for debugging learning
- Compare different implementations

## Content Maintenance Guidelines

### Adding New Content
1. **Choose appropriate location** based on topic domain
2. **Use Org-mode** for structured educational content
3. **Include references** to related existing content
4. **Add cross-links** to help navigation

### Updating Existing Content
- Maintain educational focus when modifying examples
- Preserve historical context when updating
- Update cross-references when moving content
- Verify code examples still compile/run

### Quality Standards
- **Accuracy**: Technical content must be correct
- **Clarity**: Explanations should be understandable
- **Completeness**: Cover topics adequately for learning
- **Consistency**: Follow existing patterns and styles

## Instructor-Focused Workflow

### Creating Learning Materials
1. **Define learning objectives** for each topic
2. **Structure content** with clear progression
3. **Include examples** that illustrate key points
4. **Add exercises** or thought questions
5. **Provide solutions** or guidance

### Maintaining Educational Value
- Focus on conceptual understanding
- Include multiple perspectives on complex topics
- Document common student misconceptions
- Update content based on technology changes

### Cross-Disciplinary Integration
- Show connections between different CS domains
- Include real-world applications of theoretical concepts
- Demonstrate how concepts build upon each other

## Repository-Specific Notes

- This is an **educational repository** - clarity trumps optimization
- **Code serves learning** - examples should be illustrative, not production-ready
- **Documentation is primary** - code exists to support explanations
- **Cross-references matter** - help learners navigate related concepts

Remember: The primary goal is **effective knowledge transfer**, not software development.

## Updating Summary Documentation

### Creating Per-Directory Summaries
For each major directory in the repository, create a `*_summaries.org` file containing:
1. One-sentence summary for each `.org` or `.md` file
2. Format: Markdown table with `filepath` and `summary` columns
3. Sort entries alphabetically by filepath
4. Focus on key concepts/topics covered in each file

**Example:** `/home/djn/Notes/CSNotes/computer_org_summaries.org` contains summaries for all files under `computer_org/`.

### Maintenance Guidelines
- Add new summary files when adding substantial new documentation content
- Keep summaries concise (one sentence maximum)
- Highlight technical topics, not just generic descriptions
- Update existing summaries if file content changes significantly
