
# Prompt Engineering Guide: Prompt Chaining

## Project Type
- Prompt Engineering Guide

## Goal
- Teach users how to split tasks into prompt chains for staged outputs

## Audience
- Users experienced in structured and persona prompting

## Requirements
- **Environment**: Any modern browser
- **Prerequisites**: Comfort with multi-step thinking and follow-up prompting
- **Dependencies**: ChatGPT, Claude, or similar

---

## Introduction
This guide introduces chained prompting:
- Break a problem into sequenced prompts
- Use intermediate outputs
- Build staged reasoning

---

## Core Principles

- **Stage control**: Each prompt handles part of the job
- **Data re-use**: Feed output from step A into step B
- **Workflow modeling**: Mimic human thinking process

---

## Prompt Patterns

| Pattern           | Example Prompt                                                         |
|-------------------|--------------------------------------------------------------------------|
| Idea → Expand     | “List 3 marketing ideas.” → “Expand the second into a 100-word pitch.”   |
| Fact → Format     | “Give a fact.” → “Format that fact as a tweet under 280 characters.”     |
| Extract → Summarize | “Extract key points.” → “Summarize those in a markdown list.”         |

---

## Step-by-Step Example

**Before:**
> What’s a good startup idea?

**After:**
1. “List 3 startup ideas for remote teams.”
2. “Pick the first and write a 100-word product pitch.”
3. “Summarize that pitch as a one-line tweet.”

---

## Use Cases

- Product ideation
- Multi-format generation
- Structured thinking assistance

---

## Evaluation

- Each output links logically
- Stages match intended task depth
- Output from one step feeds next step cleanly

---

## Troubleshooting

| Problem              | Fix                                                             |
|-----------------------|-----------------------------------------------------------------|
| Gaps between steps    | Restate context in each prompt                                 |
| Lost focus            | Add reminders or stage labels                                  |
| Model hallucination   | Reduce temperature, clarify stage boundaries                   |

---

## Visual Before/After

```txt
❌ “Write a startup idea.”

✅ 
1. “List 3 startup ideas for seniors.”
2. “Pick one. Write a 3-sentence pitch.”
3. “Summarize in a title and tagline.”
```

---

## Code Examples

```python
prompt1 = "List 3 AI use cases in education."
prompt2 = "Take the second and describe its impact in 100 words."
prompt3 = "Summarize that as a tweet."
```
