
# Prompt Engineering Guide: Structured Prompting

## Project Type
- Prompt Engineering Guide

## Goal
- Teach users how to apply output formatting and structure to guide the model response

## Audience
- Users with experience in simple prompts seeking control over structure and clarity

## Requirements
- **Environment**: Any modern browser
- **Prerequisites**: Understanding of basic prompts
- **Dependencies**: ChatGPT, Claude, or similar

---

## Introduction
This guide introduces structured prompting techniques:
- Markdown formatting
- Ordered lists and steps
- Word and format constraints

---

## Core Principles

- **Format directives**: Markdown, numbered lists, tables
- **Content scope**: Add word count or topic focus
- **Clarity**: Avoid vague commands by narrowing topic and output type

---

## Prompt Patterns

| Pattern          | Example Prompt                                                                 |
|------------------|---------------------------------------------------------------------------------|
| Markdown bullets | “List 5 benefits of AI in markdown bullet format.”                             |
| Numbered steps   | “Explain how to create a Git branch in 3 ordered steps.”                       |
| Word constraint  | “Write a 100-word summary on climate policy.”                                 |

---

## Step-by-Step Example

**Before:**
> Tell me about machine learning.

**After:**
> Write a 3-point summary of machine learning applications in markdown bullet format. Keep it under 75 words.

---

## Use Cases

- Structured summaries
- Quick-reference guides
- Short-form technical documentation

---

## Evaluation

- Count output items (matches list?)
- Check formatting (matches request?)
- Confirm brevity and relevance

---

## Troubleshooting

| Problem               | Fix                                                 |
|------------------------|-----------------------------------------------------|
| Too long               | Add word limits                                     |
| Wrong format           | Specify markdown, bullets, or numbered list         |
| Missing clarity        | Add topic focus and number of items requested       |

---

## Visual Before/After

```txt
❌ “Describe deep learning.”

✅ “List 3 real-world uses of deep learning in markdown bullets. Max 60 words.”
```

---

## Code Examples

```python
prompt = "List 5 open-source LLMs in markdown bullet format."
prompt = "Explain HTTP in 3 numbered steps under 50 words."
prompt = "Give a 100-word intro to graph databases. Use simple language."
```
