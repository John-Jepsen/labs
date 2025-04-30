
# Prompt Engineering Guide for AI Models

## Project Type
- Prompt Engineering Guide

## Goal
- Teach beginners how to write clear, effective prompts for AI models

## Audience
- Beginners with no prior experience in prompt design

## Requirements
- **Environment**: Any modern browser
- **Prerequisites**: Understand basic written instructions
- **Dependencies**: Access to ChatGPT, Claude, or similar

---

## Introduction to Prompt Engineering
Prompt engineering is the practice of designing inputs (prompts) that guide AI models to produce useful outputs.

Good prompts:
- Are clear and specific
- Provide context
- Match the AI’s capabilities to the task

---

## Core Principles

- **Be direct**: Say what you want, avoid vague phrasing.
- **Specify format**: Ask for lists, tables, markdown, etc.
- **Control tone**: Use system instructions or persona cues.
- **Provide context**: Embed key information into the prompt.
- **Test and refine**: Prompt design is iterative.

---

## Prompt Patterns

| Type                     | Example Prompt                                                                 |
|--------------------------|--------------------------------------------------------------------------------|
| Simple Q&A               | “What is the capital of Japan?”                                               |
| Multi-step Instructions  | “List 3 ideas for a blog. Then expand each into a 100-word paragraph.”        |
| Format Control           | “Summarize this article in bullet points.”                                   |
| Role-based               | “Act as a nutritionist. Recommend meals for a vegan athlete.”                |
| System Prompt            | “You are a friendly tutor helping a 10-year-old learn division.”             |

---

## Step-by-Step Example

**Before:**
> Tell me about dogs.

**After:**
> Give me a bulleted list of five dog breeds suitable for families with kids. Include size, temperament, and grooming needs.

**Why it’s better:**
- Adds structure
- Focuses the task
- Guides formatting

---

## Common Use Cases

- Q&A bots  
- Writing assistants  
- Simulated conversations  
- Text formatting tasks  
- Code generation helpers  

---

## Evaluation Methods

- **Compare outputs** across prompt variations  
- **Check consistency** over multiple runs  
- **Measure clarity**: Are outputs on-topic and structured?  
- **Ask humans** to rank responses  

---

## Troubleshooting

| Problem                  | Fix                                                                          |
|--------------------------|-------------------------------------------------------------------------------|
| Output too vague         | Add more constraints (e.g., format, word count)                              |
| Model misunderstands     | Rephrase with simpler terms                                                  |
| Too verbose              | Add: “Be concise. Max 3 sentences.”                                          |
| Hallucinated info        | Ask model to cite sources or say “I don’t know”                              |

---

## Special Focus Areas

- **Clarity**: Use short, structured prompts  
- **Context Management**: Reuse key info in long chats  
- **Limitations**: Models make things up; verify important data  
- **Ethics**: Avoid leading prompts, respect boundaries  
- **Beginner Traps**: Don’t assume the model “knows” what you mean without spelling it out  

---

## Visual Before/After Examples

```txt
❌ “Explain climate change.”

✅ “Write a 5-sentence summary of climate change causes and effects for high school students. Use simple language.”
```

---

## Example Code Snippets

```python
# Simple Q&A
prompt = "What are three benefits of meditation?"

# Multi-step
prompt = "List 3 fitness goals. Then write one sentence for how to achieve each."

# Format control
prompt = "Explain Kubernetes in a markdown table with columns: Concept, Description."

# Role-play
prompt = "You are a personal finance coach. Give tips for saving money on groceries."

# System prompt example (ChatGPT API-style)
system_prompt = "You are a concise expert in computer networking. Respond in two sentences max."
```
