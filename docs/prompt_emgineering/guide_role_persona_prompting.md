
# Prompt Engineering Guide: Role and Persona Control

## Project Type
- Prompt Engineering Guide

## Goal
- Teach users how to control model tone and perspective using personas and roles

## Audience
- Intermediate users ready to simulate character-based responses

## Requirements
- **Environment**: Any modern browser
- **Prerequisites**: Familiarity with format-specific prompting
- **Dependencies**: ChatGPT, Claude, or similar

---

## Introduction
This guide introduces persona prompting:
- Simulating professional or fictional roles
- Changing tone and voice
- Improving user-fit for outputs

---

## Core Principles

- **Persona control**: Tell the model who it is
- **Tone direction**: Friendly, formal, technical
- **Task alignment**: Match persona to expected output

---

## Prompt Patterns

| Pattern        | Example Prompt                                                               |
|----------------|--------------------------------------------------------------------------------|
| Professional   | “Act as a nutritionist. Give a weekly meal plan for a diabetic patient.”      |
| Teacher        | “As a math teacher, explain fractions to a 5th-grade student.”                 |
| Support Agent  | “Pretend you are a support rep. Write a response for a late delivery email.”   |

---

## Step-by-Step Example

**Before:**
> How should I learn SQL?

**After:**
> Act as a data science mentor. Recommend a 4-step plan for learning SQL for analytics.

---

## Use Cases

- Role-specific communication
- Expert simulation
- Guided coaching

---

## Evaluation

- Tone match to role
- Persona relevance
- Clarity and structure

---

## Troubleshooting

| Problem             | Fix                                                      |
|----------------------|----------------------------------------------------------|
| Output too generic   | Add explicit role and task description                   |
| Wrong tone           | Specify “professional,” “friendly,” etc.                 |
| Role confusion       | Add “act as…” or “you are…” at the start                 |

---

## Visual Before/After

```txt
❌ “Explain budgeting.”

✅ “Act as a personal finance coach. Explain budgeting to a recent college grad. Use friendly tone.”
```

---

## Code Examples

```python
prompt = "Act as a hiring manager. Share 3 things you look for in resumes from junior devs."
prompt = "You are a startup founder. Explain how you chose your product-market fit strategy."
prompt = "As a UX researcher, write feedback for a first-time app designer."
```
