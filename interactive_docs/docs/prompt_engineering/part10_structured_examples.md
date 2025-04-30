# Prompt Engineering: Structured Prompting

## Audience
- Users comfortable with simple prompts, ready to introduce format control and task clarity

## Goal
- Teach prompt structure through formatting, lists, markdown, and input scaffolding

---

## Techniques Introduced

- Add word count or structure requirements
- Force markdown or HTML in response
- Use numbered steps to get ordered results
- Combine clarity and constraints

---

## Prompt Evolution

**Before:**
> Write a summary of AI in education.

**Now:**
> Summarize AI in education in 5 bullet points using markdown. Focus on classroom applications.

---

## Practice Prompts

```python
prompt = "List 5 use cases of blockchain in logistics. Output in markdown bullets."
prompt = "Explain Git branching in 3 ordered steps."
prompt = "Write a 75-word paragraph on renewable energy. Make it suitable for high school students."
```

---
---
## Active Learning Activity
**Time**: 30 minutes  
**Group Size**: 3–4 people  
**Goal Alignment**: Practice formatting AI outputs using markdown, bullets, numbered steps, and concise summaries
**Instructions**:
1. Form teams and assign each a topic (e.g., climate change, blockchain, healthy eating).
2. Each member drafts a prompt for the topic using a different structure (e.g., list, step-by-step, 100-word summary).
3. Run the prompts and review each output for format accuracy and clarity.
4. As a team, improve 1 prompt together and test the revision.
5. Discuss how format instructions affected the model output.
**Deliverables**:
- Topic and prompt variations
- Original vs. revised outputs
- Notes on which formats were best followed

---

## Structured Prompt Techniques: Expanded Examples

**Techniques**
- Markdown formatting (lists, headers, bold text)
- Tables with explicit structure
- Word-limited sections
- Numbered steps for how-to guides
- Multi-block responses (summary + recommendations)

**Examples**

```txt
“List five AI tools.”  
→ “List five AI tools in markdown bullets. Include one use case per tool.”

“Explain version control.”  
→ “Explain version control in three steps. Use numbered list format. Under 100 words total.”

“Give a guide to installing Python.”  
→ “Provide a 3-step setup guide for installing Python 3.10 on macOS. Use markdown and include terminal commands.”

“Describe healthy snacks.”  
→ “List 5 healthy snacks in a markdown table with columns: Snack, Calories, Health Benefit.”
```

**Practice Prompts**

```python
prompt = "List 3 common sorting algorithms in markdown bullet format with one-line descriptions."
prompt = "Create a table of 4 cloud services and their main features."
prompt = "Write a two-paragraph overview of cybersecurity best practices. Max 150 words total."
```
