# Prompt Engineering: Chained and Multi-Prompt Setup

## Audience
- Users fluent in structured prompting and persona control, ready to split complex tasks

## Goal
- Teach chained prompting and multi-turn workflows

---

## Techniques Introduced

- Split complex prompts into sub-steps
- Link outputs as inputs
- Use clarifying follow-ups

---

## Prompt Evolution

**Before:**
> Write a blog post on healthy habits.

**Now (chained):**
1. “List 3 unique habits for health improvement.”
2. “Take the second habit and write a 100-word section explaining its science.”
3. “Summarize this section for Instagram.”

---

## Practice Prompts

```python
prompt1 = "List 3 tools used in data visualization."
prompt2 = "Choose one. Describe its pros/cons for beginners."
prompt3 = "Summarize your response in a tweet (under 280 chars)."
```

---
---
## Active Learning Activity
**Time**: 30 minutes  
**Group Size**: 2–3 people  
**Goal Alignment**: Break down multi-part tasks into effective chained prompts
**Instructions**:
1. As a group, pick a 3-part workflow (e.g., idea → pitch → tweet).
2. Divide steps so each person writes one prompt.
3. Pass each output to the next person as input.
4. Review the full chain at the end: Are transitions smooth? Is logic consistent?
5. Refine any step that breaks flow or returns unclear info. Rerun chain.
**Deliverables**:
- Prompt chain (3 steps)
- Output after each step
- Notes on what broke and what worked

---

## Prompt Chaining Techniques and Templates

**Chain Types**
- Stepwise workflows
- Output formatting at each stage
- Data transformation chains
- Role-switching per prompt
- Creative expansion (idea → product → tagline)

**Prompt Chain Templates**

```txt
Step 1: “List 3 eco-friendly startup ideas.”
Step 2: “Pick the second one. Write a 150-word pitch with benefits and target audience.”
Step 3: “Summarize that pitch as a tagline under 12 words.”

—

Step 1: “Extract 5 key terms from this paragraph.”
Step 2: “Define each term in one sentence.”
Step 3: “Put those definitions in a markdown list.”

—

Step 1: “You are a CEO. Explain your company’s mission in 3 sentences.”
Step 2: “You are a PR manager. Rewrite that mission in a tone suitable for social media.”
```

**Variants**
- Output validation after step 2
- Role handoffs between steps
- Insert user feedback mid-chain
