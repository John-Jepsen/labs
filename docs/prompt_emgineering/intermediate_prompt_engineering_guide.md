# Intermediate Prompt Engineering Guide

## Project Type
- Prompt Engineering Guide

## Goal
- Help users refine control, structure, and reliability of AI model outputs

## Audience
- Intermediate users with some experience writing prompts

## Requirements
- **Environment**: Any modern browser
- **Prerequisites**: Comfort with writing basic prompts
- **Dependencies**: Access to ChatGPT, Claude, or similar

---

## Introduction
This guide focuses on improving model performance through advanced prompting techniques:
- Prompt chaining
- Output templating
- Multi-shot prompting
- Token efficiency
- Controlled hallucination mitigation

---

## Advanced Principles

- **Chaining**: Split tasks across multiple prompts (e.g., idea generation → expansion → summary).
- **Memory Anchoring**: Reinforce critical context with repeated key phrases or summaries.
- **Type coercion**: Force format using numbered steps, JSON schema, or pseudo-code structures.
- **Bias mitigation**: Explicitly request diverse answers or multiple perspectives.
- **Output validation**: Instruct model to self-check output before responding.

---

## Prompt Frameworks

| Framework       | Use Case                               | Example Prompt                                                                 |
|-----------------|------------------------------------------|--------------------------------------------------------------------------------|
| Chain-of-thought | Step-by-step reasoning                  | “Solve this problem and explain each step: 23 + 47”                            |
| Zero vs Few-shot | Show model structure via examples       | “Translate to French: Hello → Bonjour”                                         |
| Reframing       | Reduce ambiguity or add perspective     | “Rephrase this from a manager's perspective”                                   |
| JSON-forcing    | Get structured output                   | “Return this summary in JSON: title, bullets, key_points”                      |
| Role layering   | Combine personas                        | “Act as a product manager and UX researcher writing feedback to a dev team.”  |

---

## Step-by-Step Refinement

**Before:**
> Explain the importance of clean code.

**Intermediate:**
> Act as a senior software engineer writing an internal memo to junior developers. Explain why clean code improves team efficiency and long-term maintenance. Give 3 clear examples using bullet points.

---

## Use Cases and Techniques

- **Data preprocessing prompts**
- **Automated QA tasks**
- **Role play for stakeholder communication**
- **Real-time prompt chaining in workflows**
- **Content summarization with constraints**

---

## Prompt Testing and Optimization

- Vary tone and role to test reliability
- Add instruction scaffolding (e.g., “Do not guess if unsure”)
- Observe token usage to avoid truncation
- Use temperature and top_p controls (in API) for output variance

---

## Example Code Snippets

```python
# JSON structure enforcement
prompt = "Summarize this text in the following JSON format: { title: '', summary: '', tags: [] }"

# Few-shot learning
examples = '''
Input: Convert to uppercase → hello
Output: HELLO

Input: Convert to uppercase → apple
Output: APPLE
'''

# Prompt chaining (simulated)
step_1 = "List 3 startup ideas in edtech."
step_2 = "Expand the second idea into a 150-word pitch with market validation and features."
```

---

## Visual Prompt Transformations

```txt
❌ “Make a lesson plan.”

✅ “Design a one-week lesson plan for a 10th grade biology class. Include daily topics, one key activity per day, and end-of-week assessment.”
```

---

## Ethical Guardrails

- Include disclaimers in sensitive or speculative prompts
- Avoid identity simulation unless explicitly needed
- Validate AI-generated data with reliable sources
- Clarify when roleplay should stop or switch

---

## Common Intermediate Errors

| Mistake                         | Correction Tip                                                           |
|----------------------------------|--------------------------------------------------------------------------|
| Under-specifying formats         | Provide schema or structure upfront                                      |
| Assuming output reuse is stable  | Repeat constraints or use rephrased anchors                              |
| Using high-temp without limiters | Use “respond in max 3 sentences” + format directive                      |


---
---
## Active Learning Activity
**Time**: 30 minutes  
**Group Size**: 3–5 people  
**Goal Alignment**: Practice chaining, tone control, and response validation through iterative prompting
**Instructions**:
1. Pick a multi-part content generation task (e.g., job post → summary → FAQ).
2. Assign each stage to a different team member.
3. Draft prompts collaboratively and run them in sequence.
4. At each stage, review the quality and consistency of the output.
5. Apply structure, tone, or persona adjustments to improve each step.
**Deliverables**:
- Prompt chain and outputs
- Role notes and prompt rewrites
- Final outputs and team reflections

---

## Intermediate Prompt Frameworks and Examples

**Frameworks Used**
- Reframing (clarify ambiguous tasks)
- Multimodal (output structure + role + tone)
- Post-processing (model checks own work)
- Chain-of-Thought + Schema

**Examples**

```python
# Stepwise with post-check
prompt = "Explain Newton’s laws briefly. Then check your explanation for accuracy and simplicity."

# Role + Format + Length
prompt = "As a financial advisor, list 3 credit score tips in markdown bullets. Max 20 words each."

# Reframed query
original = "Write about the internet."
revised = "Write a 75-word paragraph explaining how the internet enables cloud computing."

# JSON-coercion
prompt = "Return a list of 3 key plot points from the story in this JSON format: { title: '', event: '' }"
```

**Prompt Rewrite Exercise**
- Write → Reframe → Add structure → Add role → Output limiter → Validate
