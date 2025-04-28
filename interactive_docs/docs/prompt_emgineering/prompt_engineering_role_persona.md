# Prompt Engineering: Role and Persona Control

## Audience
- Users able to specify output structure and now learning how to apply personas for tailored responses

## Goal
- Teach tone management and role simulation to fit user needs

---

## Techniques Introduced

- Impersonate a defined role (teacher, lawyer, support agent)
- Adjust tone: casual, professional, concise
- Combine with structured responses

---

## Prompt Evolution

**Before:**
> Give me tips on writing resumes.

**Now:**
> Act as a senior recruiter. Give 3 resume tips to recent college grads in a friendly tone.

---

## Practice Prompts

```python
prompt = "Act as a UX researcher. List 3 feedback techniques for user interviews."
prompt = "You are a customer service trainer. Write an email template for handling delayed orders."
prompt = "As a career coach, write 3 tips for introverts preparing for interviews."
```

---
---
## Active Learning Activity
**Time**: 30 minutes  
**Group Size**: 2–4 people  
**Goal Alignment**: Explore how role prompts change tone, relevance, and information delivery
**Instructions**:
1. Pick a use case (e.g., job advice, health tips, customer support).
2. Each person writes a prompt using a different persona (e.g., recruiter, doctor, support rep).
3. Run each prompt and compare tone, depth, and output relevance.
4. Pick the best output, then rewrite one weaker prompt to better fit the role.
5. Reflect as a group on how persona framing affects communication.
**Deliverables**:
- Use case and persona list
- Output comparison table
- Revised prompt and reflections

---

## Persona-Based Prompting Techniques

**Persona Types**
- Professionals: doctor, lawyer, software engineer
- Educators: teacher, coach, mentor
- Agents: support rep, chatbot, recruiter
- Fictional: historical figures, aliens, AI with constraints

**Framing Methods**
- “Act as a…” + role
- Set tone: friendly, concise, formal
- Combine role + output format

**Examples**

```txt
“Act as a resume coach. Give 3 bullet-point tips for someone changing careers.”
“Pretend you are a nutritionist. Write a short grocery list for a high-protein diet.”
“You are a high school teacher. Explain why studying history matters. Use simple terms.”
“Act as a product manager writing a one-sentence summary of a failed feature rollout.”
```

**Prompt Expansion Ideas**
- Compare same prompt across roles
- Rewrite using a humorous or sarcastic tone
- Use two characters responding to each other (dialogue)
