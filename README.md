<div align="center">
  <img src="https://img.shields.io/badge/Framework-Hybrid%20AI-purple?style=for-the-badge&logo=github" alt="Framework Badge">
  <img src="https://img.shields.io/badge/Language-Python%20%26%20Lisp-blue?style=for-the-badge&logo=python" alt="Python & Lisp Badge">
  <img src="https://img.shields.io/badge/Paradigm-Neuro--Symbolic%20System-orange?style=for-the-badge&logo=tensorflow" alt="Neuro-Symbolic System Badge">
  <img src="https://img.shields.io/github/stars/your-username/mozart?style=for-the-badge&color=gold" alt="Stars Badge">
</div>

<br>

<h1 align="center">
  Mozart: A Hybrid AI System for Music Generation
</h1>

<p align="center">
  <i>Composing music with a soul: a dialogue between creative intuition and logical structure.</i>
</p>

<br>

>[!IMPORTANT]
> Mozart is architected as a lightweight client-server application, making it ideal for interactive creative sessions and integration into digital audio workstations (DAWs) or other artistic tools.

## 🌟 Table of Contents

-   [🌟 Table of Contents](#-table-of-contents)
-   [✨ Introduction](#-introduction)
-   [💡 Core Design Philosophy: From Probabilistic Notes to Structured Emotion](#-core-design-philosophy-from-probabilistic-notes-to-structured-emotion)
-   [🧠 Architecture Core: The Hybrid Reasoning Engine](#-architecture-core-the-hybrid-reasoning-engine)
-   [🧩 Architectural Components in Detail](#-architectural-components-in-detail)
    -   [The LLM Front-End (The Creative Director)](#the-llm-front-end-the-creative-director)
    -   [The Lisp Expert System (The Composer)](#the-lisp-expert-system-the-composer)
    -   [The Command & Fact Bridge](#the-command--fact-bridge)
-   [🔄 How It Works: The Composition Loop](#-how-it-works-the-composition-loop)
-   [🚀 Unique Advantages & Innovations](#-unique-advantages--innovations)
-   [🤝 Contribution](#-contribution)

<br>

---

## ✨ Introduction

This project introduces **Mozart**, a novel hybrid AI architecture designed for high-fidelity, emotionally resonant music generation.

**Mozart** re-conceptualizes AI composition by treating music not as a flat sequence of notes, but as a rich, **structured entity governed by rules and emotion**. It moves beyond the limitations of standard generative models, which often struggle with long-range harmonic consistency and producing music that feels coherent (e.g., creating "probabilistic soup"). The architecture synergizes the powerful natural language understanding of Large Language Models (LLMs) with the rigorous, explicit logic of a Lisp-based Expert System. This fusion creates a highly controllable and musically coherent generation system capable of understanding high-level creative direction and executing it based on a formal knowledge of music theory.

<br>

---

## 💡 Core Design Philosophy: From Probabilistic Notes to Structured Emotion

**Mozart is not just another generative model; it represents a fundamental shift in how we approach AI creativity.** We believe the next leap in AI-powered artistic tools requires models that can reason about their craft with the same structural awareness as a human artist. It ingeniously translates a user's abstract creative intent into a formal, rule-driven composition process.

> "The future of AI music lies in moving from probabilistic note generation to structured emotional reasoning."

This design aims to surmount the inherent limitations of conventional LLMs in maintaining harmonic structure, respecting musical theory, and avoiding the "aimless wandering" that plagues many purely neural music generation systems.

<br>

---

## 🧠 Architecture Core: The Hybrid Reasoning Engine

The **Hybrid Reasoning Engine** stands as the **bedrock** of the Mozart architecture and serves as the **"Single Source of Truth"** for both creative intent and logical execution. This mechanism liberates the system from the constraints of being either a rigid, inflexible rule engine or a purely chaotic generative model.

**Functionality:**
The system simultaneously leverages two distinct but complementary AI paradigms:
1.  **The LLM:** Understands the user's creative, emotional prompt in natural language (e.g., "a sad, slow piano melody").
2.  **The Expert System:** Possesses a deep, explicit knowledge of music theory (harmony, rhythm, scales) encoded as a set of rules.

Every note generated is therefore informed not only by a high-level creative direction but also by a strict set of logical rules that ensure the final piece is musically sound and coherent. This is a crucial mechanism for bridging the gap between human creativity and machine execution.

<br>

---

## 🧩 Architectural Components in Detail

The different components within Mozart fulfill specialized roles to achieve a holistic compositional process, driving systemic intelligence through a clear division of creative and logical labor.

### The LLM Front-End (The Creative Director)
*   **Objective:** To capture the user's high-level artistic vision and translate it into a machine-readable format.
*   **Characteristics:** This operates as an "interpreter." Using a Python front-end, it takes a user's prompt (e.g., "Create a happy, upbeat tune") and instructs an LLM to generate a single, precise Lisp command that sets up the initial conditions for the composition.

### The Lisp Expert System (The Composer)
*   **Objective:** To provide a persistent, rule-based engine for generating musically coherent sequences.
*   **Functionality:** This is the heart of the logical engine. Written in Common Lisp for its powerful symbolic processing capabilities, the expert system contains a "knowledge base" of music theory rules. It runs a forward-chaining inference engine that takes initial "facts" (like `(style happy)`) and generates notes step-by-step, ensuring every decision adheres to its programmed musical knowledge.

### The Command & Fact Bridge
*   **Role:** The crucial link where the two paradigms converge.
*   **Functionality:** The LLM's output is not music; it's a Lisp command string (e.g., `(generate-music '((start-generation) (style sad)) "rules/sad-minor.lisp")`). This command is sent to the Lisp server, which executes it. The command's arguments become the initial "facts" in the Expert System's "working memory," directly seeding the logical composition process with the user's creative intent. This transforms an abstract idea into a concrete starting point for logical reasoning.

<br>

---

## 🔄 How It Works: The Composition Loop

The operation of Mozart follows a clear, interactive cycle designed for real-time creative partnership:

1.  **User Prompt:** The user provides a creative brief in natural language via the Python front-end.
2.  **LLM Translation:** The prompt is sent to an LLM, which translates it into a structured Lisp function call, selecting the appropriate rule-set and initial parameters (e.g., style, tempo, key).
3.  **Command Dispatch:** The Python client sends the Lisp command string to the running Lisp server over a socket.
4.  **Expert System Inference:** The Lisp server receives the command, loads the specified rule-set, and initializes its working memory with the facts provided. The inference engine then begins a cycle of matching facts to rules and firing those rules to generate notes, advance time, and change chords.
5.  **Result Aggregation:** The process continues until a termination rule is fired. The expert system gathers all generated `(generated-note ...)` facts and sends them back to the Python client as a structured list.
6.  **Output:** The Python client displays the final musical composition to the user.

<br>

---

## 🚀 Unique Advantages & Innovations

While "black-box" neural models for music generation can produce impressive results, they often lack **control, coherence, and interpretability.**

**This is precisely the direction that Mozart is designed to profoundly explore and address.**

**Mozart**, through its unique **hybrid neuro-symbolic architecture**, provides users with:

*   **Deep Creative Control:** Instead of just hitting "generate," the user acts as a creative director, providing high-level instructions that the system follows logically.
*   **Guaranteed Musical Coherence:** The Expert System acts as a "safety net," ensuring that no matter the creative input, the output will always adhere to the principles of music theory encoded in its knowledge base. This eliminates musically "incorrect" or nonsensical results.
*   **Interpretability and Extensibility:** The system's musical knowledge is not hidden in a matrix of weights; it is explicitly written in human-readable Lisp rules. Want to teach Mozart a new chord progression or a specific rhythmic style? Simply add a new rule to the knowledge base.
*   **The Best of Both Worlds:** Mozart combines the flexibility and natural language power of LLMs with the precision, reliability, and structured reasoning of Symbolic AI.

Through Mozart, we aspire to construct a more intelligent, reliable, and collaborative AI music tool, transitioning the paradigm from a "stochastic jukebox" to a true "creative partner" in the composition process.

<br>

---

## 🤝 Contribution

We welcome and encourage contributions to this project! If you have any ideas, suggestions, or discover bugs, please feel free to submit a Pull Request or create an Issue.

<br>
