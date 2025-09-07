## Mozart 生成系统：提示工程指南

### **前言**

本指南旨在为 Mozart 符号主义专家系统的引导工程师（Prompt Engineer）提供一份详尽的理论与实践手册。Mozart 是一个基于符号逻辑与前向链推理的生成式音乐模型。对其进行引导，本质上是对其认知过程进行编程。本指南旨在解锁系统的全部潜力，实现从“音乐生成”到“音乐思想涌现”的飞跃。

本指南将围绕三大核心理论展开：

1.  **第一章：引导的粒度与层次**
2.  **第二章：动态引导模式**
3.  **第三章：元引导与系统自适应**

---

### **第一章：引导的粒度与层次**

引导并非单一维度的操作，而是存在于不同抽象层次的复杂行为。理解并运用这些层次是进行精细化创作的基础。

#### **1.1 引导粒度谱系**

我们将引导的粒度定义为一次引导操作所影响的音乐时空范围。

*   **微观引导**: 影响单个音符或瞬时和声。
    *   **概念**: 这是对音乐原子的操作。
    *   **技巧**: **事实注入**。在推理中途（例如，通过外部事件循环）向工作内存动态添加临时事实。
    *   **示例**: 强制在第三小节第二拍出现一个特定的经过音 `F#`。
        ```lisp
        ;; 外部控制逻辑 (非规则)
        (when (and (fact-exists-p '(measure 3))
                   (fact-exists-p '(beat 2)))
          (add-fact '(force-note F#4)))
        ```
        ```lisp
        ;; rules.lisp
        (rule play-forced-note
          :if ((force-note ?pitch))
          :then ((add-fact (generated-note ... ?pitch))
                 (remove-fact (force-note ?pitch)))
          :weight 999.0) ; 绝对优先
        ```
*   **介观引导 (Meso-Guidance)**: 影响一个乐句、一个小节或一个动机。
    *   **概念**: 这是对音乐“分子”（即动机、模式）的操作。
    *   **技巧**: **模式规则**。定义一个包含多个时间步骤的“宏规则”。
    *   **示例**: 定义一个“曼海姆火箭”音型（Mannheim Rocket），这是一个快速上行的琶音。
        ```lisp
        (rule mannheim-rocket-start
          :if ((section A) (measure 1) (beat 1) (current-chord C))
          :then ((add-fact (execute-pattern mannheim-rocket-c-major)) ; 触发模式
                 (remove-fact (beat 1))))
        
        (rule mannheim-rocket-step1 :if ((execute-pattern mannheim-rocket-c-major)) :then ((add-fact (generated-note ... C4)) (add-fact (pattern-step 1))) :weight 100)
        (rule mannheim-rocket-step2 :if ((pattern-step 1)) :then ((add-fact (generated-note ... E4)) (add-fact (pattern-step 2))) :weight 100)
        (rule mannheim-rocket-step3 :if ((pattern-step 2)) :then ((add-fact (generated-note ... G4)) (add-fact (pattern-step 3))) :weight 100)
        (rule mannheim-rocket-step4 :if ((pattern-step 3)) :then ((add-fact (generated-note ... C5)) (remove-fact (execute-pattern mannheim-rocket-c-major)) (add-fact (beat 2))) :weight 100)
        ```
*   **宏观引导 (Macro-Guidance)**: 影响整个段落、曲式结构或风格。
    *   **概念**: 这是对音乐“有机体”（即曲式、发展）的操作。
    *   **技巧**: **结构事实 (Structural Facts)** 和 **阶段控制器 (Phase Controllers)**。
    *   **示例**: 实现一个奏鸣曲式（Sonata form）。
        ```lisp
        ;; 初始事实
        '((sonata-form) (phase exposition))

        ;; 规则
        (rule theme-A-in-tonic
          :if ((phase exposition) (in-tonic-key))
          :then (...))
        
        (rule transition-to-dominant
          :if ((phase exposition) (theme-A-finished))
          :then ((add-fact (in-dominant-key)) ...))
        
        (rule end-exposition
          :if ((phase exposition) (closing-theme-finished))
          :then ((add-fact (phase development))
                 (remove-fact (phase exposition)) ...))
        ```

---

### **第二章：动态引导模式 (Dynamic Guidance Patterns)**

本章介绍一系列高级设计模式，它们使引导不再是静态的预设，而是能随音乐发展而动态演变的交互过程。

#### **2.1 情绪弧光 (Affect Arc)**

**概念**: 模拟音乐中情绪的建立、高潮和缓和的过程。我们通过一个数值化的事实 `(tension ?value)` 来量化音乐的紧张度，并让规则对该值做出反应。

**设计模式**:
1.  **张力累积规则**: 某些音乐事件（如使用不协和和弦、音区升高、节奏加密）会增加 `tension` 值。
2.  **张力释放规则**: 其他事件（如解决到主和弦、音区下降、节奏放缓）会降低 `tension` 值。
3.  **情境感知规则**: 旋律和节奏规则的 `:if` 条件会检查当前的 `tension` 值，从而在不同紧张度下表现出不同的行为。

**示例**:
```lisp
;; 张力累积
(rule use-diminished-chord
  :if ((tension < 80) ...) ; 避免过度紧张
  :then ((add-fact (current-chord Gdim7))
         (increment-fact tension 20))) ; 增加20点张力

;; 张力释放
(rule resolve-to-tonic
  :if ((tension > 50) (current-chord G7))
  :then ((add-fact (current-chord C))
         (decrement-fact tension 30))) ; 释放30点张力

;; 情境感知旋律
(rule melody-high-tension
  :if ((tension > 70) (current-chord Gdim7))
  :then ((add-fact (generated-note ... F#5 sixteenth)) ...) ; 高音区、短时值音符
  :weight 15.0)

(rule melody-low-tension
  :if ((tension < 30) (current-chord C))
  :then ((add-fact (generated-note ... E4 half)) ...) ; 中音区、长时值音符
  :weight 10.0)

;; (increment-fact 和 decrement-fact 是需要自行实现的辅助函数)
```

#### **2.2 动机发展引擎**

**概念**: 模拟作曲家围绕一个核心动机（Motif）进行变奏、倒影、逆行、增值、减值等操作。

**设计模式**:
1.  **动机定义**: 将一个核心动机存储为事实，例如 `(motif "alpha" (notes (C4 D4 E4)))`。
2.  **转换算子规则 (Transformation Operator Rules)**: 编写一系列规则，每个规则代表一种音乐发展手法。
3.  **发展控制器 (Development Controller)**: 一个高层规则，决定在何时、对哪个动机、应用哪种转换算子。

**示例**:
```lisp
;; 初始动机
'((motif "alpha" (notes (C D E))) (current-beat 1))

;; 转换算子：倒影 (Inversion)
(rule apply-inversion
  :if ((develop-motif "alpha" using inversion)
       (motif "alpha" (notes ?note-list)))
  :then ((let ((inverted-notes (calculate-inversion ?note-list 'C))) ; 以C为轴进行倒影
           (add-fact (play-notes-sequentially inverted-notes)))
         (remove-fact (develop-motif "alpha" using inversion))))

;; 转换算子：增值 (Augmentation)
(rule apply-augmentation
  :if ((develop-motif "alpha" using augmentation)
       (motif "alpha" (notes ?note-list)))
  :then ((play-notes-sequentially ?note-list :duration-multiplier 2)) ...)

;; 发展控制器
(rule development-phase-logic
  :if ((phase development) (measure 5))
  :then ((add-fact (develop-motif "alpha" using inversion))))
```
这创建了一个强大的二级推理系统：顶层决定“做什么”，底层执行“怎么做”。

---

### **第三章：元引导与系统自适应**

本章探讨前沿的引导概念，即引导系统本身的行为，使其具备一定的学习和反思能力。

#### **3.1 规则熔断与激活**

**概念**: 系统可以根据生成历史动态地启用或禁用某些规则，以避免重复或强制风格转变。

**设计模式**:
1.  **规则元数据**: 为规则添加元数据事实，如 `(rule-metadata melody-A-C-arpeggio (style "classical") (usage-count 0))`。
2.  **使用计数器**: 每当一个规则被触发，其 `usage-count` 增加。
3.  **熔断器规则 (Circuit Breaker Rule)**: 当一个规则的 `usage-count` 超过阈值，一个高优先级规则会添加一个 `(rule-disabled melody-A-C-arpeggio)` 事实。
4.  **规则前提检查**: 所有规则的 `:if` 条件前都隐式或显式地增加一条 `(not (rule-disabled <self-name>))`。

**示例**:
```lisp
;; 触发后更新计数
(rule melody-A-C-arpeggio
  :if ((not (rule-disabled melody-A-C-arpeggio))
       (section A) (measure 1) ...)
  :then (...
         (increment-fact (rule-metadata melody-A-C-arpeggio usage-count))))

;; 熔断器
(rule fuse-overused-rule
  :if ((rule-metadata ?rule-name (usage-count > 5)))
  :then ((add-fact (rule-disabled ?rule-name))))
```
这个模式能有效防止“创意枯竭”，强迫系统去探索知识库中其他未被充分利用的部分。

#### **3.2 自我修正的回溯提示**

**概念**: 这是一个模拟人类作曲家“写下一段旋律，觉得不好，划掉重写”的过程。系统在生成不理想的片段后，能够“撤销”并尝试其他路径。

**设计模式**:
1.  **评估规则 (Evaluator Rules)**: 在一个乐句生成后，一系列评估规则会检查其质量，例如 `(check-for-parallel-fifths)` 或 `(check-melodic-climax)`。
2.  **回溯提示事实 (Backtracking Hint Fact)**: 如果评估发现问题，它不会直接修改音乐，而是添加一个提示事实，如 `(backtracking-hint (reason "parallel fifths") (source-rule transition-measure-2-to-3-in-A))`。
3.  **重写控制器 (Rewrite Controller)**: 一个极高优先级的规则，当它检测到 `backtracking-hint` 时：
    *   移除最近生成的N个音符事实。
    *   将工作内存恢复到问题发生前的状态（需要日志或快照机制）。
    *   添加一个临时的 `(avoid-rule <source-rule-name>)` 事实。
    *   重新启动该时间点的推理。

**示例**:
```lisp
;; 评估器
(rule detect-parallel-fifths
  :if ((generated-note ?t1 ?p1) (generated-note ?t2 ?p2) ...)
  :then ((when (is-parallel-fifth? ...)
           (add-fact (backtracking-hint (reason "...") ...)))))

;; 重写控制器 (概念)
(rule rewrite-on-hint
  :if ((backtracking-hint ?hint))
  :then ((undo-last-phrase)
         (restore-wm-snapshot (getf ?hint :time))
         (add-fact (avoid-rule (getf ?hint :source-rule)))
         (remove-fact (backtracking-hint ?hint))))
```
