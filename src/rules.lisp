;; 默认使用的专家系统规则
;; 结合了三种扩展方案：
;; 1. 结构扩展: 音乐长度为8小节。
;; 2. 状态管理: 引入 (section A) 和 (section B) 事实来控制不同段落的逻辑。
;; 3. 概率选择: 为某些情境提供了多个规则，通过权重引入变化。

(in-package :cl-user)

(setf *rules*
  '(
    ;; --- 0. 初始化 ---
    ;; 规则 1: 接收 (start-generation) 事实，初始化整个音乐状态，并进入A段
    (rule initialize-composition
      :if ((start-generation))
      :then ((add-fact (key C-major))
             (add-fact (section A))         ; 方案2: 进入A段
             (add-fact (measure 1))
             (add-fact (beat 1))
             (add-fact (current-chord C))
             (remove-fact (start-generation)))
      :weight 1.0)

    ;; --- 1. 时间与结构管理 ---
    ;; 备用规则：如果某拍没有旋律规则，这些规则确保时间仍然前进
    (rule advance-beat-1-to-2 :if ((beat 1)) :then ((add-fact (beat 2)) (remove-fact (beat 1))) :weight 1.0)
    (rule advance-beat-2-to-3 :if ((beat 2)) :then ((add-fact (beat 3)) (remove-fact (beat 2))) :weight 1.0)
    (rule advance-beat-3-to-4 :if ((beat 3)) :then ((add-fact (beat 4)) (remove-fact (beat 3))) :weight 1.0)
    
    (rule advance-measure
      :if ((beat 4))
      :then ((add-fact (beat 1))
             (add-fact (new-measure))
             (remove-fact (beat 4)))
      :weight 1.0)
    
    ;; 小节号递增规则
    (rule increment-measure-1-to-2 :if ((measure 1) (new-measure)) :then ((add-fact (measure 2)) (remove-fact (measure 1)) (remove-fact (new-measure))) :weight 100.0)
    (rule increment-measure-2-to-3 :if ((measure 2) (new-measure)) :then ((add-fact (measure 3)) (remove-fact (measure 2)) (remove-fact (new-measure))) :weight 100.0)
    (rule increment-measure-3-to-4 :if ((measure 3) (new-measure)) :then ((add-fact (measure 4)) (remove-fact (measure 3)) (remove-fact (new-measure))) :weight 100.0)
    
    ;; 方案2: 段落转换规则
    ;; 在A段第4小节结束后，切换到B段，并重置小节号
    (rule switch-A-to-B
      :if ((section A) (measure 4) (new-measure))
      :then ((add-fact (section B))
             (remove-fact (section A))
             (remove-fact (measure 4))
             (add-fact (measure 1)) ; 重置小节号为1，开始B段
             (remove-fact (new-measure)))
      :weight 200.0) ; 赋予最高权重，确保段落转换优先于一切

    ;; --- 2. 和声进行 (根据段落) ---
    ;; A段和声进行 (I-V-vi-IV)
    (rule progression-A-I-V   :if ((section A) (measure 2) (new-measure)) :then ((add-fact (current-chord G))  (remove-fact (current-chord C)))  :weight 100.0)
    (rule progression-A-V-vi  :if ((section A) (measure 3) (new-measure)) :then ((add-fact (current-chord Am)) (remove-fact (current-chord G)))  :weight 100.0)
    (rule progression-A-vi-IV :if ((section A) (measure 4) (new-measure)) :then ((add-fact (current-chord F))  (remove-fact (current-chord Am))) :weight 100.0)
    
    ;; B段和声进行 (ii-V-I-I)
    (rule progression-B-IV-ii :if ((section B) (measure 1) (new-measure)) :then ((add-fact (current-chord Dm)) (remove-fact (current-chord F)))  :weight 100.0)
    (rule progression-B-ii-V  :if ((section B) (measure 2) (new-measure)) :then ((add-fact (current-chord G))  (remove-fact (current-chord Dm))) :weight 100.0)
    (rule progression-B-V-I   :if ((section B) (measure 3) (new-measure)) :then ((add-fact (current-chord C))  (remove-fact (current-chord G)))  :weight 100.0)
    ;; B段第4小节保持C和弦，走向终止

    ;; --- 3. 旋律生成 (根据段落) ---
    ;; === A段旋律 ===
    ;; A段 - C 和弦
    (rule melody-A-C-arpeggio
      :if ((section A) (measure 1) (beat 1) (current-chord C))
      :then ((add-fact (generated-note 1.1 C4 eighth))
             (add-fact (generated-note 1.15 E4 eighth))
             (remove-fact (beat 1)) (add-fact (beat 2)))
      :weight 12.0)
    
    ;; 方案3: 概率选择 - 为A段C和弦的第1拍提供一个变奏
    (rule melody-A-C-variation
      :if ((section A) (measure 1) (beat 1) (current-chord C))
      :then ((add-fact (generated-note 1.1 G3 quarter))
             (remove-fact (beat 1)) (add-fact (beat 2)))
      :weight 5.0) ; 较低权重，使其成为不常见的变奏

    (rule melody-A-C-high-note
      :if ((section A) (measure 1) (beat 3) (current-chord C))
      :then ((add-fact (generated-note 1.3 G4 quarter))
             (remove-fact (beat 3)) (add-fact (beat 4)))
      :weight 10.0)

    ;; A段 - G 和弦
    (rule melody-A-G-rhythm
      :if ((section A) (measure 2) (beat 1) (current-chord G))
      :then ((add-fact (generated-note 2.1 G4 eighth))
             (add-fact (generated-note 2.15 G4 eighth))
             (remove-fact (beat 1)) (add-fact (beat 2)))
      :weight 10.0)

    ;; A段 - Am 和弦
    (rule melody-A-Am-upward
      :if ((section A) (measure 3) (beat 1) (current-chord Am))
      :then ((add-fact (generated-note 3.1 A3 quarter))
             (add-fact (generated-note 3.3 E4 quarter))
             (remove-fact (beat 1)) (add-fact (beat 3)))
      :weight 10.0)

    ;; A段 - F 和弦
    (rule melody-A-F-bridge
      :if ((section A) (measure 4) (beat 1) (current-chord F))
      :then ((add-fact (generated-note 4.1 F3 quarter))
             (add-fact (generated-note 4.3 C4 quarter))
             (remove-fact (beat 1)) (add-fact (beat 3)))
      :weight 10.0)

    ;; === B段旋律 (小节号从1开始) ===
    ;; B段 - Dm 和弦
    (rule melody-B-Dm-descend
      :if ((section B) (measure 1) (beat 1) (current-chord Dm))
      :then ((add-fact (generated-note 5.1 F4 quarter))
             (add-fact (generated-note 5.3 D4 quarter))
             (remove-fact (beat 1)) (add-fact (beat 3)))
      :weight 10.0)

    ;; B段 - G 和弦
    (rule melody-B-G-resolve-prep
      :if ((section B) (measure 2) (beat 1) (current-chord G))
      :then ((add-fact (generated-note 6.1 B3 quarter))
             (add-fact (generated-note 6.3 G4 quarter))
             (remove-fact (beat 1)) (add-fact (beat 3)))
      :weight 10.0)

    ;; B段 - C 和弦 (收束)
    (rule melody-B-C-cadence
      :if ((section B) (measure 3) (beat 1) (current-chord C))
      :then ((add-fact (generated-note 7.1 E4 half))
             (remove-fact (beat 1)) (add-fact (beat 3)))
      :weight 10.0)

    (rule melody-B-C-final
      :if ((section B) (measure 4) (beat 1) (current-chord C))
      :then ((add-fact (generated-note 8.1 C3 whole))
             (remove-fact (beat 1)) (add-fact (beat 4)))
      :weight 10.0)

    ;; --- 4. 终止 ---
    ;; 在B段第4小节的末尾结束整个乐曲
    (rule end-composition
      :if ((section B) (measure 4) (beat 4))
      :then ((add-fact (composition-finished)))
      :weight 1.0)
  ))
