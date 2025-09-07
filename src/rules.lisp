;; 默认使用的专家系统规则
;; 结合了三种扩展方案：
;; 1. 结构扩展: 音乐长度为8小节。
;; 2. 状态管理: 引入 (section A) 和 (section B) 事实来控制不同段落的逻辑。
;; 3. 概率选择: 为某些情境提供了多个规则，通过权重引入变化。

(in-package :cl-user)

(setf *rules*
  '(
    ;; --- 0. 初始化 ---
    (rule initialize-composition
      :if ((start-generation))
      :then ((add-fact (key C-major))
             (add-fact (section A))
             (add-fact (measure 1))
             (add-fact (beat 1))
             (add-fact (current-chord C))
             (remove-fact (start-generation)))
      :weight 1.0)

    ;; --- 1. 时间管理 (仅保留节拍推进) ---
    (rule advance-beat-1-to-2 :if ((beat 1)) :then ((add-fact (beat 2)) (remove-fact (beat 1))) :weight 1.0)
    (rule advance-beat-2-to-3 :if ((beat 2)) :then ((add-fact (beat 3)) (remove-fact (beat 2))) :weight 1.0)
    (rule advance-beat-3-to-4 :if ((beat 3)) :then ((add-fact (beat 4)) (remove-fact (beat 3))) :weight 1.0)

    ;; --- 2. 场景转换规则 ---
    ;; 这些规则原子化地处理一个小节的结束和下一个小节的开始
    
    (rule transition-measure-1-to-2-in-A
      :if ((section A) (measure 1) (beat 4))
      :then ((add-fact (measure 2)) (remove-fact (measure 1))
             (add-fact (current-chord G)) (remove-fact (current-chord C))
             (add-fact (beat 1)) (remove-fact (beat 4)))
      :weight 100.0)

    (rule transition-measure-2-to-3-in-A
      :if ((section A) (measure 2) (beat 4))
      :then ((add-fact (measure 3)) (remove-fact (measure 2))
             (add-fact (current-chord Am)) (remove-fact (current-chord G))
             (add-fact (beat 1)) (remove-fact (beat 4)))
      :weight 100.0)

    (rule transition-measure-3-to-4-in-A
      :if ((section A) (measure 3) (beat 4))
      :then ((add-fact (measure 4)) (remove-fact (measure 3))
             (add-fact (current-chord F)) (remove-fact (current-chord Am))
             (add-fact (beat 1)) (remove-fact (beat 4)))
      :weight 100.0)

    ;; 段落转换：从A段第4小节结束时，进入B段第1小节
    (rule transition-A-to-B
      :if ((section A) (measure 4) (beat 4))
      :then ((add-fact (section B)) (remove-fact (section A))
             (add-fact (measure 1)) (remove-fact (measure 4)) ; 重置小节号
             (add-fact (current-chord Dm)) (remove-fact (current-chord F))
             (add-fact (beat 1)) (remove-fact (beat 4)))
      :weight 100.0)

    (rule transition-measure-1-to-2-in-B
      :if ((section B) (measure 1) (beat 4))
      :then ((add-fact (measure 2)) (remove-fact (measure 1))
             (add-fact (current-chord G)) (remove-fact (current-chord Dm))
             (add-fact (beat 1)) (remove-fact (beat 4)))
      :weight 100.0)
      
    (rule transition-measure-2-to-3-in-B
      :if ((section B) (measure 2) (beat 4))
      :then ((add-fact (measure 3)) (remove-fact (measure 2))
             (add-fact (current-chord C)) (remove-fact (current-chord G))
             (add-fact (beat 1)) (remove-fact (beat 4)))
      :weight 100.0)

    (rule transition-measure-3-to-4-in-B
      :if ((section B) (measure 3) (beat 4))
      :then ((add-fact (measure 4)) (remove-fact (measure 3))
             ; 和弦保持C不变
             (add-fact (beat 1)) (remove-fact (beat 4)))
      :weight 100.0)
      
    ;; --- 3. 旋律生成 ---
    (rule melody-A-C-arpeggio :if ((section A) (measure 1) (beat 1) (current-chord C)) :then ((add-fact (generated-note 1.1 C4 eighth)) (add-fact (generated-note 1.15 E4 eighth)) (remove-fact (beat 1)) (add-fact (beat 2))) :weight 12.0)
    (rule melody-A-C-variation :if ((section A) (measure 1) (beat 1) (current-chord C)) :then ((add-fact (generated-note 1.1 G3 quarter)) (remove-fact (beat 1)) (add-fact (beat 2))) :weight 5.0)
    (rule melody-A-C-high-note :if ((section A) (measure 1) (beat 3) (current-chord C)) :then ((add-fact (generated-note 1.3 G4 quarter)) (remove-fact (beat 3)) (add-fact (beat 4))) :weight 10.0)
    (rule melody-A-G-rhythm :if ((section A) (measure 2) (beat 1) (current-chord G)) :then ((add-fact (generated-note 2.1 G4 eighth)) (add-fact (generated-note 2.15 G4 eighth)) (remove-fact (beat 1)) (add-fact (beat 2))) :weight 10.0)
    (rule melody-A-Am-upward :if ((section A) (measure 3) (beat 1) (current-chord Am)) :then ((add-fact (generated-note 3.1 A3 quarter)) (add-fact (generated-note 3.3 E4 quarter)) (remove-fact (beat 1)) (add-fact (beat 3))) :weight 10.0)
    (rule melody-A-F-bridge :if ((section A) (measure 4) (beat 1) (current-chord F)) :then ((add-fact (generated-note 4.1 F3 quarter)) (add-fact (generated-note 4.3 C4 quarter)) (remove-fact (beat 1)) (add-fact (beat 3))) :weight 10.0)
    (rule melody-B-Dm-descend :if ((section B) (measure 1) (beat 1) (current-chord Dm)) :then ((add-fact (generated-note 5.1 F4 quarter)) (add-fact (generated-note 5.3 D4 quarter)) (remove-fact (beat 1)) (add-fact (beat 3))) :weight 10.0)
    (rule melody-B-G-resolve-prep :if ((section B) (measure 2) (beat 1) (current-chord G)) :then ((add-fact (generated-note 6.1 B3 quarter)) (add-fact (generated-note 6.3 G4 quarter)) (remove-fact (beat 1)) (add-fact (beat 3))) :weight 10.0)
    (rule melody-B-C-cadence :if ((section B) (measure 3) (beat 1) (current-chord C)) :then ((add-fact (generated-note 7.1 E4 half)) (remove-fact (beat 1)) (add-fact (beat 3))) :weight 10.0)
    (rule melody-B-C-final :if ((section B) (measure 4) (beat 1) (current-chord C)) :then ((add-fact (generated-note 8.1 C3 whole)) (remove-fact (beat 1)) (add-fact (beat 4))) :weight 10.0)

    ;; --- 4. 终止 ---
    (rule end-composition
      :if ((section B) (measure 4) (beat 4))
      :then ((add-fact (composition-finished)))
      :weight 101.0) ; 确保它在最后的小节转换中拥有最高优先权
  ))
