(in-package :cl-user)

(defparameter *rules*
  '(
    ;; --- 1. 初始化和时间管理 ---
    (rule initialize-sad
      :if ((start-generation))
      :then ((add-fact (key A-minor))
             (add-fact (measure 1))
             (add-fact (beat 1))
             (add-fact (current-chord Am))
             (remove-fact (start-generation)))
      :weight 1.0)

    (rule advance-beat-1-to-2 :if ((beat 1)) :then ((add-fact (beat 2)) (remove-fact (beat 1))) :weight 1.0)
    (rule advance-beat-2-to-3 :if ((beat 2)) :then ((add-fact (beat 3)) (remove-fact (beat 2))) :weight 1.0)
    (rule advance-beat-3-to-4 :if ((beat 3)) :then ((add-fact (beat 4)) (remove-fact (beat 3))) :weight 1.0)
    (rule advance-measure     :if ((beat 4)) :then ((add-fact (beat 1)) (add-fact (new-measure)) (remove-fact (beat 4))) :weight 1.0)
    (rule increment-measure-1-to-2 :if ((measure 1) (new-measure)) :then ((add-fact (measure 2)) (remove-fact (measure 1)) (remove-fact (new-measure))) :weight 1.0)
    (rule increment-measure-2-to-3 :if ((measure 2) (new-measure)) :then ((add-fact (measure 3)) (remove-fact (measure 2)) (remove-fact (new-measure))) :weight 1.0)
    (rule increment-measure-3-to-4 :if ((measure 3) (new-measure)) :then ((add-fact (measure 4)) (remove-fact (measure 3)) (remove-fact (new-measure))) :weight 1.0)

    ;; --- 2. 和声进行 (小调) ---
    (rule progression-i-iv :if ((measure 1) (new-measure)) :then ((add-fact (current-chord Dm)) (remove-fact (current-chord Am))) :weight 1.0)
    (rule progression-iv-v :if ((measure 2) (new-measure)) :then ((add-fact (current-chord E))  (remove-fact (current-chord Dm))) :weight 1.0)
    (rule progression-v-i  :if ((measure 3) (new-measure)) :then ((add-fact (current-chord Am)) (remove-fact (current-chord E)))  :weight 1.0)

    ;; --- 3. 旋律生成 (悲伤风格) ---
    ;; Am 和弦
    (rule sad-Am-long-note
      :if ((measure 1) (beat 1) (current-chord Am))
      :then ((add-fact (generated-note 1.1 A3 half)))
      :weight 12.0)
    
    (rule sad-Am-descending
      :if ((measure 1) (beat 3) (current-chord Am))
      :then ((add-fact (generated-note 1.3 E4 quarter))
             (add-fact (generated-note 1.4 D4 quarter))) ; 预备下一个Dm和弦
      :weight 8.0)

    ;; Dm 和弦
    (rule sad-Dm-root
      :if ((measure 2) (beat 1) (current-chord Dm))
      :then ((add-fact (generated-note 2.1 D3 half)))
      :weight 10.0)

    ;; E 和弦
    (rule sad-E-tension
      :if ((measure 3) (beat 1) (current-chord E))
      :then ((add-fact (generated-note 3.1 G#3 quarter))) ; 导音
      :weight 10.0)

    ;; Am 和弦
    (rule sad-Am-resolution
      :if ((measure 4) (beat 1) (current-chord Am))
      :then ((add-fact (generated-note 4.1 A2 whole))) ; 漫长的全音符结尾
      :weight 15.0)

    ;; --- 4. 终止 ---
    (rule end-composition
      :if ((measure 4) (new-measure))
      :then ((add-fact (composition-finished)))
      :weight 1.0)
  ))
