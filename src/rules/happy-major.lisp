(in-package :cl-user)

(defparameter *rules*
  '(
    ;; --- 1. 初始化和时间管理 ---
    (rule initialize-happy
      :if ((start-generation))
      :then ((add-fact (key C-major))
             (add-fact (measure 1))
             (add-fact (beat 1))
             (add-fact (current-chord C))
             (remove-fact (start-generation)))
      :weight 1.0)

    (rule advance-beat-1-to-2 :if ((beat 1)) :then ((add-fact (beat 2)) (remove-fact (beat 1))) :weight 1.0)
    (rule advance-beat-2-to-3 :if ((beat 2)) :then ((add-fact (beat 3)) (remove-fact (beat 2))) :weight 1.0)
    (rule advance-beat-3-to-4 :if ((beat 3)) :then ((add-fact (beat 4)) (remove-fact (beat 3))) :weight 1.0)
    (rule advance-measure     :if ((beat 4)) :then ((add-fact (beat 1)) (add-fact (new-measure)) (remove-fact (beat 4))) :weight 1.0)
    (rule increment-measure-1-to-2 :if ((measure 1) (new-measure)) :then ((add-fact (measure 2)) (remove-fact (measure 1)) (remove-fact (new-measure))) :weight 1.0)
    (rule increment-measure-2-to-3 :if ((measure 2) (new-measure)) :then ((add-fact (measure 3)) (remove-fact (measure 2)) (remove-fact (new-measure))) :weight 1.0)
    (rule increment-measure-3-to-4 :if ((measure 3) (new-measure)) :then ((add-fact (measure 4)) (remove-fact (measure 3)) (remove-fact (new-measure))) :weight 1.0)

    ;; --- 2. 和声进行 ---
    (rule progression-I-V   :if ((measure 1) (new-measure)) :then ((add-fact (current-chord G))  (remove-fact (current-chord C)))  :weight 1.0)
    (rule progression-V-vi  :if ((measure 2) (new-measure)) :then ((add-fact (current-chord Am)) (remove-fact (current-chord G)))  :weight 1.0)
    (rule progression-vi-IV :if ((measure 3) (new-measure)) :then ((add-fact (current-chord F))  (remove-fact (current-chord Am))) :weight 1.0)

    ;; --- 3. 旋律生成 ---
    ;; C 和弦
    (rule happy-C-arpeggio
      :if ((measure 1) (beat 1) (current-chord C))
      :then ((add-fact (generated-note 1.1 C4 eighth))
             (add-fact (generated-note 1.15 E4 eighth)))
      :weight 12.0)
    
    (rule happy-C-high-note
      :if ((measure 1) (beat 3) (current-chord C))
      :then ((add-fact (generated-note 1.3 G4 quarter)))
      :weight 10.0)

    ;; G 和弦
    (rule happy-G-rhythm
      :if ((measure 2) (beat 1) (current-chord G))
      :then ((add-fact (generated-note 2.1 G4 eighth))
             (add-fact (generated-note 2.15 G4 eighth)))
      :weight 10.0)

    ;; Am 和弦
    (rule happy-Am-upward
      :if ((measure 3) (beat 1) (current-chord Am))
      :then ((add-fact (generated-note 3.1 A3 quarter))
             (add-fact (generated-note 3.3 E4 quarter)))
      :weight 10.0)

    ;; F 和弦
    (rule happy-F-ending
      :if ((measure 4) (beat 1) (current-chord F))
      :then ((add-fact (generated-note 4.1 F3 quarter))
             (add-fact (generated-note 4.3 C4 half))) ; 结束在属音上，有开放感
      :weight 10.0)

    ;; --- 4. 终止 ---
    (rule end-composition
      :if ((measure 4) (new-measure))
      :then ((add-fact (composition-finished)))
      :weight 1.0)
  ))
