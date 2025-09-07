(in-package :cl-user)

(defmacro generate-rules-from-script (script)
  "宏：根据剧本自动生成所有场景转换规则"
  `(progn
     ,@(loop for event in script
             collect (let* ((measure (first event))
                            (beat (second event))
                            (actions (cddr event))
                            (rule-name (intern (format nil "SCRIPT-EVENT-M~A-B~A" measure beat))))
                       `(rule ,rule-name
                          :if ((measure ,measure) (beat ,beat))
                          :then (,@(loop for action in actions collect
                                         (case (car action)
                                           ('set-phase `(progn (remove-fact (phase ?p)) (add-fact '(phase ,(second action)))))
                                           ('change-chord-to `(progn (remove-fact (current-chord ?c)) (add-fact '(current-chord ,(second action)))))
                                           ('finish-composition `(add-fact '(composition-finished)))
                                           (t action))))
                          :weight 100.0)))))

;; =================================================================
;; === 1. 时间推进模块 ===
;; =================================================================
(rule advance-beat
  :if ((beat ?b) (< ?b 4))
  :then ((remove-fact (beat ?b))
         (add-fact (beat (+ ?b 1))))
  :weight 1.0)

(rule advance-measure
  :if ((beat 4) (measure ?m))
  :then ((remove-fact (beat 4)) (add-fact (beat 1))
         (remove-fact (measure ?m)) (add-fact (measure (+ ?m 1))))
  :weight 1.0)

;; =================================================================
;; === 2. 旋律生成模块 ===
;; =================================================================
;; --- 主题与动机 ---
(rule melody-theme-A
  :if ((phase exposition) (measure ?m) (< ?m 5) (beat 1) (current-chord C))
  :then ((add-fact (generated-note (+ ?m 0.1) C4 eighth))
         (add-fact (generated-note (+ ?m 0.15) E4 eighth))
         (remove-fact (beat 1)) (add-fact (beat 2)))
  :weight 10.0)

(rule melody-theme-B
  :if ((phase exposition) (measure ?m) (>= ?m 9) (< ?m 13) (beat 1) (current-chord F))
  :then ((add-fact (generated-note (+ ?m 0.1) A4 half))
         (remove-fact (beat 1)) (add-fact (beat 3)))
  :weight 10.0)

(rule melody-dev-motif
  :if ((phase development) (measure ?m) (current-chord ?c) (beat 1))
  :then ((add-fact (generated-note (+ ?m 0.1) (random-note-from-chord ?c 'am-scale))) ; 动态生成
         (remove-fact (beat 1)) (add-fact (beat 2)))
  :weight 8.0)

(rule melody-recap-theme-A
  :if ((phase recapitulation) (measure ?m) (< ?m 45) (beat 1) (current-chord C))
  :then ((add-fact (generated-note (+ ?m 0.1) C5 eighth)) ; 高八度
         (add-fact (generated-note (+ ?m 0.15) E5 eighth))
         (remove-fact (beat 1)) (add-fact (beat 2)))
  :weight 10.0)

(rule melody-coda-final
  :if ((phase coda) (measure 80) (beat 1) (current-chord C))
  :then ((add-fact (generated-note 80.1 C3 whole)))
  :weight 10.0)

;; --- 通用填充 ---
(rule filler-arpeggio
  :if ((beat 1))
  :then ((add-fact (generated-note (+ (get-fact-value 'measure) 0.1) C4 eighth))
         (remove-fact (beat 1)) (add-fact (beat 2)))
  :weight 0.5)

(rule filler-rhythm
  :if ((beat 3))
  :then ((add-fact (generated-note (+ (get-fact-value 'measure) 0.3) G3 quarter))
         (remove-fact (beat 3)) (add-fact (beat 4)))
  :weight 0.5)
