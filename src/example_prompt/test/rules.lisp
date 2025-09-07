(in-package :cl-user)

(setf *rules*
  '(
    ;; =================================================================
    ;; === 1. 事件调度器 (Event Dispatcher / Daemon) ===
    ;; =================================================================
    (rule event-dispatcher
      :if ((measure ?m)
           (beat ?b)
           (script (?event-m ?event-b . ?actions) . ?rest-of-script))
      :then ((when (and (= ?m ?event-m) (= ?b ?event-b))
               (dolist (action ?actions)
                 (add-fact action))
               (remove-fact (cons 'script (cons (list ?event-m ?event-b) ?rest-of-script))) ; 错误：无法移除部分列表
               (add-fact (cons 'script ?rest-of-script)))) ; 正确：用剩余的剧本替换旧的
      :weight 1000.0) ; 赋予绝对最高优先级，确保它在每个时间点都最先检查

    ;; =================================================================
    ;; === 2. 通用音乐能力 (Generic Musical Abilities) ===
    ;; =================================================================
    ;; 时间推进
    (rule advance-beat-1-to-2 :if ((beat 1)) :then ((add-fact (beat 2)) (remove-fact (beat 1))) :weight 1.0)
    (rule advance-beat-2-to-3 :if ((beat 2)) :then ((add-fact (beat 3)) (remove-fact (beat 2))) :weight 1.0)
    (rule advance-beat-3-to-4 :if ((beat 3)) :then ((add-fact (beat 4)) (remove-fact (beat 3))) :weight 1.0)
    (rule advance-measure
      :if ((beat 4) (measure ?m))
      :then ((add-fact (list 'measure (+ ?m 1)))
             (remove-fact (list 'measure ?m))
             (add-fact (beat 1))
             (remove-fact (beat 4)))
      :weight 50.0)

    ;; 响应指令的能力
    (rule apply-chord-change
      :if ((change-chord-to ?new-chord) (current-chord ?old-chord))
      :then ((add-fact (list 'current-chord ?new-chord))
             (remove-fact (list 'current-chord ?old-chord))
             (remove-fact (list 'change-chord-to ?new-chord)))
      :weight 200.0)

    (rule apply-phase-change
      :if ((set-phase ?new-phase) (phase ?old-phase))
      :then ((add-fact (list 'phase ?new-phase))
             (remove-fact (list 'phase ?old-phase))
             (remove-fact (list 'set-phase ?new-phase)))
      :weight 200.0)

    (rule apply-finish
      :if ((finish-composition))
      :then ((add-fact (composition-finished)))
      :weight 200.0)

    ;; =================================================================
    ;; === 3. 具体旋律与内容 (Specific Musical Content) ===
    ;; =================================================================
    ;; 这些规则现在只关心当前的音乐上下文，不关心宏观结构
    
    ;; 呈示部主题
    (rule melody-theme-A
      :if ((phase exposition) (measure < 5) (beat 1) (current-chord C))
      :then ((add-fact (generated-note (+ (get-fact-value 'measure) 0.1) C4 eighth))
             (add-fact (generated-note (+ (get-fact-value 'measure) 0.15) E4 eighth))
             (remove-fact (beat 1)) (add-fact (beat 2)))
      :weight 10.0)
    
    ;; 发展部动机
    (rule melody-dev-motif
      :if ((phase development) (beat 1) (current-chord Am))
      :then ((add-fact (generated-note (+ (get-fact-value 'measure) 0.1) A4 eighth))
             (add-fact (generated-note (+ (get-fact-value 'measure) 0.15) C5 eighth))
             (remove-fact (beat 1)) (add-fact (beat 2)))
      :weight 10.0)
      
    ;; 再现部主题
    (rule melody-recap-theme-A
      :if ((phase recapitulation) (measure > 40) (measure < 45) (beat 1) (current-chord C))
      :then ((add-fact (generated-note (+ (get-fact-value 'measure) 0.1) C5 eighth)) ; 高八度
             (add-fact (generated-note (+ (get-fact-value 'measure) 0.15) E5 eighth))
             (remove-fact (beat 1)) (add-fact (beat 2)))
      :weight 10.0)

    ;; 尾声
    (rule melody-coda-final
      :if ((phase coda) (measure 80) (beat 1) (current-chord C))
      :then ((add-fact (generated-note 80.1 C3 whole))
             (remove-fact (beat 1)) (add-fact (beat 4)))
      :weight 10.0)

    ;; 通用填充规则
    (rule filler-generic-rhythm
      :if ((beat 1))
      :then ((add-fact (generated-note (+ (get-fact-value 'measure) 0.1) G3 quarter))
             (remove-fact (beat 1)) (add-fact (beat 2)))
      :weight 0.5)
  ))
