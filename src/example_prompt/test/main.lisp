(load "mozart.lisp")

(defparameter *sonata-script*
  '(
    ;; (measure beat (action1) (action2) ...)
    (1 1 (set-phase exposition) (set-chord C))
    (4 4 (change-chord-to G))
    (8 4 (change-chord-to F))
    (12 4 (change-chord-to C))
    (16 4 (set-phase development) (change-chord-to Am))
    (24 4 (change-chord-to Dm))
    (32 4 (change-chord-to G7))
    (40 4 (set-phase recapitulation) (change-chord-to C))
    (48 4 (change-chord-to G))
    (56 4 (change-chord-to F))
    (64 4 (set-phase coda) (change-chord-to C))
    (72 4 (change-chord-to G7))
    (80 1 (set-chord C)) ; 确保最后一个和弦
    (80 4 (finish-composition))
  ))

(defun main ()
  (reset-wm)
  (load "rules.lisp")

  ;; 将整个剧本作为一个事实注入
  (add-fact (cons 'script *sonata-script*))
  
  ;; 基础时间事实
  (add-fact '(measure 1))
  (add-fact '(beat 1))
  
  ;; 初始音乐上下文
  (add-fact '(current-chord C))
  (add-fact '(phase exposition))

  ;; 运行 Mozart
  (run-engine 1000 0.2)
)

;; 执行主函数
(main)
