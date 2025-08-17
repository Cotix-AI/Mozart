;;; --- 1. 工作内存 ---
;;; 使用一个全局变量来存储所有 fact
(defparameter *working-memory* nil)

;;; 添加一个 fact 到工作内存，避免重复
(defun add-fact (fact)
  (unless (member fact *working-memory* :test #'equal)
    (push fact *working-memory*)))

;;; 初始化工作内存
(defun reset-wm ()
  (setf *working-memory* nil))

;;; --- 2. 规则库 ---
;;; 使用一个全局变量存储所有 rule
(defparameter *rules* nil)

;;; --- 3. 推理机 ---

;;; 检查单个条件是否在工作内存中得到满足
(defun condition-matches-p (condition)
  (member condition *working-memory* :test #'equal))

;;; 检查一个规则的所有 :if 条件是否都满足
(defun rule-conditions-met-p (rule)
  (let ((conditions (getf (cddr rule) :if)))
    (every #'condition-matches-p conditions)))

;;; 找到所有可以被触发的规则
(defun find-applicable-rules ()
  (remove-if-not #'rule-conditions-met-p *rules*))

;;; 执行一个规则的 :then 动作
;;; 目前只支持 add-fact 动作
(defun apply-rule (rule)
  (let ((actions (getf (cddr rule) :then)))
    (format t "~&[FIRE] 触发专家: ~a" (second rule))
    (dolist (action actions)
      (case (first action)
        ('add-fact (add-fact (second action)))
        ;; 这里可以扩展其他动作, 例如 'play-note', 'change-instrument' 等
        (t (format t "~&[WARN] 未知动作: ~a" action))))))

(defun run-engine (&optional (max-cycles 10))
  (loop for i from 1 to max-cycles
        do (let ((applicable-rules (find-applicable-rules)))
             (if (null applicable-rules)
                 (progn
                   (format t "~&[INFO] 没有可启动的专家了，生成结束。")
                   (return))
                 ;; 简单的冲突解决策略：选择第一个可应用的规则
                 (let ((rule-to-fire (first applicable-rules)))
                   (apply-rule rule-to-fire))))))

;;; --- 4. 主流程和输出 ---

;;; 从工作内存中提取生成的音符
(defun get-generated-notes ()
  (remove-if-not #'(lambda (fact) (eq (first fact) 'generated-note))
                 *working-memory*))

;;; 主生成函数
(defun generate-music (initial-facts rules-file &optional (cycles 20))
  (reset-wm)
  (load rules-file) 
  (format t "~&[INFO] 从 ~a 加载了 ~d 个专家。" rules-file (length *rules*))

  ;; 添加初始 fact
  (dolist (fact initial-facts)
    (add-fact fact))
  (format t "~&[INFO] 初始事实: ~a" *working-memory*)
  
  (format t "~&~%--- 开始音乐生成 ---~%")
  (run-engine cycles)
  (format t "~&--- 生成结束 ---~%~%")
  
  (let ((notes (get-generated-notes)))
    (format t "生成的音符序列: ~%")
    (dolist (note notes)
      (format t "  ~a~%" note))
    (reverse notes))) ; 反转，因为我们用 push 添加，所以是倒序的
