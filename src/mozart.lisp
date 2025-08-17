(in-package :cl-user)

;;; =================================================================
;;; 1. 工作内存 (Working Memory)
;;; =================================================================

(defparameter *working-memory* nil "存储所有当前事实的列表")

(defun add-fact (fact)
  "向工作内存中添加一个事实，除非它已经存在"
  (unless (member fact *working-memory* :test #'equal)
    (push fact *working-memory*)))

(defun remove-fact (fact)
  "从工作内存中移除一个事实"
  (setf *working-memory* (remove fact *working-memory* :test #'equal)))

(defun reset-wm ()
  "清空工作内存"
  (setf *working-memory* nil))

(defun fact-exists-p (fact)
  "检查一个fact是否存在于工作内存中"
  (member fact *working-memory* :test #'equal))


;;; =================================================================
;;; 2. 规则库 (Knowledge Base)
;;; =================================================================

(defparameter *rules* nil "存储从文件加载的所有规则")


;;; =================================================================
;;; 3. 推理机 (Inference Engine) - 包含温度采样逻辑
;;; =================================================================

;;; --- 3a. 规则匹配 ---

(defun condition-matches-p (condition)
  "检查单个条件是否满足, 支持 (not <fact>) 形式"
  (if (and (listp condition) (eq (first condition) 'not))
      (not (fact-exists-p (second condition)))
      (fact-exists-p condition)))

(defun rule-conditions-met-p (rule)
  "检查一个规则的所有 :if 条件是否都满足"
  (let ((conditions (getf (cddr rule) :if)))
    (every #'condition-matches-p conditions)))

(defun find-applicable-rules ()
  "找到当前工作内存状态下所有可以被触发的规则"
  (remove-if-not #'rule-conditions-met-p *rules*))

;;; --- 3b. Softmax 和带权采样辅助函数 ---

(defun softmax (scores &optional (temperature 1.0))
  "将一组分数通过 Softmax 函数转换为概率分布。
  温度 T 控制随机性：T->0 更确定，T->inf 更随机"
  (when (or (null scores) (<= temperature 0.01))
    (let ((max-score (reduce #'max scores))
          (result (make-list (length scores) :initial-element 0.0)))
      (setf (nth (position max-score scores) result) 1.0)
      (return-from softmax result)))

  (let* ((scaled-scores (mapcar #'(lambda (s) (/ (float s) temperature)) scores))
         (max-score (reduce #'max scaled-scores))
         (exps (mapcar #'(lambda (s) (exp (- s max-score))) scaled-scores))
         (sum-exps (reduce #'+ exps)))
    (if (zerop sum-exps)
        (make-list (length scores) :initial-element (/ 1.0 (length scores)))
        (mapcar #'(lambda (e) (/ e sum-exps)) exps))))

(defun sample-from-distribution (items probabilities)
  "根据给定的概率分布从 items 中采样一个元素"
  (let ((rand (random 1.0))
        (cumulative 0.0))
    (loop for item in items
          for prob in probabilities
          do (incf cumulative prob)
          when (> cumulative rand)
          return item
          finally (return (car (last items))))))

;;; --- 3c. 规则执行与主循环 ---

(defun apply-rule (rule)
  "执行一个规则的 :then 部分的动作。"
  (let ((actions (getf (cddr rule) :then)))
    (format t "~&[FIRE] 触发规则: ~a" (second rule))
    (dolist (action actions)
      (case (first action)
        ('add-fact (add-fact (second action)))
        ('remove-fact (remove-fact (second action)))
        (t (format t "~&[WARN] 未知动作: ~a" action))))))

(defun run-engine (&optional (max-cycles 100) (temperature 1.0))
  "推理机主循环，使用温度采样选择下一个要触发的规则。"
  (loop for i from 1 to max-cycles
        when (fact-exists-p '(composition-finished))
        do 
          (format t "~&[INFO] 发现 (composition-finished) 事实，推理结束。")
          (return)
        do (let ((applicable-rules (find-applicable-rules)))
             (if (null applicable-rules)
                 (progn
                   (format t "~&[INFO] 没有可应用的规则了，推理结束。")
                   (return))
                 (let* ((weights (mapcar #'(lambda (rule) (getf (cddr rule) :weight 1.0))
                                         applicable-rules))
                        (probabilities (softmax weights temperature))
                        (rule-to-fire (sample-from-distribution applicable-rules probabilities)))
                   
                   (when (> (length applicable-rules) 1)
                     (format t "~&[SAMPLE] 竞争规则: ~a, 权重: ~a, 概率: ~a"
                             (mapcar #'second applicable-rules)
                             weights
                             (mapcar #'(lambda (p) (format nil "~,2f" p)) probabilities)))

                   (apply-rule rule-to-fire))))
        finally (format t "~&[WARN] 达到最大循环次数 ~d，推理强制结束。" max-cycles)))


;;; =================================================================
;;; 4. 主流程函数
;;; =================================================================

(defun get-generated-notes ()
  "从工作内存中提取所有生成的音符fact"
  (remove-if-not #'(lambda (fact) (and (listp fact) (eq (first fact) 'generated-note)))
                 *working-memory*))

(defun generate-music (initial-facts rules-file &key (cycles 100) (temperature 1.0))
  "初始化、加载规则、运行引擎并返回结果"
  (reset-wm)
  (let ((full-path (merge-pathnames rules-file)))
      (unless (probe-file full-path)
        (error "规则文件不存在: ~a" full-path))
      (load full-path))
  
  (format t "~&[INFO] 从 ~a 加载了 ~d 条规则。" rules-file (length *rules*))
  (format t "~&[INFO] 使用温度: ~a" temperature)

  (dolist (fact initial-facts)
    (add-fact fact))
  (format t "~&[INFO] 初始事实: ~a" *working-memory*)
  
  (format t "~&~%--- 开始音乐生成 ---~%")
  (run-engine cycles temperature)
  (format t "~&--- 生成结束 ---~%~%")
  
  (let ((notes (sort (get-generated-notes) #'< :key #'second)))
    (format t "生成的音乐序列: ~%")
    (dolist (note notes)
      (format t "  ~a~%" note))
    notes))

(provide "expert-system-v3") ; 标记这个文件已经被加载
