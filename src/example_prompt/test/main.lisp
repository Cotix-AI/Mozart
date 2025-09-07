(load "mozart.lisp")

(defun random-note-from-chord (chord scale)
  (declare (ignore chord scale))
  'A4)

;; ==========================================================
;; ===            PROMPT DEFINITION AREA                  ===
;; ==========================================================
(defparameter *sonata-script*
  '(
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
    (80 1 (change-chord-to C))
    (80 4 (finish-composition))
  ))

;; 加载规则库，并使用宏根据剧本生成规则
(load "rules.lisp")
(generate-rules-from-script *sonata-script*)

;; ==========================================================
;; ===            MAIN EXECUTION LOGIC                    ===
;; ==========================================================
(defun main ()
  (reset-wm)
  
  ;; 初始事实
  (add-fact '(phase exposition))
  (add-fact '(measure 1))
  (add-fact '(beat 1))
  (add-fact '(current-chord C))
  
  (run-engine 1000 0.2))

(main)
