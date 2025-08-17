(ql:quickload "usocket")
(load "mozart.lisp")

(defun handle-connection (stream)
  (let ((command-string (read-line stream)))
    (format t "[INFO] 收到命令: ~s~%" command-string)
    (handler-case
        (let* (
               (command-form (read-from-string command-string))
               (result (eval command-form)))
        
          (let ((result-string (prin1-to-string result)))
            (write-line result-string stream)
            (finish-output stream)
            (format t "[INFO] 已发送结果: ~s~%" result-string)))
      
      (error (c)
        (let ((error-message (format nil "ERROR: ~a" c)))
          (write-line error-message stream)
          (finish-output stream)
          (format t "[ERROR] 执行命令时出错: ~a~%" c))))))

(defun start-server (&key (port 4242))
  (format t "[INFO] Port:~d...~%" port)
  (usocket:with-socket-listener (socket "127.0.0.1" port)
    (loop
      (usocket:with-server-socket (connection (usocket:socket-accept socket))
        (handle-connection (usocket:socket-stream connection))))))

(start-server)
