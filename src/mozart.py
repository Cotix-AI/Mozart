import socket
import os
from openai import OpenAI

def get_lisp_command_from_llm(user_prompt: str) -> str:
    client = OpenAI(api_key=os.environ.get("OPENAI_API_KEY"))
    system_prompt = """
    你是一个自然语言到 Lisp 代码的翻译器。
    你的任务是将用户的音乐创作请求转换为一个特定的 Lisp 函数调用。
    函数签名是: (generate-music initial-facts rules-file &key (temperature 1.0))

    - initial-facts: 一个事实列表，用于启动专家系统。必须包含 '(start-generation)'.
      可以根据用户请求添加额外的事实，如 '(style sad)', '(key A-minor)', '(tempo fast)'.
    - rules-file: 指向规则库文件的字符串。根据风格选择不同的文件。
      例如: "rules/happy-major.lisp", "rules/sad-minor.lisp", "rules/ambient.lisp".
    - temperature: 一个浮点数。较高的温度 (如 1.5) 意味着更有创意和随机性。
      较低的温度 (如 0.6) 意味着更保守和结构化。

    你的输出必须是且仅是一行 Lisp 代码，不包含任何解释或代码块标记。

    --- Shots ---
    用户: "创作一首快乐的C大调歌曲"
    你: (generate-music '((start-generation) (style happy) (key C-major)) "rules/happy-major.lisp" :temperature 1.4)

    用户: "生成一段缓慢、悲伤的A小调旋律"
    你: (generate-music '((start-generation) (style sad) (key A-minor) (tempo slow)) "rules/sad-minor.lisp" :temperature 0.7)
    
    用户: "来点有实验性的、随机的音乐"
    你: (generate-music '((start-generation) (style experimental)) "rules/ambient.lisp" :temperature 2.0)
    """

    print("\n[LLM] 正在将您的请求翻译为 Lisp 命令...")
    try:
        response = client.chat.completions.create(
            model="gpt-4o",
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt}
            ],
            temperature=0.2,
        )
        lisp_command = response.choices[0].message.content.strip()
        print(f"[LLM] 翻译结果: {lisp_command}")
        return lisp_command
    except Exception as e:
        print(f"[ERROR] 调用 LLM API 失败: {e}")
        return None

def communicate_with_lisp(command: str, host='127.0.0.1', port=4242) -> str:
    print(f"\n[Socket] 正在连接 Lisp 服务器 ({host}:{port})...")
    try:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.connect((host, port))
            print("[Socket] 连接成功！正在发送命令...")
            s.sendall((command + '\n').encode('utf-8'))
            
            print("[Socket] 命令已发送，等待结果...")
            data = s.recv(4096) 
            response = data.decode('utf-8').strip()
            
            print("[Socket] 收到结果！")
            return response
    except ConnectionRefusedError:
        return "ERROR: 连接被拒绝。请确保 Lisp 服务器正在运行。"
    except Exception as e:
        return f"ERROR: 通信期间发生错误: {e}"

def main():
    print("欢迎使用 Mozart。")
    print("输入您的创作指令 (例如: '创作一首欢快的乐曲' 或 'quit')")

    while True:
        user_input = input("\n> ")
        if user_input.lower() in ['退出', 'exit', 'quit']:
            break

        # 1. 从 LLM 获取 Lisp 命令
        lisp_command = get_lisp_command_from_llm(user_input)
        if not lisp_command or lisp_command.startswith("ERROR"):
            continue

        # 2. 与 Lisp 服务器通信
        result = communicate_with_lisp(lisp_command)

        # 3. 显示结果
        print("\n--- 生成结果 ---")
        if result.startswith("ERROR:"):
            print(result)
        else:
            # 简单的格式化输出
            notes = result.replace("(", " (").strip().split('(')
            for note in notes:
                if 'GENERATED-NOTE' in note:
                    print(f"  {note.replace(')', '').strip()}")
        print("--------------------")

if __name__ == "__main__":
    main()
