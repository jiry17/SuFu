import os 
from openai import OpenAI

if __name__ == "__main__":
    file_paths = []
    directory = "/Users/pro/Desktop/work/2025A/ml-dataset/SuFu/ml-benchmark"
    for root, dirs, files in os.walk(directory):
        for file in files:
            if not file.endswith(".f"): continue 
            ml_file = file[:-1] + "ml"
            f_path, ml_path = os.path.join(root, file), os.path.join(root, ml_file)
            if os.path.exists(ml_path): continue 
            file_paths.append((f_path, ml_path))
    
    template = """帮我把下面这个程序翻译到 ocaml，不要添加任何文字注释，也不要添加任何类型注释。此外：
1. 不允许有 let 中的 tuple unfold，例如 let (x, y) = ... 请用 match 来展开
2. 不允许有嵌套的 match pattern，例如 Cons (h, Cons (g2, _))
3. 如何最后一个函数名是 main，请把它改成 program"""

    client = OpenAI(
        base_url="https://llm.xmcp.ltd/",
        api_key="sk-zok7SB_DCOpnahhSyKwTMw",
    )

    for index, (inp, oup) in enumerate(file_paths):
        with open(inp, "r") as inp:
            lines = "".join(inp.readlines())
        prompt = template + "\n" + lines

        print("processing %d/%d: %s" % (index, len(file_paths), inp))


        messages = [{"role": "user", "content": prompt}]
        res = client.chat.completions.create(
            model = "yunwu/gpt-5-2025-08-07",
            messages = messages
        )
        content = res.choices[0].message.content 
        
        with open(oup, "w") as oup:
            oup.write(content)

