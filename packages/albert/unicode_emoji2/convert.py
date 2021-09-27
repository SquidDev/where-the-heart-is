import re
import json

# https://api.github.com/emojis
with open("emoji.json", "r") as h:
    data = json.load(h)

URL = re.compile(r"/([^/.]+).png")

emojis = {}
for name, url in data.items():
    match = URL.search(url)
    if not match:
        print(url)
        continue

    codes = match[1].split('-')
    try:
        emoji = "".join(chr(int(x, 16)) for x in codes)
    except ValueError:
        continue

    emojis[name] = emoji

with open("emoji_tbl.json", "w") as h:
    json.dump(emojis, h)
