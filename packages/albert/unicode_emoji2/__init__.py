# -*- coding: utf-8 -*-

"""
Offline Unicode emoji picker.

Synopsis: <trigger> [filter]
"""

from albert import *
from collections import namedtuple
from threading import Thread
import os
import subprocess
import json
from typing import Optional

__title__ = "Unicode Emojis (v2)"
__version__ = "0.4.4"
__triggers__ = ":"
__authors__ = ["Tim Zeitz", "manuelschneid3r", "SquidDev"]
__exec_deps__ = ["convert"]

EmojiSpec = namedtuple('EmojiSpec', ['string', 'name'])
emoji_data_path = os.path.dirname(__file__) + "/emoji_tbl.json"
icon_path_template = os.path.join(cacheLocation(), __name__, "%s.png")
emojiSpecs = []
thread = None


class WorkerThread(Thread):
    def __init__(self):
        super().__init__()
        self.stop = False

    def run(self):

        # Create cache dir
        cache_dir_path = os.path.join(cacheLocation(), __name__)
        if not os.path.exists(cache_dir_path):
            os.mkdir(cache_dir_path)

        # Build the index and icon cache
        # global emojiSpecs
        emojiSpecs.clear()
        with open(emoji_data_path) as f:
            store = json.load(f)

        for name, emoji in store.items():
            emojiSpecs.append(EmojiSpec(emoji, name))

            icon_path = icon_path_template % emoji
            if not os.path.exists(icon_path):
                subprocess.call(["convert", "-pointsize", "64", "-background", "transparent", "pango:%s" % emoji, icon_path])

            if self.stop:
                return

def initialize():
    # Build the index and icon cache
    global thread
    thread = WorkerThread()
    thread.start()


def finalize():
    global thread
    if thread is not None:
        thread.stop = True
        thread.join()

SCORE_WEIGHT = 1000
ADJACENCY_BONUS = 5
LEADING_LETTER_PENALTY = -3
LEADING_LETTER_PENALTY_MAX = -9
UNMATCHED_LETTER_PENALTY = -1


def match(test: str, pattern: str) -> Optional[int]:
    """Match an test input against a pattern"""
    best_score, matched = 0, False

    test_lower = test.lower()
    pattern_lower = pattern.lower()

    if pattern == "":
        return SCORE_WEIGHT + UNMATCHED_LETTER_PENALTY * len(test)

    start = 0
    while True:
        start = test_lower.find(pattern_lower[0], start) + 1
        if start <= 0:
            break

        score = SCORE_WEIGHT + \
            max(LEADING_LETTER_PENALTY * start, LEADING_LETTER_PENALTY_MAX)
        previous_match = True

        test_pos, pattern_pos = start, 1
        while test_pos < len(test) and pattern_pos < len(pattern):
            test_char = test_lower[test_pos]
            pattern_char = pattern_lower[pattern_pos]

            if test_char == pattern_char:
                if previous_match:
                    score += ADJACENCY_BONUS
                previous_match = True
                pattern_pos = pattern_pos + 1
            else:
                score += UNMATCHED_LETTER_PENALTY
                previous_match = False

            test_pos += 1

        if pattern_pos >= len(pattern) and (not matched or score > best_score):
            matched = True
            best_score = score

    return best_score if matched else None


def handleQuery(query):
    if not query.isValid or not query.isTriggered:
        return

    items = []

    pattern = query.string
    if pattern.endswith(":"):
        pattern = pattern[:-1]

    for es in emojiSpecs:
        if not match(es.name, pattern):
            continue

        items.append(
            Item(
                id="%s%s" % (__name__, es.string),
                completion=":%s:" % es.name,
                icon=icon_path_template % es.string,
                text=es.name.capitalize(),
                actions=[ClipAction("Copy to clipboard", es.string)]
            )
        )
    return items
