import os
from dataclasses import dataclass
from hashlib import sha512

from kittens.ssh.main import get_connection_data
from kitty.constants import config_dir
from kitty.fast_data_types import Screen, get_boss
from kitty.tab_bar import DrawData, ExtraData, TabBarData, as_rgb, draw_tab_with_powerline
from kitty.window import CwdRequest

__all__ = ("draw_tab",)


@dataclass
class StatusPart:
    contents: str
    priority: int
    index: int = 0


@dataclass
class Server:
    bg: int = 0xEEAD0E
    fg: int = 0x3E3D31


_servers = {
    # We hash the hostname as a vague obfuscation method for some personal/work servers.
    "da69848bdf66776a": Server(bg=0xC15855),
    "6f28d32ea2bad5c3": Server(bg=0x90EE90),
    "88138504e31a2ba7": Server(bg=0xd6264f),
    "80ff834aa3664ea0": Server(bg=0xd6264f),
}


def _get_icon(host: str) -> str | None:
    if "/" in host:
        return None

    icon_dir = os.path.join(config_dir, "server-imgs")

    icon = os.path.join(icon_dir, f"{_hash_hostname(host)}.png")
    return icon if os.path.isfile(icon) else None


def _get_connection_data(window):
    if args := window.ssh_kitten_cmdline():
        idx = args.index("ssh")
        args = [arg for arg in args[idx:] if not arg.startswith("--kitten=")]
        return get_connection_data(args, window.cwd_of_child)

    return get_connection_data(window.child.foreground_cmdline, window.cwd_of_child)


def _hash_hostname(host: str) -> str:
    return sha512(host.encode()).hexdigest()[0:16]


def draw_tab(
    draw_data: DrawData,
    screen: Screen,
    tab: TabBarData,
    before: int,
    max_title_length: int,
    index: int,
    is_last: bool,
    extra_data: ExtraData,
) -> int:
    draw_tab_with_powerline(draw_data, screen, tab, before, max_title_length, index, is_last, extra_data)

    if is_last and (tm := get_boss().active_tab_manager) and (window := tm.active_window):
        parts: list[StatusPart] = []

        current_fg = int(draw_data.active_fg)
        current_bg = int(draw_data.active_bg)

        if cwd := CwdRequest(window).cwd_of_child:
            parts.append(StatusPart(cwd, 1))

        if conn_data := _get_connection_data(window):
            parts.append(StatusPart(conn_data.hostname, 2))
            if server := _servers.get(_hash_hostname(conn_data.hostname)):
                current_fg = server.fg
                current_bg = server.bg

        for i, part in enumerate(parts):
            part.index = i
        parts.sort(key=lambda part: part.priority, reverse=True)

        remaining = screen.columns - screen.cursor.x - 1
        part_strs: list[str | None] = [None] * len(parts)
        for part in parts:
            part_len = len(part.contents) + 2
            if part_len > remaining:
                break
            else:
                part_strs[part.index] = part.contents
                remaining -= part_len + 1

        part_str = "\uE0B9".join(f" {x} " for x in part_strs if x)
        if part_str:
            fg = as_rgb(current_fg)
            bg = as_rgb(current_bg)

            screen.cursor.bold = True
            screen.cursor.italic = False
            screen.cursor.bg = as_rgb(int(draw_data.active_fg))
            screen.cursor.fg = bg
            screen.cursor.x = screen.columns - len(part_str) - 1
            screen.draw("")

            screen.cursor.bg = bg
            screen.cursor.fg = fg

            screen.draw(part_str)

        for kitty_tab in tuple(tm.tabs):
            for kitty_window in tuple(kitty_tab.windows):
                icon = None
                if conn_data := _get_connection_data(kitty_window):
                    icon = _get_icon(conn_data.hostname)

                if getattr(kitty_window, "$$icon", None) != icon:
                    kitty_window.set_logo(icon or "")
                    setattr(kitty_window, "$$icon", icon)

    return screen.cursor.x
