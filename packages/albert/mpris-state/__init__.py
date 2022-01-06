# -*- coding: utf-8 -*-

"""
Display the currently playing song.
"""

from configparser import ConfigParser
from pathlib import Path
from typing import Optional, Dict, List
from urllib.parse import urlparse, unquote
from urllib.request import urlopen
import dbus
import hashlib
import math
import os
import os.path

from albert import *


__title__ = "mpris status"
__version__ = "0.4.0"
__authors__ = "Jonathan Coates"


PROPERTY_BUS = "org.freedesktop.DBus.Properties"
PLAYER_BUS = "org.mpris.MediaPlayer2.Player"
MPRIS_BUS = "org.mpris.MediaPlayer2"

DATA_DIR: str = os.getenv("XDG_DATA_DIRS", "/usr/share:/usr/share/local")
HOME_DIR: str = os.getenv("XDG_HOME_DIR", os.path.expanduser("~/.local/share"))
DATA_DIRS: List[str] = [x for x in DATA_DIR.split(':') + HOME_DIR.split(':') if x != ""]

CACHE_PATH: str = os.path.join(cacheLocation(), __name__)
EXTENSIONS: List[str] = [".jpeg", ".png"]


session: dbus.SessionBus = None
applications: Dict[str, str] = {}


def get_url(url: str) -> Optional[str]:
    """Extract a local URL from a remote one."""
    try:
        parsed = urlparse(url)
    except ValueError:
        return None

    if parsed.scheme in ('file', ''):
        return unquote(parsed.path)
    elif parsed.scheme in ('http', 'https'):
        if url.startswith('https://open.spotify.com/image/'):
            url = 'https://i.scdn.co/image/' + url[len('https://open.spotify.com/image/'):]

        name = hashlib.sha1(url.encode("utf-8")).hexdigest()
        path = os.path.join(CACHE_PATH, name) + Path(parsed.path).suffix

        if os.path.isfile(path):
            info(f"Already downloaded at {path}")
            return path

        # Download the file to our cache. We should probably do this asynchronously,
        # but rely on the fact that the remote server is _probably_ fast enough.
        warning(f"Downloading {url} -> {path}")
        try:
            os.makedirs(CACHE_PATH, exist_ok=True)
            with urlopen(url) as read:
                with open(path, "wb") as write:
                    while chunk := read.read(2048):
                        write.write(chunk)

            return path
        except Exception as e:
            critical("Error getting image " + str(e))

            try:
                os.remove(path)
            except:
                pass

            return None
    else:
        return None


def initialize() -> None:
    global session
    session = dbus.SessionBus()

    # We build up a map of Application Name -> Icon, so we can give a nice icon.
    # This isn't foolproof by any means, shouldn't have any false-positives at
    # least.
    global applications
    for path in DATA_DIRS:
        for child in Path(path, "applications").glob("**/*.desktop"):
            config = ConfigParser(strict=False)
            config.read(child)
            main_config = config['Desktop Entry']

            if 'Icon' in main_config and 'Name' in main_config:
                icon = iconLookup(main_config['Icon'])
                if icon:
                    applications[main_config['Name']] = icon


def get_bus(name: str, interface: str) -> Optional[dbus.Interface]:
    """Get the dbus properties interface"""
    try:
        bus = session.get_object(
            "org.mpris.MediaPlayer2." + name, "/org/mpris/MediaPlayer2")
        return dbus.Interface(bus, interface)
    except dbus.exceptions.DBusException:
        return None


def format_time(time: float) -> str:
    time /= 1_000_000

    mins = math.floor(time / 60)
    secs = math.floor(time % 60)
    return f"{mins}:{secs:02d}"


def escape(msg: str) -> str:
    return msg.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&rt;")


def play_pause(name: str):
    def run() -> None:
        bus = get_bus(name, PLAYER_BUS)
        if bus is not None:
            bus.PlayPause()
    return run

def handleQuery(query):
    if query.string != "":
        return

    items = []

    dbus_meta = dbus.Interface(session.get_object("org.freedesktop.DBus", "/org/freedesktop/DBus"), "org.freedesktop.DBus")
    bus_name: str
    for bus_name in dbus_meta.ListNames():
        if bus_name.startswith(MPRIS_BUS + "."):
            bus_name = bus_name[len(MPRIS_BUS) + 1:]
            bus = get_bus(bus_name, PROPERTY_BUS)
            if bus is None:
                continue

            app = bus.Get(MPRIS_BUS, "Identity")
            properties = bus.GetAll(PLAYER_BUS)
            metadata = properties["Metadata"]
            if all(x not in metadata for x in ("xesam:title", "xesam:artist", "xesam:albumArtist")):
                continue

            title = metadata.get("xesam:title", "«Unknown»").strip()
            artists = metadata.get("xesam:albumArtist") or metadata.get("xesam:artist")
            artist = artists and artists[0].strip()

            text = escape(title)

            position = properties.get("Position", 0)
            if position != 0:
                text += f" {format_time(position)} / {format_time(metadata['mpris:length'])}"
            if artist:
                text += f" ({escape(artist)})"

            icon = None
            if "mpris:artUrl" in metadata:
                icon = get_url(str(metadata["mpris:artUrl"]))

            if icon is None:
                icon = applications.get(str(app), ':python_module')

            items.append(Item(
                id="%s.%s" % (__name__, app),
                icon=icon,
                text=str(app),
                subtext=text,
                actions=[
                    FuncAction(text="Play/Pause", callable=play_pause(bus_name)),
                ],
            ))

    return items
