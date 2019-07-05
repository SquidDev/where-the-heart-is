# Start the Gnome Keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)

# Set fallback cursor.
xsetroot -cursor_name left_ptr

# Finally launch Emacs. The exciting stuff
exec dbus-launch --exit-with-session emacs
