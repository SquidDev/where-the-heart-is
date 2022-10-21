{ pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtkNativeComp;
  };

  services.emacs.enable = true;

  xdg.desktopEntries.emacsclient = {
    name = "Emacs (Nix Client)";
    genericName = "Text Editor";
    comment = "Edit text";
    mimeType = [
      # All the boring ones
      "text/english" "text/plain" "text/x-makefile" "text/x-c++hdr" "text/x-c++src" "text/x-chdr" "text/x-csrc"
      "text/x-java" "text/x-moc" "text/x-pascal" "text/x-tcl" "text/x-tex" "application/x-shellscript" "text/x-c"
      "text/x-c++"
      # Org Mode
      "x-scheme-handler/org-protocol"
    ];
    exec = ''${pkgs.emacsPgtkNativeComp}/bin/emacsclient --create-frame %U'';
    icon = "emacs";
    type = "Application";
    terminal = false;
    categories = ["Development" "TextEditor"];
    startupNotify = true;

    settings = {
      StartupWMClass = "Emacsd";
      Keywords = "Text;Editor;";
    };
  };
}
