{ pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      package = pkgs.emacs-pgtk;
      alwaysEnsure = true;
    };
  };

  services.emacs.enable = true;

  xdg.desktopEntries.emacsclient = {
    name = "Emacs (Client)";
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
    # We use this massively cursed string to launch a new emacs instance (--create-frame) if launched directly, and try
    # to reuse an existing one if opening a file.
    exec = ''${pkgs.emacs-pgtk}/bin/emacsclient --alternate-editor= -r %U'';
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

  # We don't want to take complete control over the mime file, so manually add it.
  home.activation.emacs = ''
    xdg-mime default emacsclient.desktop x-scheme-handler/org-protocol
  '';
}
