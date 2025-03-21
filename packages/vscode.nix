{...}: {
  programs.vscode = {
    enable = true;

    profiles.default.keybindings = [
      # Ctrl(+Shift)+Tab switches between tabs in order, rather than most
      # recently used.
      { key = "ctrl+shift+tab"; command = "workbench.action.previousEditor"; }
      { key = "ctrl+tab";       command = "workbench.action.nextEditor"; }
    ];
  };
}
