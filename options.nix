{ lib, ... }: {
  options.where-the-heart-is = {
    window-decorations = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = "Whether to display window decorations.";
    };
  };
}
