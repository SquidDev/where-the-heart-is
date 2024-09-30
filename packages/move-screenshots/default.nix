{ lib, pkgs, ... }:
let
  move-screenshots = pkgs.stdenv.mkDerivation {
    name = "move-screenshots";
    src = ./move-screenshots.py;
    propagatedBuildInputs = [pkgs.python3];
    dontUnpack = true;
    installPhase = "install -Dm755 $src $out/bin/move-screenshots";
  };
in {
  systemd.user.timers.move-screenshots = {
    Unit = {
      Description = "Organise screenshots into per-month folders";
    };
    Timer = {
      OnBootSec = "1m";
      OnUnitActiveSec = "1hour";
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };

  systemd.user.services.move-screenshots = {
    Unit = {
      Description = "Organise screenshots into per-month folders";
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${move-screenshots}/bin/move-screenshots";
    };
  };
}
