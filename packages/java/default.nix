{config, lib, pkgs, ...}:
  let
    # We use jetbrains-jdk as it has DCEVM support.
    jetbrains-jdk = pkgs.stdenv.mkDerivation {
      name = "jetbrains.jdk-17";
      version = "17.0.4.1-b629.2";
      src = fetchTarball {
        url = "https://cache-redirector.jetbrains.com/intellij-jbr/jbrsdk-17.0.4.1-linux-x64-b629.2.tar.gz";
        sha256 = "0m8k6z36hnf3wfk04jh9skwv8f2qdq384jqdkdsr713m9nmw553s";
      };
      installPhase = ''
        out="$out/lib/jvm/java-17-jetbrains"
        mkdir -p $out
        cp -r . $out
      '';

      meta = with lib; {
        description = "An OpenJDK fork to better support Jetbrains's products.";
        homepage = "https://github.com/JetBrains/JetBrainsRuntime/";
      };
    };
  in {
    home.packages = [
      jetbrains-jdk
    ];
  }
