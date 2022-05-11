function nix-sh --description "Starts a new shell inside the nix chroot"
  nix-user-chroot $HOME/.local/share/nix bash -c '. $HOME/.nix-profile/etc/profile.d/nix.sh && fish' $argv;
end
