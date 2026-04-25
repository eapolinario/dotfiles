# Disk layout used by nixos-anywhere to partition and format the target machine.
# Reference: https://github.com/nix-community/disko/tree/master/example
#
# TODO: Customize this for your target machine:
#   1. Set `device` to the correct disk (check with: lsblk)
#   2. Choose a filesystem: ext4 (simple) or btrfs (snapshots/compression)
#   3. Add swap if needed, or LUKS encryption (see disko examples)
{
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        device = "/dev/sda"; # <-- set your target disk (e.g. /dev/nvme0n1, /dev/vda)
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "512M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };
            root = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4"; # or "btrfs"
                mountpoint = "/";
              };
            };
          };
        };
      };
    };
  };
}
