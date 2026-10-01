#!/usr/bin/env bash
#
# Reboot into Ubuntu once without changing the default UEFI boot entry.
#
# UEFI BootNext selects Ubuntu for the next restart only. The normal BootOrder
# remains unchanged, so Windows stays the default for later boots.
#
# Usage: boot-ubuntu [-y]
#   -y, --yes    reboot immediately without confirmation
#
set -euo pipefail

usage() {
  echo "usage: boot-ubuntu [-y]" >&2
  echo "  Reboot once into Ubuntu via UEFI BootNext." >&2
  exit 1
}

assume_yes=false
case "${1:-}" in
  -y|--yes) assume_yes=true ;;
  -h|--help) usage ;;
  "") ;;
  *) usage ;;
esac

# Find the first "ubuntu" UEFI boot entry number (4 hex digits).
entry="$(efibootmgr | grep -i 'ubuntu' | head -1 || true)"
if [[ -z "$entry" ]]; then
  echo "error: no 'ubuntu' UEFI boot entry found." >&2
  echo "       run 'efibootmgr' to inspect available entries." >&2
  exit 1
fi
num="$(echo "$entry" | sed -E 's/^Boot([0-9A-Fa-f]{4}).*/\1/')"

echo "Found Ubuntu: Boot${num}"
sudo efibootmgr --bootnext "$num" >/dev/null
echo "Staged a one-time boot into Ubuntu; Windows remains the default afterward."

if ! $assume_yes; then
  read -rp "Reboot into Ubuntu now? [y/N] " ans
  case "$ans" in
    y|Y|yes|YES) ;;
    *)
      echo "Not rebooting. Ubuntu will start on the next restart."
      echo "To cancel:  sudo efibootmgr --delete-bootnext"
      exit 0
      ;;
  esac
fi

echo "Rebooting into Ubuntu..."
sudo reboot
