#!/usr/bin/env bash
#
# boot-windows — reboot once into Windows, then revert to Ubuntu.
#
# Uses the UEFI BootNext variable: the firmware boots the Windows Boot
# Manager entry exactly once on the next restart, then automatically
# falls back to the normal BootOrder (Ubuntu). Nothing is changed
# permanently, and no boot menu needs to be visible — handy when the
# display stays dark until the OS loads.
#
# Usage: boot-windows [-y]
#   -y, --yes    reboot immediately without confirmation
#
set -euo pipefail

usage() {
  echo "usage: boot-windows [-y]" >&2
  echo "  Reboot once into Windows via UEFI BootNext, then back to Ubuntu." >&2
  exit 1
}

assume_yes=false
case "${1:-}" in
  -y|--yes) assume_yes=true ;;
  -h|--help) usage ;;
  "") ;;
  *) usage ;;
esac

# Find the "Windows Boot Manager" UEFI boot entry number (4 hex digits).
entry="$(efibootmgr | grep -i 'Windows Boot Manager' | head -1 || true)"
if [[ -z "$entry" ]]; then
  echo "error: no 'Windows Boot Manager' UEFI boot entry found." >&2
  echo "       run 'efibootmgr' to inspect available entries." >&2
  exit 1
fi
num="$(echo "$entry" | sed -E 's/^Boot([0-9A-Fa-f]{4}).*/\1/')"

echo "Found Windows Boot Manager: Boot${num}"
sudo efibootmgr --bootnext "$num" >/dev/null
echo "Staged a one-time boot into Windows; it reverts to Ubuntu automatically afterward."

if ! $assume_yes; then
  read -rp "Reboot into Windows now? [y/N] " ans
  case "$ans" in
    y|Y|yes|YES) ;;
    *)
      echo "Not rebooting now — Windows will start on your NEXT restart."
      echo "To cancel:  sudo efibootmgr --delete-bootnext"
      exit 0
      ;;
  esac
fi

echo "Rebooting into Windows…"
sudo reboot
