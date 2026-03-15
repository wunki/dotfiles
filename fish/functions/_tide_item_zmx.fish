function _tide_item_zmx
    if set -q ZMX_SESSION
        _tide_print_item zmx '⟨'$ZMX_SESSION'⟩'
    end
end
