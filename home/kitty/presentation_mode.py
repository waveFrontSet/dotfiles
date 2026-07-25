# mypy: disable-error-code="import-not-found, import-untyped"
"""kitty kitten: toggle presentation mode.

Presentation mode blanks the tab bar (see ``tab_bar.py``) so customer/session
names stay hidden while screen sharing. This is a *no-UI* kitten: ``main`` runs
in a throwaway child process and does nothing, while ``handle_result`` runs
inside the kitty process, flips the flag on the shared ``Boss`` instance and
forces every tab bar to redraw.

Bound to ``cmd+shift+p`` in ``default.nix``.
"""

from __future__ import annotations

from kittens.tui.handler import result_handler
from kitty.boss import Boss

#: Attribute name shared with ``tab_bar.py``.
PRESENTATION_ATTR = "presentation_mode"


def main(args: list[str]) -> None:
    """No-UI kitten: nothing runs in the child process."""


@result_handler(no_ui=True)
def handle_result(
    args: list[str],
    answer: str,
    target_window_id: int,
    boss: Boss,
) -> None:
    """Toggle presentation mode and redraw every OS window's tab bar."""
    new_state = not getattr(boss, PRESENTATION_ATTR, False)
    setattr(boss, PRESENTATION_ATTR, new_state)
    for tab_manager in boss.os_window_map.values():
        tab_manager.mark_tab_bar_dirty()
