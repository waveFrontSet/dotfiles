# mypy: disable-error-code="import-not-found, import-untyped"
"""Custom kitty tab bar.

Delegates to kitty's built-in *round powerline* renderer, so the tab bar looks
exactly like ``tab_bar_style = powerline`` — except while **presentation mode**
is active, when every tab is drawn blank so no session/customer names leak during
screen sharing.

Presentation mode is a boolean stored on the shared ``Boss`` instance and toggled
by the ``presentation_mode.py`` kitten (bound to ``cmd+shift+p``). kitty loads
this file because of ``tab_bar_style = custom`` in ``kitty.conf`` and calls
``draw_tab`` once per tab.
"""

from __future__ import annotations

from kitty.boss import Boss
from kitty.fast_data_types import Screen, get_boss
from kitty.tab_bar import DrawData, ExtraData, TabBarData, draw_tab_with_powerline

#: Attribute name shared with ``presentation_mode.py``.
PRESENTATION_ATTR = "presentation_mode"


def presentation_mode_active() -> bool:
    """Whether the tab bar should currently hide all titles."""
    boss: Boss = get_boss()
    return bool(getattr(boss, PRESENTATION_ATTR, False))


def draw_tab(
    draw_data: DrawData,
    screen: Screen,
    tab: TabBarData,
    before: int,
    max_tab_length: int,
    index: int,
    is_last: bool,
    extra_data: ExtraData,
) -> int:
    """Render a single tab (kitty's ``custom`` tab-bar entry point)."""
    if presentation_mode_active():
        # Draw nothing: the bar remains but is blank, hiding all tab titles.
        return screen.cursor.x
    return draw_tab_with_powerline(
        draw_data, screen, tab, before, max_tab_length, index, is_last, extra_data
    )
