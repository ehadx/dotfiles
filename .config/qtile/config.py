import os
import subprocess

from libqtile import bar, layout, qtile, widget, hook, extension
from libqtile.config import Click, Drag, DropDown, Group, Key, Match, Screen, ScratchPad
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal, send_notification

mod = "mod4"
terminal = 'kitty'
browser = 'flatpak run one.ablaze.floorp'
rofi = 'rofi -show drun'

keys = [
    # a list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # switch between windows
    Key([mod], "h", lazy.layout.left(), desc="move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="move window focus to other window"),
    # move windows between left/right columns or move up/down in current stack.
    # moving out of range in columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="move window up"),
    # grow windows. if current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="reset all window sizes"),
    # toggle between split and unsplit sides of stack.
    # split = all windows displayed
    # unsplit = 1 window displayed, like max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "return", lazy.layout.toggle_split(),
        desc="toggle between split and unsplit sides of stack"),
    Key([mod], "return", lazy.spawn(terminal), desc="launch terminal"),
    Key([mod], 'w', lazy.spawn(browser), desc='launch web browser'),
    # toggle between different layouts as defined below
    Key([mod], "tab", lazy.next_layout(), desc="toggle between layouts"),
    Key([mod], "q", lazy.window.kill(), desc="kill focused window"),
    Key([mod], "f", lazy.window.toggle_fullscreen(), desc="toggle fullscreen on the focused window",),
    Key([mod], "t", lazy.window.toggle_floating(), desc="toggle floating on the focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="shutdown qtile"),
    Key([mod], "p", lazy.spawncmd(), desc="spawn a command using a prompt widget"),
]

# add key bindings to switch vts in wayland.
# we can't check qtile.core.name in default config as it is loaded before qtile is started
# we therefore defer the check until the key binding is run by using .when(func=...)
for vt in range(1, 8):
    keys.append(
        Key(
            ["control", "mod1"],
            f"f{vt}",
            lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
            desc=f"Switch to VT{vt}",
        )
    )


names = list("1234567890")
names.append('minus')
names.append('equal')

labels = list("1234567890-=")

groups = [Group(name, label=labels[i]) for i, name in enumerate(names)]

for group in groups:
    keys.extend([
        Key([mod], group.name, lazy.group[group.name].toscreen(),
            desc="Switch to group {}".format(group.name)),
        # Key([mod, "shift"],group.name, lazy.window.togroup(i.name, switch_group=True),
        #    desc="Switch to & move focused window to group {}".format(group.name),),
        Key([mod, "shift"], group.name, lazy.window.togroup(group.name),
            desc="move focused window to group {}".format(group.name)),
    ])

groups.append(ScratchPad('scratchpad', [
    DropDown('term', 'kitty --class=scratch', width=0.8, height=0.8, x=0.1, y=0.1, opacity=0.9),
]))
keys.extend([
    Key([mod], 'n', lazy.group['scratchpad'].dropdown_toggle('term')),
])

layouts = [
    layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=4),
    layout.Max(),
    layout.Stack(num_stacks=2),
    layout.Bsp(),
    layout.Matrix(),
    layout.MonadTall(),
    layout.MonadWide(),
    layout.RatioTile(),
    layout.Tile(),
    layout.TreeTab(),
    layout.VerticalTile(),
    layout.Zoomy(),
]

colors = [
    '#1f2335',
    '#24283b',
    '#292e42',
    '#3b4261',
    '#414868',
    '#545c7e',
    '#565f89',
    '#737aa2',
    '#a9b1d6',
    '#c0caf5',
    '#394b70',
    '#3d59a1',
    '#7aa2f7',
    '#7dcfff',
    '#b4f9f8',
    '#bb9af7',
    '#9d7cd8',
    '#ff9e64',
    '#ffc777',
    '#c3e88d',
    '#4fd6be',
    '#41a6b5',
    '#ff757f',
    '#c53b53',
    '#ff007c',
    '#ffffff',
]

widget_defaults = dict(
    font="Ubuntu Nerd Font",
    fontsize=14,
    padding=5,
    margin=5,
    active=colors[24],
    inactive=colors[4]
)
extension_defaults = widget_defaults.copy()


def init_widget_list(monitor_num):
    widget_list = [
        widget.CurrentLayout(),
        widget.Sep(),
        widget.GroupBox(
            highlight_method='line',
            use_mouse_wheel=False,
            hide_unused=True
        ),
        widget.Prompt(
            ignore_dups_histor=True,
        ),
        widget.Sep(),
        widget.WindowName(),
        widget.Spacer(),
        # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
        # widget.StatusNotifier(),
        widget.Systray(),
        widget.Sep(),
        widget.TextBox('󰛳', fontsize='16'),
        widget.Net(format='{down:.0f}{down_suffix} ↓↑ {up:.0f}{up_suffix}'),
        widget.Sep(),
        widget.TextBox('', fontsize='16'),
        widget.CPU(
            update_interval=2.0,
            format='{load_percent}%'
        ),
        widget.Sep(),
        widget.TextBox('󰋊', fontsize="18"),
        widget.HDD(
            device='nvme0n1',
            format='{HDDPercent}%',
        ),
        widget.Sep(),
        widget.TextBox('󰍛', fontsize="18"),
        widget.Memory(
            format='{MemUsed:.0f}/{MemTotal:.0f}{mm}',
            measure_mem='M',
        ),
        widget.Sep(),
        widget.TextBox('󱄠', fontsize="18"),
        widget.Volume(),
        widget.Sep(),
        widget.KeyboardLayout(
            configured_keyboards=['us', 'ara(mac)'],
            mouse_callbacks={
                'Button1': lazy.widget['keyboardlayout'].next_keyboard(),
            }
        ),
        widget.Sep(),
        widget.Battery(
            empty_char='󰂎',
            discharge_char='󱟥',
            charge_char='󰢜',
            full_char='󰂅',
            not_charging_char='󰂁',

            format='{char} {percent:2.0%}'
        ),
        widget.Sep(),
        widget.Clock(format="%a %d-%m %I:%M %p"),
    ] 
    return widget_list


keys.extend([Key([mod], 'Space', lazy.widget['keyboardlayout'].next_keyboard())])


def init_2nd_widget_list(monitor_num):
    widget_list = init_widget_list(monitor_num)
    return widget_list


screens = [
    Screen(
        background=colors[0],
        top=bar.Bar(init_widget_list('1'), 24)
        # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
        # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
    ),
    # You can uncomment this variable if you see that on X11 floating resize/moving is laggy
    # By default we handle these events delayed to already improve performance, however your system might still be struggling
    # This variable is set to None (no cap) by default, but you can set it to 60 to indicate that you limit it to 60 events per second
    # x11_drag_polling_rate = 60,
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# xcursor theme (string or None) and size (integer) for Wayland backend
wl_xcursor_theme = None
wl_xcursor_size = 24

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
