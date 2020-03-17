HYPER = ['command', 'option', 'control']

non_apple_modifiers_rule = {
    'description': 'Non-Apple modifiers (L-Command -> Hyper)',
    # Exclude all Apple devices.
    'conditions': [
        {
            'type': 'device_unless',
            'identifiers': [
                {
                    'vendor_id': 1452
                }
            ]
        }
    ],
    'manipulators': [
        (
            ('left_command', None, 'any'),
            ('left_command', ['left_option', 'left_control'])
        )
    ]
}

apple_modifiers_rule = {
    'description': 'Apple modifiers (L-Option -> Hyper, L-Command -> L-Option)',
    # Only target Apple devices.
    'conditions': [
        {
            'type': 'device_if',
            'identifiers': [
                {
                    'vendor_id': 1452
                }
            ]
        }
    ],
    'manipulators': [
        (
            ('left_command', None, 'any'),
            ('left_option')
        ),
        (
            ('left_option', None, 'any'),
            ('left_command', ['left_option', 'left_control']),
        ),
        (
            ('right_command', None, 'any'),
            ('right_option')
        ),
        (
            ('right_option', None, 'any'),
            ('right_command')
        )
    ]
}

remap_caps_lock_and_control_rule = {
    'description': ', '.join([
        'Caps Lock -> Control',
        'Control_L / Fn_L -> Command_L',
        'Shift_L + Shift_R -> Caps Lock'
    ]),
    'manipulators': [
        # Caps lock to Control_L.
        (
            ('caps_lock', None, 'any'),
            ('left_control')
        ),

        # Control_L -> Command_L. Tapping Control_L it for <100ms triggers caps
        # lock.
        {
            'from': ('left_control', None, 'any'),
            'to': ('left_command'),
            'to_if_alone': [
                {
                    'hold_down_milliseconds': 100,
                    'key_code': 'caps_lock'
                }
            ],
            'parameters': {
                'basic.to_if_alone_timeout_milliseconds': 200
            }
        },

        # Fn -> Command_L. Tapping Fn it for <100ms triggers caps
        # lock.
        {
            'from': ('fn', None, 'any'),
            'to': ('left_command'),
            'to_if_alone': [
                {
                    'hold_down_milliseconds': 100,
                    'key_code': 'caps_lock'
                }
            ],
            'parameters': {
                'basic.to_if_alone_timeout_milliseconds': 200
            }
        },

        # Left Shift + Right Shift (simultaneously) -> Caps Lock.
        (
            {
                'simultaneous': [
                    {
                        'key_code': 'left_shift'
                    },
                    {
                        'key_code': 'right_shift'
                    }
                ]
            },
            ('caps_lock')
        )
    ]
}

# Remaps the Pause key to Eject. This is useful because we can then use the
# default MacOS Control + Shift + Eject shortcut to show the lock screen.
pause_to_eject_rule = {
    'description': 'Pause / Break -> Eject',
    'manipulators': [
        (
            ('pause', None, 'any'),
            ('eject')
        )
    ]
}

mouse_bindings_rule = {
    'description': 'Mouse bindings: Back, Forward, Control + Click',
    'manipulators': [
        # Remap the mouse "Back" button. Note that we need to define the "from"
        # clause in its raw form to use the `pointing_button` directive.
        (
            {
                'pointing_button': 'button4'
            },
            ('open_bracket', 'left_command')
        ),

        # Control + "Back" button -> Forward.
        (
            {
                'pointing_button': 'button4',
                'modifiers': {
                    'mandatory': [
                        'control'
                    ]
                }
            },
            ('close_bracket', 'left_command')
        ),

        # Control + Left Click emulates Right Click by default, which is really
        # annoying. Here, we remap it to Command + Left Click instead, which is
        # useful for opening links in new tabs in Chrome.
        (
            {
                'pointing_button': 'button1',
                'modifiers': {
                    'mandatory': [
                        'control'
                    ]
                }
            },
            {
                'pointing_button': 'button1',
                'modifiers': ['left_command']
            }
        )
    ]
}

control_based_shortcuts_rule = {
    'description': 'Control-based shortcuts',
    'conditions': [
        {
            'type': 'frontmost_application_unless',
            'bundle_identifiers': [
                'org.gnu.Emacs',
                'com.googlecode.iterm2',
                'com.microsoft.VSCode',
            ]
        }
    ],
    'manipulators': [
        # Control -> Command shortcuts
        (
            ('l', 'control'),
            ('l', 'left_command')
        ),
        (
            ('t', 'control', 'shift'),
            ('t', 'left_command')
        ),
        (
            ('r', 'control', 'shift'),
            ('r', 'left_command')
        ),
        (
            ('c', 'control'),
            ('c', 'left_command')
        ),
        (
            ('v', 'control'),
            ('v', 'left_command')
        ),
        (
            ('x', 'control'),
            ('x', 'left_command')
        ),
        (
            ('z', 'control'),
            ('z', 'left_command')
        ),
        (
            ('open_bracket', 'control'),
            ('open_bracket', 'left_command')
        ),
        (
            ('close_bracket', 'control'),
            ('close_bracket', 'left_command')
        ),
        (
            ('equal_sign', 'control'),
            ('equal_sign', 'left_command')
        ),
        (
            ('hyphen', 'control'),
            ('hyphen', 'left_command')
        ),
        (
            ('0', 'control'),
            ('0', 'left_command')
        ),

        # Control + Option -> Command shortcuts
        (
            ('w', ['control', 'option']),
            ('w', 'left_command')
        ),
        (
            ('f', ['control', 'option']),
            ('f', 'left_command')
        ),

        # Control + Left / Right Arrow should allow skipping a word at a time.
        (
            ('left_arrow', 'control', 'shift'),
            ('left_arrow', 'left_option')
        ),
        (
            ('right_arrow', 'control', 'shift'),
            ('right_arrow', 'right_option')
        )
    ]
}

page_up_down_home_end_rule = {
    'description': 'Command + Arrow -> Page Up / Page Down / Home / End',
    'conditions': [
        {
            'type': 'frontmost_application_unless',
            'bundle_identifiers': [
                'com.apple.finder',
                'com.googlecode.iterm2'
            ]
        }
    ],
    'manipulators': [
        (
            ('up_arrow', 'command', 'shift'),
            ('page_up'),
        ),
        (
            ('down_arrow', 'command', 'shift'),
            ('page_down'),
        ),
        (
            ('left_arrow', 'command', 'shift'),
            ('home'),
        ),
        (
            ('right_arrow', 'command', 'shift'),
            ('end'),
        )
    ]
}

pc_home_end_rule = {
    'description': 'PC-style Home / End',
    'manipulators': [
        (
            ('home'),
            ('a', 'left_control')
        ),
        (
            ('end'),
            ('e', 'left_control')
        )
    ]
}

switch_tabs_rule = {
    'description': 'Control + [1-9] -> Command + [1-9] in browsers and terminal',
    'conditions': [
        {
            'type': 'frontmost_application_if',
            'bundle_identifiers': [
                'com.google.Chrome',
                'com.googlecode.iterm2'
            ]
        }
    ],
    'manipulators': [
        (
            (str(i), 'control'),
            (str(i), 'left_command')
        ) for i in range(1, 10)
    ]
}

keepassx_rule = {
    'description': 'KeePassX: Copy username and password',
    'conditions': [
        {
            'type': 'frontmost_application_if',
            'bundle_identifiers': [
                '\\.keepassx$'
            ]
        }
    ],
    'manipulators': [
        (
            ('c', ['control', 'option']),
            [
                ('c', 'left_command'),
                # HACK: Simulates a 'sleep' before we execute the next key. We
                # need this because KeePassX can't receive keystrokes fast
                # enough.
                *[('escape') for i in range(20)],
                ('b', 'left_command')
            ]
        ),
        (
            ('b', 'control'),
            ('b', 'left_command')
        ),
        (
            ('u', 'control'),
            ('u', 'left_command')
        )
    ]
}

macpass_rule = {
    'description': 'MacPass: Copy username and password',
    'conditions': [
        {
            'type': 'frontmost_application_if',
            'bundle_identifiers': [
                'com.hicknhacksoftware.MacPass'
            ]
        }
    ],
    'manipulators': [
        # Copy both the username and password in a single hotkey.
        (
            ('c', ['control', 'option']),
            [
                ('c', ['left_command', 'left_option']),
                # HACK: Simulates a 'sleep' before we execute the next key. We
                # need this because MacPass can't receive keystrokes fast
                # enough.
                *[('escape') for i in range(20)],
                ('c', ['left_command', 'left_shift'])
            ]
        ),

        # Ctrl + G to clear out of the "Find" dialog. This works by first
        # focusing on the Groups menu, then tapping the up arrow followed by the
        # down arrow.
        (
            ('g', 'control'),
            [
                ('g', ['left_command', 'left_option']),
                ('up_arrow'),
                ('down_arrow'),
            ]
        ),
    ]
}

emacs_rule = {
    'description': 'Emacs everywhere hotkeys',
    'conditions': [
        {
            'type': 'frontmost_application_unless',
            'bundle_identifiers': [
                'org.gnu.Emacs',
                'com.googlecode.iterm2',
                'com.microsoft.VSCode',
            ]
        }
    ],
    'manipulators': [
        # Cursor up.
        (
            ('p', 'control'),
            ('up_arrow'),
        ),
        # Cursor down.
        (
            ('n', 'control'),
            ('down_arrow'),
        ),
        # Cursor left.
        (
            ('b', 'control'),
            ('left_arrow'),
        ),
        # Cursor right.
        (
            ('f', 'control'),
            ('right_arrow')
        ),
        # Cursor back one word.
        (
            ('b', 'option'),
            ('left_arrow', ['left_option'])
        ),
        # Cursor forward one word.
        (
            ('f', 'option'),
            ('right_arrow', ['left_option'])
        ),
        # Cursor to beginning of line.
        (
            ('a', 'control'),
            ('left_arrow', ['left_command'])
        ),
        # Cursor to end of line.
        (
            ('e', 'control'),
            ('right_arrow', ['left_command'])
        ),
        # Delete word backward.
        (
            ('w', 'control'),
            ('delete_or_backspace', ['left_option'])
        ),
        # Delete word forward.
        (
            ('d', 'option'),
            ('delete_forward', ['left_option'])
        ),
        # Kill line. This is already built in to MacOS, but its default
        # implementation doesn't do anything if the cursor is at the end of a
        # line. Our implementation will kill the newline character, which is in
        # line with our expectations from using emacs.
        (
            ('k', 'control'),
            [
                ('e', ['control', 'shift']),
                ('delete_forward')
            ]
        ),
        # Copy.
        (
            ('w', 'option'),
            ('c', ['left_command'])
        ),
        # Paste.
        (
            ('y', 'control'),
            ('v', ['left_command']),
        ),
        # Undo / redo.
        (
            ('slash', 'control'),
            ('z', ['left_command'])
        ),
        (
            ('slash', ['control', 'option']),
            ('z', ['left_command', 'left_shift'])
        ),
        # Move cursor to first non-whitespace character online.
        (
            ('m', 'option'),
            [
                ('left_arrow', 'left_command'),
                ('right_arrow', 'left_option'),
                ('left_arrow', 'left_option')
            ]
        ),
        # Move cursor to top of file.
        (
            ('comma', ['option', 'shift']),
            ('up_arrow', ['left_command'])
        ),
        # Move cursor to bottom of file.
        (
            ('period', ['option', 'shift']),
            ('down_arrow', ['left_command'])
        ),
        # Open new line below the current line.
        (
            ('o', 'control'),
            [
                ('e', ['left_control']),
                ('return_or_enter')
            ]
        ),
        # Open new line above the current line.
        (
            ('o', ['control', 'option']),
            [
                ('a', ['left_control']),
                ('return_or_enter'),
                ('up_arrow'),
            ]
        ),
        # Cursor up 5 lines.
        (
            ('p', 'option'),
            [('up_arrow') for i in range(5)]
        ),
        # Cursor down 5 lines.
        (
            ('n', 'option'),
            [('down_arrow') for i in range(5)]
        ),
        # Scroll page up.
        (
            ('p', ['control', 'option']),
            {
                'mouse_key': {'vertical_wheel': -64}
            }
        ),
        # Scroll page down.
        (
            ('n', ['control', 'option']),
            {
                'mouse_key': {'vertical_wheel': 64}
            }
        ),
    ]
}

vscode_rule = {
    'description': 'VSCode rules',
    'conditions': [
        {
            'type': 'frontmost_application_if',
            'bundle_identifiers': [
                'com.microsoft.VSCode',
            ]
        }
    ],
    'manipulators': [
        # Cursor up 5 lines.
        (
            ('p', 'option'),
            [('up_arrow') for i in range(5)]
        ),
        # Cursor down 5 lines.
        (
            ('n', 'option'),
            [('down_arrow') for i in range(5)]
        ),
    ]
}

application_mappings = {
    'grave_accent_and_tilde': 'Finder.app',
    'return_or_enter': 'iTerm.app',
    'e': 'Emacs.app',
    'w': 'Google Chrome.app',
    's': 'MacPass.app',
    'h': 'Messages.app',
    'o': 'Notes.app',
    'c': 'Visual Studio Code.app',
}
application_launchers = {
    'description': 'Application launchers',
    'manipulators': [
        (
            (key_code, HYPER),
            {
                'shell_command': f'open -a "{app_name}"'
            }
        )
        for key_code, app_name in application_mappings.items()
    ]
}

option_based_window_switching = {
    'description': 'Option + Tab / Backtick -> switch windows / apps',
    'manipulators': [
        (
            ('tab', 'option'),
            ('tab', 'left_command'),
        ),
        (
            ('grave_accent_and_tilde', 'option'),
            ('grave_accent_and_tilde', 'left_command'),
        )
    ]
}

finder_remappings = {
    'description': 'Finder: F2 to rename, Return to Open',
    'conditions': [
        {
            'type': 'frontmost_application_if',
            'bundle_identifiers': [
                'com.apple.finder'
            ]
        }
    ],
    'manipulators': [
        (
            ('f2'),
            ('return_or_enter')
        ),
        # On the Microsoft Sculpt keyboard, the F2 action key actually performs
        # Hyper + Tab for some reason. Unclear why.
        (
            ('tab', HYPER),
            ('return_or_enter')
        ),
        # On the Apple keyboard, intercept the brightness up key.
        (
            ('display_brightness_increment'),
            ('return_or_enter')
        ),
        (
            ('return_or_enter'),
            ('o', 'left_command')
        )
    ]
}

# Emulate Linux-style "Hyper + Click" for moving/resizing. To be used in
# combination with Zooom2. Note that we use Fn as the movement modifier key for
# Zooom2 because it's the only one that doesn't come with any default
# functionality when combined with left-click. We need to add the following
# settings to Zooom2:
#
#   - Movement shortcut: Fn
#   - Resize shortcut: Fn + Shift
zooom2_remappings = {
    'description': 'Zooom2 support for Hyper + Click to move / resize windows with Zooom/2.',
    'manipulators': [
        (
            {
                'pointing_button': 'button1',
                'modifiers': {
                    'mandatory': HYPER
                }
            },
            {
                'pointing_button': 'button1',
                'modifiers': ['fn']
            }
        ),
        (
            {
                'pointing_button': 'button2',
                'modifiers': {
                    'mandatory': HYPER
                }
            },
            {
                'pointing_button': 'button2',
                'modifiers': ['fn', 'left_shift']
            }
        )
    ]
}

# Map Hyper + Comma/Period to Left/Right Brackets.  By default the Hyper +
# Comma/Period keyboard shortcuts should move focus to different screens (a la
# AwesomeWM, DWM, etc.). Additionally, we want Shift + Hyper + Comma/Period to
# move windows across different screens, but this is activates sysdiagnose by
# default. We can't disable this binding, so we need map the Hyper +
# Comma/Period shortcuts to something else, and in our case we choose to use the
# bracket keys. In Hammerspoon, we then need to create mappings for:
#
#   - Hyper + ]/[ -> Move focus between screens
#   - Hyper + Shift -> ]/[ -> Move windows between screens
hammerspoon_remappings = {
    'description': 'Hammerspoon: Hyper + Comma/Period to Left/Right Brackets',
    'manipulators': [
        (
            ('comma', HYPER, 'shift'),
            ('open_bracket', ['left_command', 'left_option', 'left_control'])
        ),
        (
            ('period', HYPER, 'shift'),
            ('close_bracket', ['left_command', 'left_option', 'left_control'])
        )
    ]
}
