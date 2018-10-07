hyper_key = ['left_command', 'left_option', 'left_control']

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
        'Control_L / Fn_L -> Caps Lock when tapped'
    ]),
    'manipulators': [
        # Caps lock to Control_L.
        (
            ('caps_lock', None, 'any'),
            ('left_control')
        ),

        # Control_L -> Command_L.
        {
            'from': ('left_control', None, 'any'),
            'to': ('left_command'),
            'to_if_alone': [
                {
                    'hold_down_milliseconds': 100,
                    'key_code': 'caps_lock'
                }
            ]
        },

        # Fn -> Command_L.
        {
            'from': ('fn', None, 'any'),
            'to': ('left_command'),
            'to_if_alone': [
                {
                    'hold_down_milliseconds': 100,
                    'key_code': 'caps_lock'
                }
            ]
        },
    ]
}

mouse_bindings_rule = {
    'description': 'Mouse back button to Back (Forward with control)',
    'manipulators': [
        (
            # Define this `from` clause in its raw form.
            {
                'pointing_button': 'button4'
            },
            ('open_bracket', 'left_command')
        ),
        (
            # Define this `from` clause in its raw form.
            {
                'pointing_button': 'button4',
                'modifiers': {
                    'mandatory': [
                        'control'
                    ]
                }
            },
            ('close_bracket', 'left_command')
        )
    ]
}

control_based_shortcuts_rule = {
    'description': 'Control-based shortcuts',
    'conditions': [
        {
            'type': 'frontmost_application_unless',
            'bundle_identifiers': [
                '\\.Emacs$',
                '\\.iterm2$',
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
                '\\.finder$',
                '\\.iterm2$'
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
                '\\.Chrome$',
                '\\.iterm2$'
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

keepasx_rule = {
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
                {'shell_command': 'sleep 0.2'},
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

emacs_rule = {
    'description': 'Implements Emacs hotkeys',
    'conditions': [
        {
            'type': 'frontmost_application_unless',
            'bundle_identifiers': [
                '\\.Emacs$',
                '\\.iterm2$',
            ]
        }
    ],
    'manipulators': [
        (
            ('p', ['control']),
            ('up_arrow'),
        ),
        (
            ('n', ['control']),
            ('down_arrow'),
        ),
        (
            ('b', ['control']),
            ('left_arrow'),
        ),
        (
            ('b', ['option']),
            ('left_arrow', ['left_option'])
        ),
        (
            ('f', ['control']),
            ('right_arrow')
        ),
        (
            ('f', ['option']),
            ('right_arrow', ['left_option'])
        ),
        (
            ('a', ['control']),
            ('left_arrow', ['left_command'])
        ),
        (
            ('e', ['control']),
            ('right_arrow', ['left_command'])
        ),
        (
            ('w', ['control']),
            ('delete_or_backspace', ['left_option'])
        ),
        (
            ('w', ['option']),
            ('c', ['left_command'])
        ),
        (
            ('y', ['control']),
            ('v', ['left_command']),
        ),

        # Undo / redo.
        (
            ('slash', ['control']),
            ('z', ['left_command'])
        ),
        (
            ('slash', ['control', 'option']),
            ('z', ['left_command', 'left_shift'])
        ),

        # Move cursor to
        (
            ('m', ['option']),
            [
                ('left_arrow', 'left_command'),
                ('right_arrow', 'left_option'),
                ('left_arrow', 'left_option')
            ]
        )
    ]
}

application_mappings = {
    'grave_accent_and_tilde': 'Finder.app',
    'return_or_enter': 'iTerm.app',
    'e': 'Emacs.app',
    'w': 'Google Chrome.app',
    's': 'KeePassX.app',
    'h': 'Messages.app',
    'o': 'Notes.app',
}
application_launchers = {
    'description': 'Application launchers',
    'manipulators': [
        (
            (key_code, hyper_key),
            {
                'shell_command': f'open -a "{app_name}"'
            }
        )
        for key_code, app_name in application_mappings.items()
    ]
}

finder_remappings = {
    'description': 'Finder: F2 to rename, Return to Open',
    'conditions': [
        {
            'type': 'frontmost_application_if',
            'bundle_identifiers': [
                '\\.finder$'
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
            ('tab', hyper_key),
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
