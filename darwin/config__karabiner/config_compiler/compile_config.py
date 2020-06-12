#!/usr/bin/env python

import json

import helpers
import rules

rule_defs = [
    rules.non_apple_modifiers_rule,
    rules.apple_modifiers_rule,
    rules.remap_caps_lock_and_control_rule,
    rules.pause_to_eject_rule,
    rules.mouse_bindings_rule,
    rules.control_based_shortcuts_rule,
    rules.page_up_down_home_end_rule,
    rules.pc_home_end_rule,
    rules.switch_tabs_rule,
    rules.macpass_rule,
    rules.emacs_rule,
    rules.vscode_rule,
    rules.application_launchers,
    rules.option_based_window_switching,
    rules.finder_remappings,
    rules.hammerspoon_remappings,
]

config = {
    'title': 'Custom configuration',
    'rules': [helpers.build_rule(rule) for rule in rule_defs],
}

print(json.dumps(config, indent=2))
