# Karabiner Elements config compiler

## How to add / modify a rule

1. Find the rule you'd like to modify in `rules.py`, or add a new rule section. If you're adding a new rule, also be sure to add it to `compile_config.py`.
2. Compile the new config file be calling `make` in this directory.
3. Open Karabiner Elements. If you're modifying a rules, go to "Complex Modifications" and remove the existing rule.
4. Restart Karabiner Elements by going to "Misc" -> "Restart Karabiner-Elements".
5. Add the new / updated rule by going to "Complex Modifications" -> "Add rule" -> click "Enable" next to the relevant rule.
