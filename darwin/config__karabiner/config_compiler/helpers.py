def build_rule(rule_config):
    """Builds a rule dictionary from the given rule configuration."""
    return {
        'description': rule_config['description'],
        'manipulators': [
            build_manipulator(
                manipulator,
                conditions=rule_config.get('conditions'))
            for manipulator in rule_config['manipulators']
        ]
    }


def build_manipulator(manipulator_config, conditions=None):
    """Builds a manipulator dictionary from the given manipulator config.

    The manipulator config can either be specified as a tuple or a dictionary.

    If specified as as tuple, it should have the following format:

      (
        (<from key code>, [<mandatory modifiers>], [<optional modifiers>]),
        (<to key code>, [<modifiers>])
      )

    If the event should trigger multiple `to` events, then the second element in
    the tuple should be a list of tuples:

      (
        (<from key code>, [<mandatory modifiers>], [<optional modifiers>]),
        [
          (<to key code>, [<modifiers>]),
          (<to key code>, [<modifiers>])
        ]
      )

    Any of the modifiers can be omitted and this will still work fine.

    If specified as a dictionary, we expect the following format:

      {
        'from': (key code tuple),
        'to': (key code tuple),
        'to_if_alone': [{
          ...
        }],
        ...
      }
    """
    if isinstance(manipulator_config, tuple):
        from_config = manipulator_config[0]
        to_config = manipulator_config[1]
        other_configs = None
    else:
        from_config = manipulator_config.pop('from')
        to_config = manipulator_config.pop('to')
        other_configs = manipulator_config

    result = {
        'type': 'basic',
        'from': build_from_config(from_config),
        'to': build_to_config(to_config)
    }

    if other_configs:
        result.update(other_configs)

    if conditions:
        result['conditions'] = conditions

    return result


def build_from_config(config):
    """Returns a dictionary that can be used in the `from` section of the
    manipulator config.
    """
    # If the config is already a dict, then return it in its raw form.
    if isinstance(config, dict):
        return config

    key_code_tuple = normalize_key_code_tuple(config)

    result = {
        'key_code': key_code_tuple[0]
    }

    if key_code_tuple[1] or key_code_tuple[2]:
        result['modifiers'] = {}
    if key_code_tuple[1]:
        result['modifiers']['mandatory'] = key_code_tuple[1]
    if key_code_tuple[2]:
        result['modifiers']['optional'] = key_code_tuple[2]

    return result


def build_to_config(config):
    """Returns a list that can be used in the `to` section of the manipulator
    config.
    """
    # The `config` might not be defined as a list, so we should coerce it
    # into one.
    if not isinstance(config, list):
        config = [config]

    result = []
    for c in config:
        # If this is already defined as a dictionary, then take it in its raw
        # form.
        if isinstance(c, dict):
            result.append(c)
        else:
            tup = normalize_key_code_tuple(c)
            to_dict = {'key_code': tup[0]}
            if tup[1]:
                to_dict['modifiers'] = tup[1]
            result.append(to_dict)

    return result


def normalize_key_code_tuple(key_code_tuple, length=3):
    """Normalizes the given key code tuple.

    Converts the given key code tuple into a normal format that we can operate
    on. This lets us handle tuples that are defined without all modifier keys,
    or defined with modifier keys that aren't lists, etc.
    """
    if not isinstance(key_code_tuple, (list, tuple)):
        key_code_tuple = (key_code_tuple,)

    key_code_tuple = (key_code_tuple + (None,) * length)[:length]

    # Ensure all modifier keys are defined as lists.
    key_code = key_code_tuple[0]
    modifiers = [ensure_list(m) if m else None for m in key_code_tuple[1:]]

    return (key_code, *modifiers)


def ensure_list(val):
    """Ensures the given value is a list, wrapping it in a list if necessary."""
    if isinstance(val, list):
        return val
    elif isinstance(val, tuple):
        return list(val)
    else:
        return [val]
