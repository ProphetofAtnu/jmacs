import inspect

from .helpers import Plugin

class Inspector(Plugin):
    def describe(self, obj):
        members = inspect.getmembers(obj)
        type_info = repr(type(obj))
        props = []
        methods = []

        for (name, val) in members:
            if callable(val):
                methods.append(name)
            else:
                props.append(name)
        return dict(type=type_info, props=props, methods=methods)

