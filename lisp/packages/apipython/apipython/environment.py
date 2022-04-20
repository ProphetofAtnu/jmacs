import importlib
import sys

from .helpers import Plugin

class Environment(Plugin):
    ns_prefix = "env/"

    def __init__(self) -> None:
        pass

    def get_imported(self):
        return [*sys.modules.keys()]

    def import_module(self, *args, **kwargs):
        return importlib.import_module(*args, **kwargs)
