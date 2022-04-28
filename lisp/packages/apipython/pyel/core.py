import ast
from operator import attrgetter, methodcaller
from typing import Callable, Optional, Any
import inspect
from .world import World
from .to_el import create_el


class ApiCore:
    def __init__(self, world: World) -> None:
        self.world = world

    def attr(self, attr, this, set=None, setnone=False):
        if set:
            setattr(this, attr, set)
            return
        if setnone:
            setattr(this, attr, None)
            return
        else:
            return getattr(this, attr)

    def method(self, method, this, args = [], kwargs = {}):
        clr = methodcaller(method, *args, **kwargs)
        return clr(this)

    def var(self, var: str, set: Optional[Any] = None, glob=False):
        if set:
            self.world.lexical_set(var, set, glob)
            return
        return self.world.lexical_scope(var)

    def call(self, call: str, args, kwargs):
        if f := self.world.lexical_scope(call):
            print(f)
            if callable(f):
                return f(*args, **kwargs)

    def dir(self, it: Any):
        return dir(it)

    def scope(self):
        return list(self.world.globals.keys()) + list(self.world.locals.keys())

    def echo(self, it: Any):
        return it

    def import_module(self, module: str, reload=False):
        return self.world.import_module(module, reload)

    def help(self, describe: Optional[str] = None):
        if describe:
            atr = getattr(self, describe)
            return str(inspect.signature(atr))
        return list(
            filter(
                lambda x: not x.startswith("_") and callable(getattr(self, x)),
                dir(self),
            )
        )

    def run_file(self, file):
        self.world.run_file(file)

    def run(self, code):
        self.world.run(code)

    def make_el(self):
        return create_el(self)
