import importlib
import ast
import json
from typing import Any, Optional
import contextlib
import io

from .endpoints import Endpoint, export


def eval_never_printing(code, glob, local):
    with (
        io.StringIO() as dev,
        contextlib.redirect_stdout(dev),
        contextlib.redirect_stderr(dev),
    ):
        return (eval(code, glob, local), dev.getvalue())


def exec_never_printing(code, glob, local):
    with (
        io.StringIO() as dev,
        contextlib.redirect_stdout(dev),
        contextlib.redirect_stderr(dev),
    ):
        exec(code, glob, local)
        return dev.getvalue()

def run_never_printing(code, glob, loc):
    results = []
    stdo = ""
    for stmt in ast.parse(code).body:
        if isinstance(stmt, ast.Expr):
            res, pstr = eval_never_printing(ast.unparse(stmt), glob, loc)
            results.append(
                res
            )
            stdo += pstr
        else:
            stdo += exec_never_printing(ast.unparse(stmt), glob, loc)
    return (results, stdo)

class World(Endpoint, name="world"):
    def __init__(self) -> None:
        self.globals = {}
        self.locals = {}
        self._reg = {}

    def run(self, code):
        results = []
        for stmt in ast.parse(code).body:
            if isinstance(stmt, ast.Expr):
                results.append(
                    eval_never_printing(ast.unparse(stmt), self.globals, self.locals)
                )
            else:
                exec_never_printing(ast.unparse(stmt), self.globals, self.locals)
        return results

    def run_file(self, pth):
        with open(pth) as file:
            return self.run(file)

    def lexical_set(self, key, value, glob):
        if glob:
            self.globals[key] = value
        else:
            self.locals[key] = value

    def lexical_scope(self, key) -> Optional[Any]:
        return self.locals.get(key, self.globals.get(key, None))

    @export
    def flush(self, everything: bool):
        self.locals = {}
        if everything:
            self.flush_refs()

    @export
    def import_module(self, module, reload=False):
        if reload:
            mod = importlib.reload(module)
            self.globals[mod.__name__] = mod
        else:
            mod = importlib.import_module(module)
            self.globals[mod.__name__] = mod
        return mod

    def make_ref(self, obj: Any):
        oid = id(obj)
        self._reg[oid] = obj
        return oid

    def resolve(self, id: int) -> Optional[Any]:
        return self._reg.get(id, None)

    def flush_refs(self):
        self._reg = {}

    @export
    def remove_ref(self, obj):
        oid = id(obj)
        del self._reg[oid]

    @export
    def count_refs(self):
        return len(self._reg.items())

    def encode(self, o):
        return json.dumps(o, default=self._default_marshal)

    def decode(self, o: str | bytes):
        return json.loads(o, object_hook=self._decode_hook)

    def _decode_hook(self, dct):
        if "$ref" in dct:
            return self.resolve(dct["$ref"])
        elif "$evl" in dct:
            return eval_never_printing(dct["$evl"], self.globals, self.locals)
        elif "$" in dct:
            return self.lexical_scope(dct["$"])
        return dct

    def _default_marshal(self, o):
        return {"$ref": self.make_ref(o)}
