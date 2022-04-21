import contextlib
import io
from functools import cached_property
from typing import List, Optional
import jedi
import ast
import hashlib
import base64

from .helpers import Plugin


class Evaluator(Plugin):
    provide = ["describe_globals", "describe_locals", "run", "flush", "create_buffer"]
    ns_prefix = "eval/"

    def __init__(self) -> None:
        self.locals = {}
        self.globals = {}

    def describe_globals(self):
        return {k: str(v) for (k, v) in self.globals}

    def describe_locals(self):
        return {k: str(v) for (k, v) in self.locals}

    def eval(self, source, as_str=False):
        with io.StringIO() as sio, contextlib.redirect_stdout(sio):
            res = eval(source, self.globals, self.locals)
            return (res, sio.getvalue())

    def exec(self, source):
        with io.StringIO() as sio, contextlib.redirect_stdout(sio):
            exec(source, self.globals, self.locals)
            return sio.getvalue()

    def run(self, code, as_str=False):
        results = []
        for stmt in ast.parse(code).body:
            if isinstance(stmt, ast.Expr):
                results.append(self.eval(ast.unparse(stmt), as_str=as_str))
            else:
                self.exec(ast.unparse(stmt))
        return results

    def flush(self):
        self.locals = {}
        self.globals = {}

    def create_buffer(self) -> "EvaluatorBuffer":
        evl = EvaluatorBuffer(parent=self)
        return evl


class EvaluatorBuffer:
    def __init__(self, parent: Evaluator) -> None:
        self.parent = parent
        self.current_buffer = ""
        self._state: Optional[jedi.Interpreter] = None

    def get_buffer_contents(self):
        return self.current_buffer

    def get_hash_state(self):
        sha = hashlib.sha1()
        sha.update(self.current_buffer.encode())
        return sha.digest().hex()

    def sync(self, code: str):
        self._state = None
        self.current_buffer = code

    def apply_edit(self, start, end, plen, chars=""):
        self._state = None
        pre, post = self.current_buffer[:start], self.current_buffer[end + plen :]
        self.current_buffer = pre + chars + post

    def get_state(self):
        if st := self._state:
            return st
        st = jedi.Interpreter(
            self.current_buffer, [self.parent.locals, self.parent.globals]
        )
        self._state = st
        return st

    def complete_at(self, line, column, hash: Optional[str] = None, fuzzy=True):
        if hash and self.get_hash_state() != hash:
            return
        state = self.get_state()
        completions = state.complete(line=line, column=column, fuzzy=fuzzy)
        return [(c.name_with_symbols, c.type) for c in completions]

    def complete_in(self, code: str, line, column, fuzzy=True):
        state = jedi.Interpreter(
            self.current_buffer, [self.parent.locals, self.parent.globals]
        )
        completions = state.complete(line=line, column=column, fuzzy=fuzzy)
        return [(c.name_with_symbols, c.type) for c in completions]
