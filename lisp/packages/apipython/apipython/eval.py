from functools import cached_property
from typing import List, Optional
import jedi
import ast
import hashlib
import base64

from .helpers import Plugin

class Evaluator(Plugin):
    def __init__(self) -> None:
        self.locals = {}
        self.globals = {}

    def eval(self, source):
        eval(source, self.globals, self.locals)

    def exec(self, source):
        exec(source, self.globals, self.locals)

    def run(self, code):
        results = []
        for stmt in ast.parse(code).body:
            if isinstance(stmt, ast.Expr):
                results.append(self.eval(ast.unparse(stmt)))
            else:
                self.exec(ast.unparse(stmt))
        return results

    def flush(self):
        self.locals = {}
        self.globals = {}

    def create_buffer(self) -> 'EvaluatorBuffer':
        evl = EvaluatorBuffer(parent=self)
        return evl
        

class EvaluatorBuffer:

    def __init__(self, parent: Evaluator) -> None:
        self.parent = parent
        self.current_buffer = ""
        self._state: Optional[jedi.Interpreter]= None

    def get_buffer_contents(self):
        return self.current_buffer

    def get_hash_state(self):
        sha = hashlib.sha1()
        sha.update(self.current_buffer.encode())
        return sha.digest().hex()

    def sync(self, code: str):
        self._state = None
        self.current_buffer = code
        
    def apply_edit(self, start, end, plen, chars = ""):
        self._state = None
        pre, post = self.current_buffer[:start], self.current_buffer[end + plen:]
        self.current_buffer = pre + chars + post
        

    def get_state(self):
        if st := self._state:
            return st
        st = jedi.Interpreter(self.current_buffer, [self.parent.locals, self.parent.globals])
        self._state = st
        return st

    async def complete_at(self, hash: str, line, column, fuzzy=True):
        if self.get_hash_state() == hash:
            state = self.get_state()
            completions = state.complete(line=line, column=column, fuzzy=fuzzy)
            return [(c.name_with_symbols, c.type) for c in completions]
            
