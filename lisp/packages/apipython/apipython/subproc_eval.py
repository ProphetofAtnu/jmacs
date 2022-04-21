import contextlib
import io
import multiprocessing as mp
from multiprocessing.managers import BaseManager
from typing import Optional, Type
import jedi
import hashlib
from .eval import Evaluator

class SubprocessEvaluatorBuffer(Evaluator):
    def __init__(self) -> None:
        super().__init__()
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

    def _get_state(self):
        if st := self._state:
            return st
        st = jedi.Interpreter(self.current_buffer, [self.locals, self.globals])
        self._state = st
        return st

    def complete_at(self, line, column, hash: Optional[str]=None, fuzzy=True):
        if hash and self.get_hash_state() != hash:
            return 
        state = self._get_state()
        completions = state.complete(line=line, column=column, fuzzy=fuzzy)
        return [(c.name_with_symbols, c.type) for c in completions]

    def complete_in(self, code: str, line, column, fuzzy=True):
        state = jedi.Interpreter(self.current_buffer, [self.locals, self.globals])
        completions = state.complete(line=line, column=column, fuzzy=fuzzy)
        return [(c.name_with_symbols, c.type) for c in completions]
        
class SubprocessEvaluator(BaseManager):
    SubprocessEvaluatorBuffer: Type[SubprocessEvaluatorBuffer]

    def isolated_buffer(self):
        buffer = self.SubprocessEvaluatorBuffer()
        return buffer
        
SubprocessEvaluator.register("SubprocessEvaluatorBuffer", SubprocessEvaluatorBuffer)

def create_isolate():
    iso = SubprocessEvaluator()
    iso.start()
    return iso
