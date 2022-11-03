import jedi
from jedi.api.classes import Completion
from typing import Dict, Any, List
from .endpoints import Endpoint, NoHandlerException, export
from .world import run_never_printing

class Code(Endpoint, name="code"):
    def __init__(self) -> None:
        self.globals = {}
        self.locals = {}
        self.last_completion_data: List[Completion] = []

    @export
    def reset(self):
        self.globals = {}
        self.locals = {}

    @export
    def ref_to(self, key):
        self.locals.get(key, self.globals.get(key, None))

    @export
    def run(self, code):
        res, stdo = run_never_printing(code, self.globals, self.locals)
        return ([str(r) for r in res], stdo)

    @export
    def capf(self, code, line, col):
        self.last_completion_data = jedi.Interpreter(code,
                                                     [self.globals, self.locals]).complete(line, col)
        rl = []
        for comp in self.last_completion_data:
            rl.append(
                (comp.name, comp.type)
            )
        return rl
