from json.encoder import JSONEncoder
from typing import Dict, Any, Optional
from .helpers import Plugin

class Registry(JSONEncoder, Plugin):
    provide = ["flush_refs", "count_refs", "remove_ref"]
    ns_prefix = "ref/"

    def flush_refs(self):
        self._reg = {}

    def remove_ref(self, obj):
        oid = id(obj)
        del(self._reg[oid])
        return f"Removed reference {oid}"

    def count_refs(self):
        return len(self._reg.items())

    def __init__(self) -> None:
        super().__init__()
        self._counter = 0
        self._reg: Dict[int, Any] = {}

    def make_ref(self, obj: Any): 
        oid = id(obj)
        self._reg[oid] = obj
        return oid

    def resolve(self, id: int) -> Optional[Any]: 
        return self._reg.get(id, None)

    def decode_hook(self, dct):
        if '$ref' in dct:
            return self.resolve(dct['$ref'])
        return dct

    def default(self, o):
        try:
            return JSONEncoder.default(self, o)
        except TypeError:
            return {'$ref': self.make_ref(o)}

