from functools import partial
from pickle import DICT
from typing import Any, Callable, Dict, Mapping
from .transport import RpcCast
import inspect

export: Callable = lambda x: None

class NoHandlerException(Exception):
    pass


def _format_func(func: Callable):
    sig = inspect.signature(func)
    res = {}
    res['arguments'] = [*sig.parameters.keys()][1:]
    return res

class Dispatcher(type):
    child_exports = {}
    registered_classes_ctors = {}
    class_lut = {}
    instances_by_endpoint = {}

    @classmethod
    def describe_endpoints(cls):
        ltr = {v:k for (k, v) in cls.class_lut.items()}
        return {ltr.get(k, k):{ki:_format_func(vi)
                                  for (ki, vi) in v.items()}
                for (k, v) in cls.child_exports.items()}

    @classmethod
    def _export(cls, target, thing: Callable):
        cls.child_exports.setdefault(target,{})[thing.__name__] = thing

    @classmethod
    def __prepare__(metacls, __name: str, __bases: tuple[type, ...], **kwds: Any) -> Mapping[str, object]:
        return {'export': partial(Dispatcher._export, __name)}

    @classmethod
    def create_instance(cls, name):
        return cls.registered_classes_ctors[name]()

    @classmethod
    def get_instance(cls, name):
        return cls.instances_by_endpoint.setdefault(name, cls.create_instance(name))

    @classmethod
    def resolve(cls, name, method):
        target = cls.get_instance(name)
        can = cls.class_lut[name]
        return partial(cls.child_exports[can][method], target)

    @classmethod
    async def accept(cls, msg: RpcCast):
        try:
            ep = msg['endpoint']
            mname = msg['method']
            method = cls.resolve(ep, mname)
            return method(*msg.get("args", []), **msg.get("kwargs", {}))
        except KeyError:
            raise NoHandlerException(f"no handler for {str(msg)}")

class Endpoint(metaclass=Dispatcher):

    exports: Dict[str, Callable]

    def __init_subclass__(cls, name) -> None:
        Dispatcher.registered_classes_ctors[name] = cls
        Dispatcher.class_lut[name] = cls.__name__

class Meta(Endpoint, name='meta'):
    @export
    def help(self):
        return Dispatcher.describe_endpoints()
