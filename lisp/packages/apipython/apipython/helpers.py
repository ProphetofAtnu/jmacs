from typing import Callable, ClassVar, List, Awaitable, Any, Optional

HandlerFunc = Callable[..., Awaitable[Any] | Any]

class Plugin:
    _provides: List[str] = []
    provide: ClassVar[Optional[List[str]]] = None
    ns_prefix: ClassVar[str] = ""

    def provides(self):
        cls = self.__class__
        pre = cls.ns_prefix
        return [(pre + p, getattr(self, p)) for p in cls._provides]

    def __init_subclass__(cls) -> None:
        if getattr(cls, "provide"):
            cls._provides = getattr(cls, "provide")
        else:
            cls._provides = [m for m in 
                            dir(cls)
                            if not m.startswith("_")
                            and m != 'plugin'
                            and callable(getattr(cls, m))]
