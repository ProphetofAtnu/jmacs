import inspect
from typing import Callable
from inspect import signature


def defun_template(func: Callable):
    name = func.__name__
    emacs_name = name.replace("_", "-")

    args = signature(func).parameters

    required = [
        k for (k, v) in args.items() if v.default is v.empty and v.name != "self"
    ]
    nonreq = [k for (k, v) in args.items() if not v.default is v.empty]

    rf = " ".join(required)
    nreq = " ".join(nonreq)

    req_cns = [f'(cons "{r}" {r})' for r in required]
    nreq_cns = [f'(when {r} (cons "{r}" {r}))' for r in nonreq]

    optargs = " &optional " + nreq
    brk = "\n"

    return f"""
    (defun pyel-core-{emacs_name} ({rf}{optargs if len(nonreq) > 0 else ""})
        (cl-remove-if-not #'identity (list (cons :do "{name}") {brk.join(req_cns)} {brk.join(nreq_cns)})))
    """


def create_el(from_thing):
    funs = inspect.getmembers(from_thing)
    return "\n".join(
        [defun_template(f) for (n, f) in funs if not n.startswith("_") and callable(f)]
    )
