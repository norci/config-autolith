"""调用图与数据流图：普通 Python 走 ast，Hamilton 模块走 driver 可视化。

用法:
  python graph.py --root REPO --out DIR \
      --entry pkg.mod::Class.method [--entry ...] \
      --file pkg/mod.py [--file ...] \
      [--hamilton-module pkg.flow] [--hamilton-var DRIVER] \
      [--hamilton-target name ...]

产物: <out>/ast.mmd、<out>/hamilton.dot、<out>/hamilton.mmd
判据: 末行打印 JSON（ast_edges、hamilton_edges、missing、unresolved）
"""
import argparse
import ast
import json
import pathlib
import sys


def module_of(rel):
    return rel[:-3].replace("/", ".")


def collect_defs(tree, mod, defs):
    def walk(node, cls):
        if isinstance(node, ast.ClassDef):
            for c in node.body:
                walk(c, node.name)
            return
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            defs[mod + "::" + (cls + "." if cls else "") + node.name] = (mod, node)
            for c in node.body:
                walk(c, cls)
            return
        for c in ast.iter_child_nodes(node):
            walk(c, cls)

    walk(tree, None)


def collect_imports(tree):
    imap = {}
    for node in ast.walk(tree):
        if isinstance(node, ast.ImportFrom) and node.module:
            for a in node.names:
                imap[a.asname or a.name] = node.module + "." + a.name
        elif isinstance(node, ast.Import):
            for a in node.names:
                if a.asname:
                    imap[a.asname] = a.name
                else:
                    head = a.name.split(".")[0]
                    imap[head] = head
                    imap[a.name] = a.name
    return imap


def calls_in(node):
    for child in ast.iter_child_nodes(node):
        if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Lambda)):
            continue
        if isinstance(child, ast.Call):
            yield child
        yield from calls_in(child)


def make_resolver(defs, imports, by_dotted):
    def resolve(func, mod, cls):
        if isinstance(func, ast.Name):
            if func.id in imports.get(mod, {}):
                return by_dotted.get(imports[mod][func.id])
            for q in (mod + "::" + (cls + "." if cls else "") + func.id, mod + "::" + func.id):
                if q in defs:
                    return q
            return None
        if isinstance(func, ast.Attribute):
            parts = ast.unparse(func).split(".")
            if parts[0] == "self" and cls:
                q = mod + "::" + cls + "." + ".".join(parts[1:])
                return q if q in defs else None
            if parts[0] in imports.get(mod, {}):
                return by_dotted.get(imports[mod][parts[0]] + "." + ".".join(parts[1:]))
        return None

    return resolve


def short(qual):
    mod, tail = qual.split("::", 1)
    return mod.split(".")[-1] + "." + tail


def ast_graph(root, files, entries):
    defs, imports = {}, {}
    for rel in files:
        path = root / rel
        if not path.exists():
            continue
        tree = ast.parse(path.read_text(encoding="utf-8"))
        mod = module_of(rel)
        imports[mod] = collect_imports(tree)
        collect_defs(tree, mod, defs)

    by_dotted = {q.replace("::", "."): q for q in defs}
    resolve = make_resolver(defs, imports, by_dotted)
    edges, unresolved = set(), []
    for qual, (mod, node) in defs.items():
        tail = qual.split("::", 1)[1]
        cls = tail.rsplit(".", 1)[0] if "." in tail else None
        for call in calls_in(node):
            target = resolve(call.func, mod, cls)
            if target:
                edges.add((qual, target, ", ".join(ast.unparse(a) for a in call.args)))
            else:
                unresolved.append(ast.unparse(call.func))

    missing = [e for e in entries if e not in defs]
    seen, frontier = set(entries) & set(defs), list(set(entries) & set(defs))
    while frontier:
        cur = frontier.pop()
        for c, t, _ in edges:
            if c == cur and t not in seen:
                seen.add(t)
                frontier.append(t)
    sub = sorted((c, t, a) for c, t, a in edges if c in seen and t in seen)
    mmd = "\n".join(["flowchart TD"] + [f'  {short(c)} -->|"{a}"| {short(t)}' for c, t, a in sub])
    return mmd, len(sub), missing, len(unresolved)


def hamilton_graph(root, module, var, targets, out):
    sys.path.insert(0, str(root))
    mod = __import__(module, fromlist=["*"])
    driver = getattr(mod, var)
    names = [v.name for v in driver.list_available_variables()]
    chosen = [t for t in targets if t in names]
    dot_path = out / "hamilton.dot"
    driver.visualize_execution(chosen, str(dot_path), {}, bypass_validation=True)
    pairs = []
    for line in dot_path.read_text(encoding="utf-8").splitlines():
        if "->" not in line:
            continue
        left, right = line.split("->", 1)
        clean = lambda s: s.strip().split("[")[0].strip().strip(";").strip('"').strip()
        pairs.append((clean(left), clean(right)))
    mmd = "\n".join(["flowchart LR"] + [f"  {a} --> {b}" for a, b in pairs])
    return mmd, len(pairs), chosen


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", required=True)
    parser.add_argument("--out", required=True)
    parser.add_argument("--entry", action="append", default=[])
    parser.add_argument("--file", action="append", default=[])
    parser.add_argument("--hamilton-module", default=None)
    parser.add_argument("--hamilton-var", default="DRIVER")
    parser.add_argument("--hamilton-target", action="append", default=[])
    args = parser.parse_args()

    root = pathlib.Path(args.root).resolve()
    out = pathlib.Path(args.out)
    out.mkdir(parents=True, exist_ok=True)

    ast_mmd, ast_edges, missing, unresolved = ast_graph(root, args.file, args.entry)
    (out / "ast.mmd").write_text(ast_mmd, encoding="utf-8")

    ham_edges, chosen, error = 0, [], None
    if args.hamilton_module:
        try:
            ham_mmd, ham_edges, chosen = hamilton_graph(
                root, args.hamilton_module, args.hamilton_var, args.hamilton_target, out
            )
            (out / "hamilton.mmd").write_text(ham_mmd, encoding="utf-8")
        except Exception as exc:
            error = f"{type(exc).__name__}: {exc}"

    print(json.dumps({
        "ast_edges": ast_edges,
        "hamilton_edges": ham_edges,
        "hamilton_targets": chosen,
        "missing_entries": missing,
        "unresolved_calls": unresolved,
        "ast_mmd": (out / "ast.mmd").exists(),
        "hamilton_mmd": (out / "hamilton.mmd").exists(),
        "hamilton_error": error,
    }, ensure_ascii=False))


if __name__ == "__main__":
    main()
