---
name: dataflow-graph
description: 从源码生成调用图与数据流图。要回答「谁调谁」「数据从哪到哪」时用。
---

# 图由程序算，不由人读

## 判据

- 要回答调用关系或数据流向，先跑 `references/graph.py` 出图，再读图。逐函数读源码手工推理出来的调用关系不作数。
- 图必须可重跑：模块清单、入口、产物路径都是脚本参数；判据在脚本里求值并随结果返回。
- 边是调用边，标签是调用点实参原文。实参不等于数据依赖：函数可能只取实参的形状（日轴、列名、行数）而丢弃其值。这类语义读函数体，不从图上推断。
- 图不完整时先看脚本返回的未解析调用计数，补解析规则；不要退回手工读。

## 两种画法，按模块分

| 模块特征 | 画法 |
| --- | --- |
| 无 `from hamilton import` | `ast` 解析，调用边加实参标签 |
| 有 `from hamilton import` | `DRIVER.visualize_execution(targets, path, {}, bypass_validation=True)` 出 dot，再转 mermaid |

判据：`grep -l "from hamilton" --include=*.py` 的结果决定该文件归哪一档。

## 步骤

1. 列要追溯的模块 → `--file`（仓根相对路径，可多次）。
2. 定入口函数 → `--entry 模块::类.方法`（可多次）；脚本只保留入口可达的子图。
3. 跑 `python graph.py --root <仓根> --out <目录> --entry ... --file ...`。
4. 读返回的 JSON：`edges`、`unresolved`、`missing`、`nodes`。`missing` 非空即关键边没连上，先修解析器。
5. 打开 `<out>/ast.mmd` 与 `<out>/hamilton.mmd` 看结构。

## 坑

写进解析器的判据：

- `import a.b` 绑定的名字是 `a`，不是 `a.b`。import 表按首段建，否则 `a.b.f()` 解成 `a.b.b.f`，边静默消失。
- 方法定义键要带类前缀（`模块::类.方法`），调用方的键也带。两端前缀不一致时边数为 0，而脚本不报错。
- 嵌套属性调用（`a.b.c(...)`）先按首段查 import 表再拼余下各段；只处理单层属性会漏边。
- 不带 `bypass_validation=True`，Hamilton 会因缺外部输入直接抛错，图出不来。
- Hamilton 的 dot 行带 `[pos="..."]` 属性，解析时每半截到 `[` 之前，否则节点名里混进坐标。
- 自检：用已知能连通的入口跑一次，边数为 0 就是解析器坏了，不是代码没有调用。

## 产物

分析读判据，不读图。

- 脚本末行 JSON：`ast_edges`、`hamilton_edges`、`missing_entries`、`unresolved_calls`。数边、找路径、比版本都用它。
- `<out>/ast.mmd`、`<out>/hamilton.mmd`：一行一条边，用于展示给人看。
- `<out>/hamilton.dot`：Graphviz 原始文件，行尾带 `[pos="..."]`，不直接读。

判据：需要用图来做的判断由脚本求值并回 JSON；只有把结构展示给人看才打开 `.mmd`。

脚本：`references/graph.py`。
