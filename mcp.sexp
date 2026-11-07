(:VERSION 1 :SERVERS
 ((:NAME "agent-lsp" :TRANSPORT
   (:TYPE :STDIO
    :COMMAND "bunx"
    :ARGUMENTS
    ("@blackwell-systems/agent-lsp"
     "lisp:cl-lsp"
     "go:gopls"
     "javascript:bunx,typescript-language-server,--stdio"
     "python:bunx,-p,pyright,pyright-langserver,--stdio"
     "rust:rust-analyzer"
     "typescript:bunx,typescript-language-server,--stdio")
    :DIRECTORY :WORKSPACE))
  (:NAME "hamilton" :TRANSPORT
    (:TYPE :STDIO :COMMAND "uvx" :ARGUMENTS ("--from" "apache-hamilton[mcp]" "--with" "graphviz" "--with" "httpx[socks]" "hamilton-mcp")))
  (:NAME "crw" :TRANSPORT
    (:TYPE :STDIO :COMMAND "bunx" :ARGUMENTS ("crw-mcp@latest")))
))
