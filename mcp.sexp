(:VERSION 1 :SERVERS
 ((:NAME "agent-lsp" :TRANSPORT
   (:TYPE :STDIO :COMMAND "bunx" :ARGUMENTS
    ("@blackwell-systems/agent-lsp" "go:gopls"
     "javascript:typescript-language-server,--stdio"
     "python:pyright-langserver,--stdio" "rust:rust-analyzer"
     "typescript:typescript-language-server,--stdio")
    :DIRECTORY :WORKSPACE))
  (:NAME "crw" :TRANSPORT (:TYPE :HTTP :URL "http://127.0.0.1:3000/mcp"))
  (:NAME "lightpanda" :TRANSPORT
   (:TYPE :HTTP :URL "http://127.0.0.1:9223/mcp"))
  (:NAME "gitee" :TRANSPORT
   (:TYPE :HTTP :URL "https://api.gitee.com/mcp" :HEADERS
    (("Authorization" :ENVIRONMENT "GITEE_TOKEN"))))))
