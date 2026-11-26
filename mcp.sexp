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
			 :REQUIRED-P NIL
			 :DIRECTORY :WORKSPACE))
	   (:NAME "hy-repl" :TRANSPORT
		  (:TYPE :STDIO
			 :COMMAND "uvx"
			 :ARGUMENTS
			 ("--from" "/home/josephx/code/hy-repl-mcp" "hy-repl-mcp")
			 :DIRECTORY :WORKSPACE)
		  :REQUIRED-P NIL
		  :APPROVAL :PROMPT
		  :STARTUP-TIMEOUT-SECONDS 30
		  :TOOL-TIMEOUT-SECONDS 300)
	   (:NAME "lightpanda" :TRANSPORT
		  (:TYPE :STDIO
			 :COMMAND "uvx"
			 :ARGUMENTS ("--from" "lightpanda" "lightpanda" "mcp")
			 :DIRECTORY :WORKSPACE)
		  :REQUIRED-P NIL
		  :APPROVAL :PROMPT
		  :STARTUP-TIMEOUT-SECONDS 60
		  :TOOL-TIMEOUT-SECONDS 300)))
