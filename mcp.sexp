(:version 1
	  :servers
	  ((:name "agent-lsp"
		  :transport
		  (:type      :stdio
			      :command   "bunx"
			      :arguments ("@blackwell-systems/agent-lsp"
          				  "go:gopls"
					  "javascript:typescript-language-server,--stdio"
					  "python:pyright-langserver,--stdio"
					  "rust:rust-analyzer"
					  "typescript:typescript-language-server,--stdio")
			      :directory :workspace))
	   (:name "crw"
		  :transport (:type :http
				    :url  "http://127.0.0.1:3000/mcp"))
	   (:name "lightpanda"
		  :transport (:type :http
				    :url  "http://127.0.0.1:9223/mcp"))
	   (:name "gitee"
		  :transport (:type :http
				    :url     "https://api.gitee.com/mcp"
				    :headers (("Authorization" :environment "GITEE_TOKEN"))))))
