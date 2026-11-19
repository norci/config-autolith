(:VERSION 1
 :SERVERS
 ((:NAME "gopls"
   :COMMAND "gopls"
   :LANGUAGE-ID "go"
   :EXTENSIONS (".go")
   :ROOT-MARKERS ("go.mod"))
  (:NAME "pyright"
   :COMMAND "pyright-langserver"
   :ARGUMENTS ("--stdio")
   :LANGUAGE-ID "python"
   :EXTENSIONS (".py" ".pyi")
   :ROOT-MARKERS ("pyproject.toml" "setup.py" "requirements.txt"))
  (:NAME "typescript"
   :COMMAND "typescript-language-server"
   :ARGUMENTS ("--stdio")
   :LANGUAGE-ID "typescript"
   :EXTENSIONS (".ts" ".tsx")
   :ROOT-MARKERS ("package.json" "tsconfig.json"))
  (:NAME "javascript"
   :COMMAND "typescript-language-server"
   :ARGUMENTS ("--stdio")
   :LANGUAGE-ID "javascript"
   :EXTENSIONS (".js" ".jsx")
   :ROOT-MARKERS ("package.json" "jsconfig.json"))
  (:NAME "rust-analyzer"
   :COMMAND "rust-analyzer"
   :LANGUAGE-ID "rust"
   :EXTENSIONS (".rs")
   :ROOT-MARKERS ("Cargo.toml"))))
