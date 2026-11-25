(in-package #:autolith)

(register-openai-compatible-provider
 :name "tokenhub"
 :description "TokenHub"
 :endpoint "https://tokenhub.tencentmaas.com/v1/chat/completions"
 :api-key-environment-variable "TOKENHUB_API_KEY"
 :reasoning-parameter "reasoning_effort"
 :models '((:name "hy4"
            :description "Hunyuan hy4"
            :context-window 1048576
            :reasoning-efforts ("none" "low" "medium" "high"))
           (:name "hy4-flash"
            :description "Hunyuan hy4 flash"
            :context-window 1048576
            :reasoning-efforts ("none" "low" "medium" "high"))))
