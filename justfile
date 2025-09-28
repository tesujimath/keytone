build:
    #!/usr/bin/env elvish
    clojure -M:run-m
    put build/*.typ | each {|file| typst c $file}

test:
    clojure -M:test

