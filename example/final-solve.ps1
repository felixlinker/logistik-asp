clingo --parallel-mode 8,split --time-limit=7200 --outf=1 -c max_steps=4 -c trucks=6 -c truck_capacity=3 .\example\final-instance.lp .\solve.lp `
    | Out-File -Encoding ascii .\example\final-result.lp
