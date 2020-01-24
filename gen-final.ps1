.\gen.ps1   -SampleSize 100 `
            -Options "-c max_consumes=10", `
                     "-c max_produces=10", `
                     "-c min_cost=2", `
                     "--rand-freq=0.05" `
            -Files  .\instance-generation\instance-generator.lp, `
                    .\instance-generation\final-instance-constraints.lp `
            -OutFile .\final-instance.lp
