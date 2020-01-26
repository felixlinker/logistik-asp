.\gen.ps1   -SampleSize 100 `
            -Options "-c max_consumes=10", `
                     "-c max_produces=10", `
                     "-c min_cost=2", `
                     "-c factory_num=3", `
                     "-c warehouse_num=3", `
                     "-c resource_num=3", `
                     "-c product_num=3", `
                     "--rand-freq=0.05" `
            -Files  .\instance-generation\instance-generator.lp, `
                    .\instance-generation\final-instance-constraints.lp `
            -OutFile .\final-instance.lp