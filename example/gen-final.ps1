.\gen.ps1   -SampleSize 100 `
            -Options "-c max_consumes=8", `
                     "-c max_produces=8", `
                     "-c min_cost=2", `
                     "-c factory_num=2", `
                     "-c warehouse_num=2", `
                     "-c resource_num=2", `
                     "-c product_num=2", `
                     "--rand-freq=0.05" `
            -Files  .\instance-generation\instance-generator.lp, `
                    .\example\final-instance-constraints.lp `
            -OutFile .\example\final-instance.lp
