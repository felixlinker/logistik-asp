.\gen.ps1   -SampleSize 100 `
            -Options "-c max_consumes=20", `
                     "-c max_produces=20" `
            -Files  .\instance-generation\instance-generator.lp, `
                    .\instance-generation\final-instance-constraints.lp `
            -OutFile .\final-instance.lp
