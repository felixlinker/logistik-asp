# Logistics Scheduler

This repository provides an answer set program that generates an optimal
schedule minimizing driving time between a set of warehouses and factories.
Warehouses provide resources and require products on a weekly basis whilst
factories require resources and provide products on a daily basis.

The program looks for a schedule that provides every place with what's required.

## Instance generation

There also is an answer set program that can be used to either generate random
instances of the problem or to validate manually crafted instances.
The `./instance-generation/instance-generator.lp` program outputs the facts:
* `day/1` denotes the days of the week
* `factory/1` denotes factories
* `warehouse/1` denotes warehouses
* `resource/1` denotes resources
* `product/1` denotes products
* `place/1` a place is a factory or warehouse
* `good/1` a good is a resource or a product
* `cost/3` driving cost between to places in time units; forms a metric
* `consumes/3` daily consumption rate of resources per factory
* `produces/3` daily production rate of product per factory
* `supply/3` weekly supply rate of resources per warehouse
* `demand/3` week demand rate of product per warehouse
* `vol/2` denotes the volume of some good

Exact implementation details are given in the generator program.
The program can be configured by the following constants:
* `#const days=5.`; the number of days a week has
* `#const tu_per_day=8.`; the number of time units a day has
* `#const factory_num=2.`; the number of factories
* `#const warehouse_num=3.`; the number of warehouses
* `#const resource_num=5.`; the number of resources
* `#const product_num=2.`; the number of products
* `#const min_cost=1.`; minimum drive time in time units
* `#const max_cost_frac=2.`; maximum fraction of a day that driving in one direction may take, i.e. `2` means that no route is longer than half a day.
Must be greater than or equal to `2` otherwise the actual solver might result in `UNSATISFIABLE`.
* `#const min_consumes=1.`; minimum consumption rate of a resource at a factory (rate can always be zero)
* `#const max_consumes=5.`; maximum consumption rate of a resource at a factory
* `#const min_produces=1.`; minimum production rate of a product at a factory (rate can always be zero)
* `#const max_produces=5.`; maximum production rate of a product at a factory
* `#const min_vol=1.`; minimum volume of a good
* `#const max_vol=3.`; maximum volume of a good

Supply an demand rates of warehouses can' be configured as they always match what is consumes/produced by the factories times the number of days of the week.

The `./gen.ps1` PowerShell script can be used to generate an instance.
It hands the models of the instance generator program to a python script that randomly selects one of the models returned by the generator at prints all its facts including the constants `days` and `tu_per_day` into an instance program file.

Example:
```ps
.\gen.ps1 -Days 5 `     # How many days does the week have?
    -TuPerDay 8 `       # How many tus does a day have?
    -SampleSize 100 `   # Number of models to generate
    -Options @() `      # Options to clingo
    -Files .\instance-generation\instance-generator.lp `
                        # Input files; at least the instance generator and optionally other constraints
    -OutFile .\instance.lp
                        # Files to write the instance to
```

## Solving

Solving the problem is straight-forward: simply use the `solver.lp` file and some problem instance and wait for the optimizer to do its job:
```
clingo instance.lp solve.lp
```
