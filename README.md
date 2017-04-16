# factorio-hs

Very early and experimental library for working with Factorio data. Assume it
doesn't work. The solver is intended to be a port of
[Foreman](https://bitbucket.org/Nicksaurus/foreman), but is a long way from
being so.

All command line tools will require an existing Factorio installation:

    export FACTORIO_PATH=/opt/factorio

`factorio-recipes` is a debug tool that prints all loaded recipes. It can
be a quick way to find recipe names for constructing graphs.

    $ factorio-recipes $FACTORIO_PATH | grep rocket
    Recipe {name = "rocket-part", items = [Ingredient "low-density-structure" (-10.0),Ingredient "rocket-fuel" (-10.0),Ingredient "rocket-control-unit" (-10.0),Ingredient "rocket-part" 1.0], category = "rocket-building"}

`factorio-solve` takes a graphviz representation of an assembly line and
annotates it with perfect ratio data:

    $ cat example.dot
    digraph {
      "copper-cable"
      "copper-plate"
      "iron-plate"
      "electronic-circuit" [factorioTarget = 10]

      "iron-plate" -> "electronic-circuit";
      "copper-plate" -> "copper-cable" -> "electronic-circuit";
    }
    $ factorio-solve $FACTORIO_PATH example.dot
    digraph {
        "copper-cable" [factorioRate=15.0];
        "copper-plate" [factorioRate=15.0];
        "iron-plate" [factorioRate=10.0];
        "electronic-circuit" [factorioRate=10.0
                             ,factorioTarget=10];
        "iron-plate" -> "electronic-circuit";
        "copper-plate" -> "copper-cable";
        "copper-cable" -> "electronic-circuit";
    }

This tells us that to run the `electronic-circuit` recipe 10 times, we need to
run the `copper-cable` recipe 15 times (which actually generates 30 cable).

## Installation

None provided yet. Please follow the development instructions.

## Development

Some one time setup is needed to install [GLPK](https://www.gnu.org/software/glpk/) and [Stack](https://docs.haskellstack.org). For Ubuntu something like:

    sudo apt-get install libglpk-dev haskell-stack
    stack upgrade

You can then use standard `stack` commands to run the programs:

    stack build && stack exec factorio-solve $FACTORIO_PATH example.dot

## Wishlist

* Arbitrary supply/consume/passthrough nodes.
* Show assembler/module needs.
* Calculate power usage.
* Auto graph generation (so you don't need to map a rocket build out by hand...)
* Better frontend than graphviz.
* Test coverage.
