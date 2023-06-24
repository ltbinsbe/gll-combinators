# gll-combinators

This is the GitHub repository of the Hackage package [gll](https://hackage.haskell.org/package/gll) and is superseded by the [fungll-combinators repository](https://github.com/ltbinsbe/fungll-combinators).

The following acadamic papers describe the workings of these packages:  

* gll-combinators: [GLL parsing with flexible combinators](https://doi.org/10.1145/3276604.3276618)
* fungll-combinators: [Purely functional GLL parsing](https://doi.org/10.1016/j.cola.2020.100945)
* Pre-prints are available [here](https://ltvanbinsbergen.nl/publications)

The package gll provides generalised top-down parsing according to the (R)GLL parsing algorithm [Scott and Johnstone 2016].

The user can either invoke the GLL parser directly by importing GLL.Parser and providing a value of the Grammar datatype in (exported by GLL.Parser). Alternatively, the user can import GLL.Combinators to write combinator expressions from which a grammar of the required form is extracted. The combinators enable applying arbitrary semantic actions to parse results. The documentation of the respective packages provides more information.

The main motivation for this package is the development of Domain Specific Languages (DSLs). More specifically: designing DSLs with minimal differences between between abstract and concrete syntax (abstract syntax is often ambiguous).
