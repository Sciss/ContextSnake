# ContextSnake

## statement

ContextSnake is library for the Scala programming language implementing a variable length markov chain. It is (C)opyright 2013&ndash;2014 by Hanns Holger Rutz. All rights reserved. ContextSnake is released under the [GNU Lesser General Public License](http://github.com/Sciss/ContextSnake/blob/master/LICENSE) v2.1+ and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

## requirements / installation

ContextSnake currently builds with sbt 0.13 against Scala 2.11 and 2.10.

## design

This project is unfinished. Currently we just have a fast suffix tree structure based on the algorithm of Esko Ukkonen and the C++ implementation by Mark Nelson, as described [on his blog](http://marknelson.us/1996/08/01/suffix-trees/). In the next step it will be extended to allow for probability based context extension and contraction, following the concept of Gerhard Nierhaus. It will then serve for experimentation with different parameters, snake motion strategies and multidimensional hierarchical linking.

The following short example illustrates the initial suffix tree (you can experiment with `sbt console`:

```scala

    val c = ContextTree("BANANA": _*)
    Seq[Seq[Char]]("BAN", "ANA", "FOO").map(c.containsSlice) // true, true, false
    c += 'S'
    c.containsSlice("ANANAS") // true
```

Begin of snake design (start in `sbt test:console`):

```scala

    :load notes/erlkoenig.txt
    val c = ContextTree(txt.toUpperCase: _*)
    Util.produce(c,200,4)("M").mkString
```

## todo

- eventually should be `@specialized`, but this will only make sense if scala-collections are specialized, since they are used as helper structures internally.
- should count frequencies to do a weighted choice among successors?
- should allow to cast elements into coarser bins
