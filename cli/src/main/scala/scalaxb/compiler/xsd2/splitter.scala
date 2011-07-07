package scalaxb.compiler.xsd2

import xmlschema._

trait Splitter { self: ContextProcessor with Lookup =>
  def splitIfLongSequence(tagged: Tagged[KeyedGroup])(implicit tag: HostTag): List[Tagged[KeyedGroup]] =
    splitLongSequence(tagged) getOrElse {List(tagged)}

  def splitLongSequence(tagged: Tagged[KeyedGroup])(implicit tag: HostTag): Option[List[Tagged[KeyedGroup]]] =
    splitParticles(tagged.particles)

  def splitIfLong(particles: List[Tagged[_]])(implicit tag: HostTag): List[Tagged[_]] =
    splitParticles(particles) getOrElse {particles}

  def splitParticles(particles: List[Tagged[_]])(implicit tag: HostTag): Option[List[TaggedKeyedGroup]] = {
    def buildSequence(ps: List[Tagged[_]]): TaggedKeyedGroup =
      TaggedKeyedGroup(KeyedGroup("sequence", XExplicitGroup(annotation = None,
             arg1 = ps map {Tagged.toParticleDataRecord},
             minOccurs = 1,
             maxOccurs = "1",
             attributes = Map())), tag)

    def longList(ps: List[Tagged[_]]): Option[List[Tagged[_]]] =
      if (ps.size >= config.contentsSizeLimit) Some(ps)
      else None

    def splitLong(ps: List[Tagged[_]]): List[TaggedKeyedGroup] =
      if (ps.size <= config.sequenceChunkSize) List(buildSequence(ps))
      else List(buildSequence(ps.take(config.sequenceChunkSize))) ::: splitLong(ps.drop(config.sequenceChunkSize))

    longList(particles) map { xs => splitLong(xs) }
  }
}
