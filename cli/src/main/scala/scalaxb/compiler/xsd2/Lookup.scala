package scalaxb.compiler.xsd2

import java.net.{URI}
import javax.xml.namespace.{QName}
import xmlschema._
import scalaxb._
import scalaxb.compiler.{ScalaNames, Logger, Config, Snippet, ReferenceNotFound}
import scalaxb.compiler.xsd.{XsAnyType, XsString, BuiltInSimpleTypeSymbol, XsTypeSymbol}
import Defs._
import scala.xml.{NamespaceBinding}

case class QualifiedName(namespace: Option[URI], localPart: String) {
  def toScalaCode(implicit targetNamespace: Option[URI], lookup: Lookup): String =
    if (namespace == targetNamespace || namespace.isEmpty || namespace == Some(XML_SCHEMA_URI)) localPart
    else lookup.packageName(namespace) + "." + localPart
}

object QualifiedName {
  def apply(namespace: URI, name: String): QualifiedName = QualifiedName(Some(namespace), name)

  implicit def apply(value: QName)(implicit targetNamespace: Option[URI], scope: NamespaceBinding) =
    splitTypeName(value.toString)

  def splitTypeName(name: String)(implicit targetNamespace: Option[URI], scope: NamespaceBinding): QualifiedName =
    if (name.contains('@')) QualifiedName(targetNamespace, name)
    else if (name.contains(':')) {
        val prefix = name.dropRight(name.length - name.indexOf(':'))
        val value = name.drop(name.indexOf(':') + 1)
        QualifiedName(Option[String](scope.getURI(prefix)) map {new URI(_)}, value)
      }
      else QualifiedName(Option[String](scope.getURI(null)) map {new URI(_)}, name)
}

trait Lookup extends ContextProcessor {
  implicit val lookup = this

  def schema: ReferenceSchema
  implicit def scope: NamespaceBinding = schema.scope
  implicit def targetNamespace = schema.targetNamespace

  case class Occurrence(minOccurs: Int, maxOccurs: Int, nillable: Boolean)
  object Occurrence {
    def apply(minOccurs: Int, maxOccurs: String, nillable: Boolean): Occurrence =
      Occurrence(minOccurs,
        if (maxOccurs == "unbounded") Int.MaxValue
        else maxOccurs.toInt,
        nillable)

    def apply(elem: XElement): Occurrence =
      Occurrence(elem.minOccurs.toInt, elem.maxOccurs, elem.nillable)

    def apply(any: XAny): Occurrence =
      Occurrence(any.minOccurs.toInt, any.maxOccurs, false)

    def apply(particle: DataRecord[XParticleOption]): Occurrence = particle match {
      case DataRecord(_, _, x: XElement)               => Occurrence(x)
      case DataRecord(_, _, x: XAny)                   => Occurrence(x)
      case DataRecord(_, Some("group"), x: XGroup)     => Occurrence(KeyedGroup("group", x))
      case DataRecord(_, Some("all"), x: XGroup)       => Occurrence(KeyedGroup("all", x))
      case DataRecord(_, Some("choice"), x: XGroup)    => Occurrence(KeyedGroup("choice", x))
      case DataRecord(_, Some("sequence"), x: XGroup)  => Occurrence(KeyedGroup("sequence", x))
    }

    def apply(keyed: KeyedGroup): Occurrence = keyed.key match {
      case GroupTag =>
        // TODO: fix this
        Occurrence(keyed.group.minOccurs.toInt, keyed.group.maxOccurs, false)
      case ChoiceTag =>
        val choice = keyed.group
        val o = Occurrence(choice.minOccurs.toInt, choice.maxOccurs, false)
        val particleOs = choice.arg1.toList map {Occurrence(_)}
        Occurrence((o.minOccurs :: (particleOs map { _.minOccurs})).min,
          (o.maxOccurs :: (particleOs map { _.maxOccurs})).max,
          particleOs exists {_.nillable})
      case _ =>
        Occurrence(keyed.group.minOccurs.toInt, keyed.group.maxOccurs, false)
    }

  }

  val SingleNotNillable = Occurrence(1, 1, false)
  val SingleNillable = Occurrence(1, 1, true)
  val OptionalNotNillable = Occurrence(0, 1, false)
  val OptionalNillable = Occurrence(0, 1, true)
  val UnboundedNotNillable = Occurrence(0, Int.MaxValue, false)
  val UnboundedNillable = Occurrence(0, Int.MaxValue, true)

  def buildTypeName(tagged: Tagged[Any]): QualifiedName = tagged match {
    case x: TaggedDataRecordSymbol =>
      val member = buildTypeName(x.value.member)
      QualifiedName(Some(SCALAXB_URI), "DataRecord[%s]".format(member.toScalaCode))

    case x: TaggedAny => QualifiedName(Some(SCALAXB_URI), "DataRecord[Any]")
    case x: TaggedSymbol =>
      x.value match {
        case XsAnyType                       => QualifiedName(Some(SCALAXB_URI), "DataRecord[Any]")
        case symbol: BuiltInSimpleTypeSymbol => QualifiedName(None, symbol.name)
      }
    case x: TaggedSimpleType => buildSimpleTypeTypeName(x)
    case x: TaggedComplexType =>
      QualifiedName(tagged.tag.namespace, names.get(x) getOrElse {"??"})
    case x: TaggedEnum =>
      QualifiedName(tagged.tag.namespace, names.get(x) getOrElse {"??"})
    case x: TaggedKeyedGroup =>
      x.key match {
        case ChoiceTag =>
          val particleOs = x.group.arg1.toList map {Occurrence(_)}
          if (particleOs exists {_.nillable}) {
            val member = QualifiedName(tagged.tag.namespace, names.get(x) getOrElse {"??"})
            QualifiedName(None, "Option[%s]".format(member.toScalaCode))
          }
          else QualifiedName(tagged.tag.namespace, names.get(x) getOrElse {"??"})

        case _ => QualifiedName(tagged.tag.namespace, names.get(x) getOrElse {"??"})
      }

//    case XsNillableAny  => "scalaxb.DataRecord[Option[Any]]"
//    case XsLongAll      => "Map[String, scalaxb.DataRecord[Any]]"
//    case XsLongAttribute => "Map[String, scalaxb.DataRecord[Any]]"
//    case XsAnyAttribute  => "Map[String, scalaxb.DataRecord[Any]]"
//    case XsDataRecord(ReferenceTypeSymbol(decl: ComplexTypeDecl)) if compositorWrapper.contains(decl) =>
//      compositorWrapper(decl) match {
//        case choice: ChoiceDecl => buildChoiceTypeName(decl, choice, shortLocal)
//        case _ => "scalaxb.DataRecord[Any]"
//      }
//    case r: XsDataRecord => "scalaxb.DataRecord[Any]"
//    case XsMixed         => "scalaxb.DataRecord[Any]"
//    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildTypeName(decl, shortLocal)
//    case ReferenceTypeSymbol(decl: ComplexTypeDecl) => buildTypeName(decl, shortLocal)
//    case symbol: AttributeGroupSymbol => buildTypeName(attributeGroups(symbol.namespace, symbol.name), shortLocal)
//    case XsXMLFormat(decl: ComplexTypeDecl) => "scalaxb.XMLFormat[" + buildTypeName(decl, shortLocal) + "]"
//    case XsXMLFormat(group: AttributeGroupDecl) => "scalaxb.XMLFormat[" + buildTypeName(group, shortLocal) + "]"
    case _ => error("buildTypeName # unsupported: " + tagged)
  }

  def buildSimpleTypeTypeName(decl: Tagged[XSimpleType]): QualifiedName = {
    decl.arg1.value match {
      case restriction: XRestriction if containsEnumeration(decl) =>
        // trace type hierarchy to the top most type that implements enumeration.
        val base = baseType(decl)
        QualifiedName(base.tag.namespace, names.get(base) getOrElse {"??"})
      case restriction: XRestriction =>
        buildTypeName(baseType(decl))
      case list: XList =>
        val base = baseType(decl)
        val baseName = base.value match {
          case symbol: BuiltInSimpleTypeSymbol => symbol.name
          case decl: XSimpleType               => names.get(base) getOrElse {"??"}
        }
        QualifiedName(None, "Seq[%s]".format(QualifiedName(base.tag.namespace, baseName).toScalaCode))
      // union baseType is hardcoded to xs:string.
      case union: XUnion =>
        buildTypeName(baseType(decl))
    }
  }

  def isRootEnumeration(tagged: Tagged[XSimpleType]): Boolean =
    if (!containsEnumeration(tagged)) false
    else tagged.value.arg1.value match {
      case XRestriction(_, _, _, Some(base), _) =>
        QualifiedName(base) match {
          case BuiltInType(tagged) => true
          case SimpleType(tagged)  => !containsEnumeration(tagged)
        }
      case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) =>
        !containsEnumeration(Tagged(simpleType, tagged.tag))
      case _ => false
    }

  def baseType(decl: Tagged[XSimpleType]): Tagged[Any] = decl.value.arg1.value match {
    case XRestriction(_, _, _, Some(base), _) if containsEnumeration(decl) =>
      QualifiedName(base) match {
        case BuiltInType(tagged) => decl
        case SimpleType(tagged)  =>
          if (containsEnumeration(tagged)) baseType(tagged)
          else decl
      }
    case XRestriction(_, _, _, Some(base), _) =>
      QualifiedName(base) match {
        case BuiltInType(tagged) => tagged
        case SimpleType(tagged)  => baseType(tagged)
      }
    case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) if containsEnumeration(decl) =>
      if (containsEnumeration(Tagged(simpleType, decl.tag))) baseType(Tagged(simpleType, decl.tag))
      else decl
    case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) =>
      baseType(Tagged(simpleType, decl.tag))
    case XList(_, _, _, Some(itemType), _) =>
      QualifiedName(itemType) match {
        case BuiltInType(tagged) => tagged
        case SimpleType(tagged)  => baseType(tagged)
      }
    case XList(_, Some(simpleType), _, _, _) =>
      baseType(Tagged(simpleType, decl.tag))
    case x: XUnion => Tagged(XsString, HostTag(Some(XML_SCHEMA_URI), SimpleTypeHost, "string"))
    case _ => error("baseType#: Unsupported content " +  decl.arg1.value.toString)
  }

  def resolveType(typeName: QualifiedName): Tagged[Any] = typeName match {
    case AnyType(tagged)     => tagged
    case BuiltInType(tagged) => tagged
    case SimpleType(tagged)  => tagged
    case ComplexType(tagged) => tagged
    case _ => throw new ReferenceNotFound("type", typeName.namespace map {_.toString}, typeName.localPart)
  }

  object AnyType {
    def unapply(typeName: QualifiedName): Option[Tagged[XsTypeSymbol]] = typeName match {
      case XS_ANY_TYPE => Some(Tagged(XsAnyType, HostTag(Some(XML_SCHEMA_URI), SimpleTypeHost, "anyType")))
      case _ => None
    }
  }

  object BuiltInType {
    def unapply(typeName: QualifiedName): Option[Tagged[XsTypeSymbol]] = typeName match {
      case QualifiedName(Some(XML_SCHEMA_URI), localPart) =>
        Some(Tagged(XsTypeSymbol.toTypeSymbol(localPart), HostTag(typeName.namespace, SimpleTypeHost, localPart)))
      case _ => None
    }
  }

  object SimpleType {
    def unapply(typeName: QualifiedName): Option[Tagged[XSimpleType]] = typeName match {
      case QualifiedName(targetNamespace, localPart) if schema.topTypes contains localPart =>
        schema.topTypes(localPart) match {
          case x: TaggedSimpleType => Some(x)
          case _ => None
        }
      case _ => None
    }
  }

  object ComplexType {
    def unapply(typeName: QualifiedName): Option[Tagged[XComplexType]] = typeName match {
      case QualifiedName(targetNamespace, localPart) if schema.topTypes contains localPart =>
        schema.topTypes(localPart) match {
          case x: TaggedComplexType => Some(x)
          case _ => None
        }
      case _ => None
    }
  }

  object Element {
    def unapply(qualifiedName: QualifiedName): Option[Tagged[XElement]] = qualifiedName match {
      case QualifiedName(targetNamespace, localPart) if schema.topElems contains localPart =>
        Some(schema.topElems(localPart))
      case _ => None
    }

  }

  def splitLongSequence(tagged: Tagged[KeyedGroup]): List[Tagged[KeyedGroup]] = List(tagged)

  def isForeignType(tagged: Tagged[_]): Boolean = tagged match {
    case x: TaggedElement    => x.value.ref map { QualifiedName(_).namespace != targetNamespace } getOrElse {false}
    case x: TaggedKeyedGroup if x.value.key == GroupTag =>
      x.value.group.ref map { QualifiedName(_).namespace != targetNamespace } getOrElse {false}
    case _ => false
  }

  def isOptionDescendant(tagged: Tagged[_]): Boolean = tagged match {
    case x: TaggedElement =>
      x.typeStructure match {
        case decl: TaggedComplexType => true
        case _ => false
      }
    case x: TaggedKeyedGroup if x.value.key == ChoiceTag =>
      implicit val tag = x.tag
      x.value.particles forall {isOptionDescendant}
    case x: TaggedKeyedGroup if x.value.key == SequenceTag => true
    case _ => false
  }

  def max(lhs: String, rhs: String): String =
    if (lhs == "unbounded" || rhs == "unbounded") "unbounded"
    else math.max(lhs.toInt, rhs.toInt).toString

  def packageName(namespace: Option[URI]): String =
    namespace map { ns =>
      if (ns == SCALAXB_URI) "scalaxb"
      else "packagename" } getOrElse { "packagename" }
}
