// Defines regular expression parse trees.

package edu.ucsb.cs.cs162.regex.parse_tree

sealed abstract class ParseTree {
  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Returns the character string corresponding to the leaves of this parse tree.
  def flatten: String = this match {
    case `EmptyLeaf` => ""
    case CharLeaf(char) => char.toString
    case ConcatNode(left, right) => left.flatten ++ right.flatten
    case LeftNode(child) => child.flatten
    case RightNode(child) => child.flatten
    case StarNode(children) => children.map(_.flatten).mkString("").reverse
    case CaptureNode(_, child) => child.flatten
  }

  override def toString = this match {
    case `EmptyLeaf` => "ε"
    case CharLeaf(char) => char.toString
    case ConcatNode(left, right) => s"(${left.toString} ~ ${right.toString})"
    case LeftNode(child) => s"L(${child.toString})"
    case RightNode(child) => s"R(${child.toString})"
    case StarNode(children) => children.map(_.toString).mkString("[", ", ", "]")
    case CaptureNode(name, child) => s"<${name} = ${child.toString}>"
  }

  def prettyPrint = asciiTree(this, prefix = "")

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Translate the parse tree into an ASCII tree structure.
  private def asciiTree(tree: ParseTree, prefix: String): String = {
    // The pieces that the ASCII tree structure are composed of.
    val (tee, bar, cap, end) = ("├─ ", "│  ", "└─ ", "   ")

    tree match {
      case `EmptyLeaf` => "ε\n"
      case CharLeaf(char) => char.toString + "\n"
      case ConcatNode(left, right) => {
        "ConcatNode\n" + prefix + tee + asciiTree(left, prefix + bar) +
        prefix + cap + asciiTree(right, prefix + end)
      }
      case LeftNode(child) => {
        "LeftNode\n" + prefix + cap + asciiTree(child, prefix + end)
      }
      case RightNode(child) => {
        "RightNode\n" + prefix + cap + asciiTree(child, prefix + end)
      }
      case StarNode(children) => {
        if (children.isEmpty) "StarNode\n" + prefix + cap + "ε\n"
        else {
          val init = children.init.map(prefix + tee + asciiTree(_, prefix + bar))
          val last = prefix + cap + asciiTree(children.last, prefix + end)

          "StarNode\n" + (
            if (children.size == 1) last
            else init.mkString("") + last
          )
        }
      }
      case CaptureNode(name, child) => {
        s"CaptureNode ($name)\n" + prefix + cap + asciiTree(child, prefix + end)
      }
    }
  }
}

// EmptyLeft corresponds to ε.
// CharLeaf corresponds to character 'char'.
// ConcatNode corresponds to Concatenate('right', 'left').
// LeftNode corresponds to the left side of a Union.
// RightNode corresponds to the right side of a Union.
// StarNode corresponds to a KleeneStar.
// CaptureNode corresponds to a Capture.
//
// Note that for efficiency reasons the Seq in StarNode is in reverse order; to
// get the correct order for the input string being matched it needs to be
// reversed.
case object EmptyLeaf extends ParseTree
case class CharLeaf(char: Char) extends ParseTree
case class ConcatNode(left: ParseTree, right: ParseTree) extends ParseTree
case class LeftNode(child: ParseTree) extends ParseTree
case class RightNode(child: ParseTree) extends ParseTree
case class StarNode(children: Seq[ParseTree]) extends ParseTree
case class CaptureNode(name: String, child: ParseTree) extends ParseTree

// Provides a way to conveniently access capture groups inside a parse tree.
class Extractor(tree: ParseTree) {
  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Given an access path (i.e., a sequence of nested capture group names),
  // return the associated flattened capture group(s) as a sequence of strings.
  // The sequence may be empty of the given group was not matched during the
  // parse, or may contain multiple items if the given group(s) was matched more
  // than once during the parse.
  def extract(accessPath: String*): Seq[String] = {
    // Traverse the nested group maps to find the group specified by the given
    // access path.
    def lookUp(path: Seq[String], groupMap: GroupMap): Group = {
      val name = path.head
      assert(groupMap.contains(name))

      if (path.tail.isEmpty) groupMap(name)
      else lookUp(path.tail, groupMap(name).children)
    }

    assert(accessPath.nonEmpty)
    lookUp(accessPath, groups).value
  }

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  private type GroupMap = Map[String, Group]

  // Provides the value of a capture group as a sequence of strings that were
  // matched by that capture group during parsing, plus a map of nested capture
  // group names to their own group info.
  private case class Group(value: Seq[String], children: GroupMap)

  // Extracts the capture group mapping from the given parse tree.
  private def extractGroups(tree: ParseTree): GroupMap = {
    // Merge two GroupMaps so that if they contain the same key, the two groups
    // for that common key are merged into a single group.
    def mergeGroupMaps(a: GroupMap, b: GroupMap): GroupMap =
      (a.keySet ++ b.keySet).map(key => key -> {
        val leftGroup = a.getOrElse(key, Group(Seq(), Map()))
        val rightGroup = b.getOrElse(key, Group(Seq(), Map()))
        Group(
          leftGroup.value ++ rightGroup.value,
          mergeGroupMaps(leftGroup.children, rightGroup.children))
      }).toMap

    tree match {
      case `EmptyLeaf` | _: CharLeaf => Map()
      case ConcatNode(left, right) => {
        mergeGroupMaps(extractGroups(left), extractGroups(right))
      }
      case LeftNode(child) => extractGroups(child)
      case RightNode(child) => extractGroups(child)
      case StarNode(children) => children.foldLeft(Map[String, Group]())(
        (acc, child) => mergeGroupMaps(acc, extractGroups(child))
      )
      case CaptureNode(name, child) => {
        Map(name -> Group(Seq(child.flatten), extractGroups(child)))
      }
    }
  }

  // The result of extracting capture group info from 'tree'.
  private val groups = extractGroups(tree)
}
