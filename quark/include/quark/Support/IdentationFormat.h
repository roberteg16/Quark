#ifndef __QUARK_FRONTEND_IDENTATIONFORMAT_H__
#define __QUARK_FRONTEND_IDENTATIONFORMAT_H__

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/StringRef.h>

#include <string>
#include <vector>

namespace quark {

struct IdentationElement {
  IdentationElement(std::string str, std::size_t depth)
      : Str(std::move(str)), Depth(depth) {}
  std::string Str;
  std::size_t Depth;
};

///
/// Struct that holds all the information needed to build tree-like indentations
///
struct TreeIndentationInfo {
  TreeIndentationInfo(unsigned depth) : Depth(depth) {}

  static std::vector<TreeIndentationInfo>
  Create(llvm::ArrayRef<IdentationElement> loops) {
    std::vector<TreeIndentationInfo> result;

    for (size_t i = 0; i < loops.size(); i++) {
      result.emplace_back(loops[i].Depth);
    }

    FillRemainingIndentationData(result);
    return result;
  }

  /// Depth of the loop
  unsigned Depth = 0;
  /// Tells whether we are the last child of our parent
  bool IsLastSibling = false;
  /// Tells if our parent has siblings after us in the report. Precached to
  /// avoid lot of potential iterations
  bool HasParentMoreSiblings = false;

private:
  /// Fill the remaining indentation data that the public functions "Create"
  /// created beforehand
  static void
  FillRemainingIndentationData(std::vector<TreeIndentationInfo> &depths);
};

/// Specifies the indentation format that is going to be used
class TreeIndentationFormat {
public:
  TreeIndentationFormat() {}

  TreeIndentationFormat(llvm::ArrayRef<IdentationElement> nodes)
      : TII(TreeIndentationInfo::Create(nodes)) {}

  virtual ~TreeIndentationFormat() {}

  /// Recovers the text for the indentation
  llvm::SmallString<32> findIndentationPrefix(unsigned row) const;

  std::vector<TreeIndentationInfo> TII;

protected:
  /// Str for those children that are the last of their siblings in the report
  virtual llvm::StringRef getLastChildStr() const { return "`-"; }
  /// Str for those children that are not the last of their siblings in the
  /// report
  virtual llvm::StringRef getNotLastChildStr() const { return "|-"; }
  /// Left-filling prefix Str for when we are inside a parent that still has
  /// more siblings to be printed in the report
  virtual llvm::StringRef getAncestorWithSiblingStr() const { return "| "; }
  /// Left-filling prefix Str for when we are inside a parent that does not have
  /// more siblings to be printed in the report
  virtual llvm::StringRef getAncestorWithNoSiblingStr() const { return "  "; }
};

class ElementTreeIndentationFormat : public TreeIndentationFormat {
public:
  ElementTreeIndentationFormat() {}

  ElementTreeIndentationFormat(llvm::ArrayRef<IdentationElement> nodes)
      : TreeIndentationFormat(nodes) {}

  virtual ~ElementTreeIndentationFormat() {}

  void printFormat(llvm::raw_ostream &out, std::size_t ith);
};

} // namespace quark

#endif //__QUARK_FRONTEND_IDENTATIONFORMAT_H__
