#include <quark/Support/IdentationFormat.h>

#include <llvm/Support/raw_ostream.h>

using namespace quark;

/// This functions is in charge in add the left-filling to the suffix.
static void FillIndentationPrefix(llvm::SmallVectorImpl<char> &result,
                                  llvm::StringRef ancestorWithSiblingStr,
                                  llvm::StringRef ancestorWithNoSiblingStr,
                                  llvm::ArrayRef<TreeIndentationInfo> info,
                                  size_t currPos) {
  llvm::SmallVector<llvm::StringRef, 8> stack;
  unsigned currDepth = info[currPos].Depth;

  // Traverse backwards to find our parents
  for (size_t i = currPos; i != 0 && currDepth != 0; --i) {
    if (info[i].Depth <= currDepth) {
      // Once we found a parent, choose the corrent prefix
      if (info[i].HasParentMoreSiblings) {
        stack.push_back(ancestorWithSiblingStr);
      } else {
        stack.push_back(ancestorWithNoSiblingStr);
      }
      currDepth = info[i].Depth - 1;
    }
  }

  // We need to reverse the prefixes due to the insertion order
  while (!stack.empty()) {
    result.append(stack.back().begin(), stack.back().end());
    stack.pop_back();
  }
}

void ElementTreeIndentationFormat::printFormat(llvm::raw_ostream &out,
                                               std::size_t ith) {
  if (TII.empty()) {
    return;
  }

  llvm::SmallString<32> indentationPrefix;

  if (TII[ith].Depth) {
    // If not in depth 0, add needed filling
    FillIndentationPrefix(indentationPrefix, getAncestorWithSiblingStr(),
                          getAncestorWithNoSiblingStr(), TII, ith);
  }

  indentationPrefix.append(TII[ith].IsLastSibling ? getLastChildStr()
                                                  : getNotLastChildStr());

  out << indentationPrefix;
}

/// Checks if in our current position and depth in the tree, we are the last
/// sibling of our parent
static bool AmILastSibling(llvm::ArrayRef<TreeIndentationInfo> depths,
                           size_t currPos) {
  unsigned currDepth = depths[currPos].Depth;

  for (size_t i = currPos + 1; i < depths.size(); i++) {
    if (depths[i].Depth < currDepth) {
      // We exited our parent or we ended the information of our current file
      break;
    }
    if (currDepth == depths[i].Depth) {
      // We found a sibling
      return false;
    }
  }

  return true;
}

/// Checks if in our current position and depth in the tree, our parent has more
/// siblings
static bool HasParentMoreSibligns(llvm::ArrayRef<TreeIndentationInfo> depths,
                                  size_t currPos) {
  unsigned currDepth = depths[currPos].Depth;
  if (currDepth == 0) {
    return false;
  }

  unsigned parentDepth = currDepth - 1;

  for (size_t i = currPos + 1; i < depths.size(); i++) {
    if (depths[i].Depth < parentDepth) {
      // We exited to the parent of our parent or we found the end of our
      // current file
      break;
    }
    if (parentDepth == depths[i].Depth) {
      // We found a sibling of our parent
      return true;
    }
  }
  return false;
}

void TreeIndentationInfo::FillRemainingIndentationData(
    std::vector<TreeIndentationInfo> &result) {
  for (size_t i = 0; i < result.size(); i++) {
    result[i].IsLastSibling = AmILastSibling(result, i);
    result[i].HasParentMoreSiblings = HasParentMoreSibligns(result, i);
  }
}
