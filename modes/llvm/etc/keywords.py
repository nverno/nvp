#!/usr/bin/env python

"""
Pull llvm instructions/attributes from LangRef.rst

Attributes from:
  
  Function Attributes
  -------------------
  ``alignstack(<n>)``
  ...

Constant Expressions
Atomic Memory Ordering Constraints
etc.

"""

import sys
import re

import docutils.utils
import docutils.frontend
from docutils import nodes
from docutils.utils import new_document
from docutils.parsers.rst import Parser

def parse_rst(text: str) -> docutils.nodes.document:
    parser = docutils.parsers.rst.Parser()
    components = (docutils.parsers.rst.Parser,)
    settings = docutils.frontend.OptionParser(components=components)\
                                .get_default_values()
    document = docutils.utils.new_document('<rst-doc>', settings=settings)
    parser.parse(text, document)
    return document


class InsVisitor(docutils.nodes.NodeVisitor):
    # XXX: use?
    def visit_title(self, node: docutils.nodes.title) -> None:
        print(len(node.children))
        pass
    
    def visit_section(self, node: docutils.nodes.section) -> None:
        print(node.attributes)

    def unknown_visit(self, node: docutils.nodes.Node) -> None:
        """Called for all other node types."""
        pass


def get_instruction(doc):
    """
    Instructions have: Syntax, Overview, Arguments, Semantics, Example sections
    
    """
    # FIXME:
    ref_node = doc.traverse(
        lambda n: isinstance(n, nodes.section) and n[0].astext() == "Instruction Reference"
    )
    insts = doc.traverse(nodes.section)\
        .traverse(nodes.section)
    
    for inst in insts:
        i = s.first_child_matching_class(nodes.title)
        if i != None and s[i].astext() == "Instruction Reference":
            return s
    return sects
    
def get_instructions(doc):
    """Extract instructions from LangRef.rst"""
    def is_instruction(node):
        return isinstance(node, nodes.title) and node.astext().find("Instruction") > 0
        
    insts = doc.traverse(is_instruction)

    res = []
    for ins in insts:
        m = re.search("'(.+)'", ins.astext())
        if m:
            res.append(m.group(1), ins.line)
    return res


def get_doc(rst):
    with open(rst, 'r') as f:
        text = f.read()
        doc = parse_rst(text)

    # visitor = InsVisitor(doc)
    # doc.walk(visitor)
    return doc


def main():
    import sys
    if len(sys.argv) > 1:
        rst = sys.argv[1]
    else:
        rst = "/usr/share/doc/llvm-8-doc/html/_sources/LangRef.rst.txt"
    doc = get_doc(rst)
    return get_instructions(doc)


if __name__ == '__main__':
    ins = main()
    print('\n'.join(ins))
