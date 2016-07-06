# -*- coding: utf-8 -*-
"""

"""

from __future__ import print_function, division, absolute_import
from pratt_parser import PrattParser

#
# AST stuff
#

# This AST stuff should all be independent of the rest of the module, so it
# can later be moved out.  Different applications will define it differently.
# For implementing the AST nodes, use a shallow inheritance tree:
# a base node with basic, default operations and specialized nodes for the
# different possible types of AST nodes (maybe even an extra level for
# functions???).

class AST_Node(object):
    """The base class for nodes in the abstract syntax tree."""
    def __init__(self):
        self.children_types = []
        self.children = []
        self.parent = None
    def append_children(self, *token_nodes):
        """Append all the arguments as children, also setting their parent to self."""
        for t in token_nodes:
            self.children.append(t)
            t.parent = self
    def old_repr(self):
        """This is for backward compatibility in some test cases."""
        if self.token_label == "k_number":
            return "[literal {0}]".format(self.value)
        if self.token_label == "k_lpar":
            return "[k_lpar {0} k_rpar]".format(self.children[0])
        else:
            str_val = "[" + str(self.value)
            for a in self.children: str_val += " " + str(a)
            str_val += "]"
            return str_val
    def value_repr(self):
        return str(self.value)
    def label_repr(self):
        return str(self.token_label)
    def summary_repr(self):
        return "<" + str(self.token_label) + "," + str(self.value) + ">"
    def tree_repr(self, indent=""):
        string = indent + self.summary_repr() + "\n"
        for c in self.children:
            string += c.tree_repr(indent=indent+" "*4)
        return string
    def string_repr(self):
        string = self.value_repr()
        if self.children:
            string += "["
            string += ", ".join(c.string_repr() for c in self.children)
            string += "]"
        return string
    __repr__ = old_repr

# TODO will AST nodes really need to be subclasses defined in a table, or
# can the AST_node constructor (or some predefined method) just take a
# TokenNode as an argument and spit out the corresponding AST node?
# The init function itself could make subclasses of itself, store them
# in a dict if desired, and return an instance -- but that seems weird.

class AST_NodeDict(object):
    """The purpose of this class is to save the subclasses associated with
    various AST labels.  These are subclasses of the `AST_Node` class.  A
    token with an `ast_label` attribute can be passed to the `get_AST_node`
    method to return the subclass for the attribute.

    Generally, these AST nodes are used to replace the `TokenNode` classes
    which are initially generated in the parse tree.  The user may want some
    other format.  The conversion may be trivial or more complex.
    
    Currently implemented as a dict of generated classes for tokens, indexed by
    the token label."""
    def __init__(self):
        """Initialize the symbol table."""
        self.ast_node_dict = {} 

    def get_AST_subclass(self, ast_label):
        """Return the AST node subtype representing `ast_label`, defining it if 
        necessary."""
        if ast_label in self.ast_node_dict:
            AST_Subclass = self.ast_node_dict[ast_label]
        else: # AST subclass has not been created.
            class AST_Subclass(AST_Node):
                def __init__(self, string_form=None):
                    """Argument string_form, if not `None`, should be a list of
                    n+1 strings, where n is the number of children.  These strings
                    will be printed before, between, and after the children nodes
                    when the string form is produced."""
                    super(AST_Subclass, self).__init__() # Call base class __init__.
                    self.string_form = string_form
            AST_Subclass.__name__ = ast_label
            AST_Subclass.label = ast_label
            self.ast_node_dict[ast_label] = AST_Subclass
        return AST_Subclass

    def get_AST_node(self, token):
        class_type = self.get_AST_subclass(token.ast_label)
        instance = class_type()
        instance.value = token.value
        instance.token_label = token.token_label
        return instance

#
# Run tests below when invoked as a script.
#

if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("../../test/test_AST.py", pytest_args="-v")

