from pyswip import Prolog
from queue import deque


classes ={}

class Node:
    def __init__(self, data):
        self.data = data
        self.children = []

def build(parent, nodes, classes):

    for node in nodes:
        name = node.data
        base = parent
        methods = {
            "__init__": lambda self, name: setattr(self, "name", name) 
        }
        base_class = classes[base] if base else object

        # print(f"Base Class: {parent}, Child Class: {name}")

        classes[name] = type(name, (base_class,), methods)
        build(node.data, node.children, classes)


def build_tree(prolog, class_name):
    node = Node(class_name)
    children = deque(prolog.query(f"inherit(X, {class_name})"))
    for child in children:
        child_name = child["X"]
        node.children.append(build_tree(prolog, child_name))
    return node

def print_tree(node, prefix=""):
    print(prefix + str(node.data))
    for child in node.children:
        print_tree(child, prefix + "|-- ")


def create_instance(class_name, instance_name):
    cls = classes[class_name]
    instance = cls(instance_name)
    return instance

prolog = Prolog()
prolog.consult("logic.pl")
node = build_tree(prolog, "root")

print("Class hierarchy: ")
print_tree(node)

build(None, node.children, classes)
# print(type(classes))
for i in classes:
    print(deque(prolog.query(f"{i}(X)")))

# bob = create_instance("person", "Bob")
# print(bob.name)
# print(type(bob))
# # print(classes)


