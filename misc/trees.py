def infix(tree):
    if type(tree) == int:
        return str(tree)
    elif len(tree) == 3:
        t1, op, t2 = tree
        return "({} {} {})".format(infix(t1), op, infix(t2))
    else:
        raise ValueError("Malformed expression tree.")


def prefix(tree):
    if type(tree) == int:
        return str(tree)
    elif len(tree) == 3:
        t1, op, t2 = tree
        return "({} {} {})".format(op, prefix(t1), prefix(t2))
    else:
        raise ValueError("Malformed expression tree.")


def postfix(tree):
    if type(tree) == int:
        return str(tree)
    elif len(tree) == 3:
        t1, op, t2 = tree
        return "({} {} {})".format(postfix(t1), postfix(t2), op)
    else:
        raise ValueError("Malformed expression tree.")


example = ((2, "+", 3), "+", 4)

print(infix(example))
print(prefix(example))
print(postfix(example))
