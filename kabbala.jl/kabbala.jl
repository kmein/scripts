using Graphs
using GraphIO

tree_of_life = loadgraph("tree-of-life.txt", GraphIO.EdgeList.EdgeListFormat())

print(adjacency_matrix(tree_of_life))
