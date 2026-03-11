abstract sig Node {
  flow : Node -> lone Int,
  stored : one Int
} {
  stored >= 0
  all n : Node | some flow[n] implies flow[n] >= 0
  no flow[this]
}

fun currentFlow(x, y : one Node) : Int {
  let s = x.stored, f = x.flow[y] | s < f implies s else f
}

pred withFlow[x, y : one Node] {
  currentFlow[x, y] > 0
}

pred show {}

run withFlow for 3 Int, 2 Node
