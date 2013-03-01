
trait Graph[@specialized(Int) N] extends Traversable[(N,N)] {
    def nodes: Iterable[N]
    def adjacent: N => Traversable[N]

    def edges: Traversable[(N,N)]
    def reverse: Graph[N]
}

trait Weighted[@specialized(Int) N, @specialized(Double) V] {
    def weight: (N,N) => V
}

trait GraphBase[@specialized(Int) N] extends Graph[N] {
    self =>

    override def edges: Traversable[(N,N)] = self
    override def foreach[U](f: ((N, N)) => U) {
        for(from <- nodes; to <- adjacent(from)) f((from,to))
    }
    override def reverse:Graph[N] = new GraphBase[N] {
        override val nodes: Iterable[N] = self.nodes
        override val adjacent: N => Traversable[N] = node => new Traversable[N]{
            def foreach[U](f: (N) => U) {
               for (n <- self.nodes if Graph.contains(self.adjacent(n),node)) f(n)
            }
        }
        override val edges: Traversable[(N,N)] =  new Traversable[(N,N)] {
            def foreach[U](f: ((N,N)) => U) {
                for(from <- self.nodes; to <- self.adjacent(from)) f((to,from))
            }
        }
        override val reverse = self
    }
}

object Graph {
    def apply[@specialized(Int) N](mappings:(N,Iterable[N])*): Graph[N] = new GraphBase[N] {
        val map = mappings.toMap
        def nodes: Iterable[N] = map.keys
        def adjacent: N => Iterable[N] = map
    }

    def contains[@specialized(Int) N](col:Traversable[N],node:N):Boolean = {
        col match {
            case set:Set[N] => set.contains(node)
            case _ => col exists (n => n==node)
        }
    }
}

