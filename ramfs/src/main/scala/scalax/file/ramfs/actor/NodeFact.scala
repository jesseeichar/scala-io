package scalax.file.ramfs.actor

private[ramfs] trait NodeFac {
  def create(name:String):Node
  def accepts(node:Node):Boolean
}

private[ramfs] object FileNodeFac extends NodeFac {
  def create(name:String) = new FileNode(name)
  def accepts(node:Node) = node.isInstanceOf[FileNode]
}


private[ramfs] object DirNodeFac extends NodeFac {
  def create(name:String) = new DirNode(name)
  def accepts(node:Node) = node.isInstanceOf[DirNode]
}
