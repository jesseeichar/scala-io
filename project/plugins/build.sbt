
resolvers += {
  val mapfishRepoUrl = new java.net.URL("http://dev.mapfish.org/ivy2")
  Resolver.url("Mapfish Ivy Repository", mapfishRepoUrl)(Resolver.ivyStylePatterns)
}
